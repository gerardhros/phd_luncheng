# make db for upscaling MA models
# this script describes the data merging needed for the data coupling with meta-analytical models
# the different data sources are prepared in ppr_datasets_for_upscaling.R
# main purpose is to identify specific "potential areas" where specific measures can be applied to increase SOC
# the actual expected change ins SOC is estimated later.

# merge country-climate polygon with crop grid cells (cc.spam)
cc.new <- as.data.table(cc)[,mget(c('name0','asap0_id','km2_crop','GRIDCODE'))][,ccID := .I]

# change gridcode to climate code
dt.climcode <- data.table(GRIDCODE=c(11,12,13,14,21,22,26,27,31,32,33,34,35,36,37,
                                     38,39,41,42,43,44,45,46,47,48,49,50,51,52,61,62),
                          climBeck =c('Af','Am','As','Aw','BWk','BWh','BSk','BSh','Cfa',
                                      'Cfb','Cfc','Csa','Csb','Csc','Cwa','Cwb','Cwc',
                                      'Dfa','Dfb','Dfc','Dfd','Dsa','Dsb','Dsc','Dsd',
                                      'Dwa','Dwb','Dwc','Dwd','EF','ET'),
                          climBeckGroup = c(rep('trop',4),rep('oth',4),'stme','temp',
                                            'temp',rep('stme',6),
                                            rep('temp',4),rep('stme',4),rep('oth',6)))

# step 1. merge country, climate, crops and fertilizer data

  # replace gridcode with climate
  cc.new <- merge(cc.new,dt.climcode,by='GRIDCODE')
  cc.new[,GRIDCODE:=NULL]

  # merge country-climate shape with all 300x300m plots from SPAM
  cc.new <- merge(cc.new,cc.spam,by.x = 'ccID', by.y='ID')

  # merge country-climate shape with harvested crop areas SPAM
  cc.new <- merge(cc.new,cr.area.all,by='CELL5M',all.x = TRUE)

  # remove gridcells that do not have harvest crop data
  cc.new <- cc.new[!(is.na(crop)|is.na(ALL)|ALL==0)]

  # sum all areas per ccID
  cc.sel <- c('ccID','name0','asap0_id','km2_crop','climBeck','climBeckGroup','crop')
  cc.new <- cc.new[,lapply(.SD,sum,na.rm=T),.SDcols = c('ALL','TECH_I','TECH_H','TECH_L'),by = cc.sel]

  # merge country-climate shape with nitrogen input cropland and potential N to be avoided
  cc.new <- merge(cc.new,cc.man[,.(ccID,ndm_cropland,ndm_loss,nfert)],by='ccID',all.x=TRUE)

  # convert ndm and fert to kg N / ha
  # original units is in tonnes and area in ha
  cc.new[,agr_area_ha := sum(ALL),by=.(ccID)]
  cc.new[,ndm_kgha := fifelse(ALL < 5, 0, ndm_cropland * 1000 / sum(ALL[ALL>=5])),by=.(ccID)]
  cc.new[,nif_kgha := fifelse(ALL < 1, 0, nfert * 1000 / sum(ALL[ALL>=1])),by=.(ccID)]
  cc.new[,nloss_kgha := fifelse(ALL < 5, 0, ndm_loss * 1000 / sum(ALL[ALL>=5])),by=.(ccID)]

# step 2. merge country-climate shape with tillage data

  # join crop areas with tillage fractions
  cc.new2 <- merge(cc.new,cc.till,by.x=c('ccID','crop'),by.y=c('ccID','spam'))

  # join tillage categories into three categories (HIGH, MEDIUM and LOW INTENSITY)
  # tillage categories
  # 1 = Conventional annual tillage
  # 2 = Traditional annual tillage
  # 3 = Reduced tillage
  # 4 = Conservation Agriculture
  # 5 = Rotational tillage
  # 6 = Traditional rotational tillage
  # 7 = Scenario Conservation Agriculture area
  cc.new2[,TILL_H := till1 + till5]
  cc.new2[,TILL_M := till2 + till6 + till3]
  cc.new2[,TILL_L := till4]
  cc.new2[,c('till1','till2','till3','till4','till5','till6') := NULL]

  # melt file to estimate area per tillage practice
  cc.new2 <- melt(cc.new2,id.vars = c('ccID','crop','name0','asap0_id','km2_crop','climBeck',
                                      'climBeckGroup','ALL','TECH_I','TECH_H','TECH_L',
                                      'ndm_loss','nloss_kgha','ndm_kgha','nif_kgha'),
                  measure.vars = c('TILL_H','TILL_M','TILL_L'),
                  variable.name = 'tillage',value.name = 'till.fr')

  # estimate areas per tillage practice
  cc.new2[,area_till := ALL * till.fr]
  cc.new2[,till.fr := NULL]

  # filter only on area's with more than 0 ha
  cc.new2 <- cc.new2[area_till > 0]

  # add fraction area that is characterised as high input agricultural system
  cc.new2[,TECH_INT := round((TECH_H + TECH_I) / ALL,3)]

  # remove columns not needed anymore
  cc.new2[,c('ALL','TECH_I','TECH_H','TECH_L') := NULL]


# step 3. prepare input data country based from latest FAO data ---

  # read and summarize FAO data per crop per country (area in ha, yield in tonnes)

  # select the data columns to be used
  cr.cols <- c('Area Code','Area','Item Code','Item','Element','Unit','Y2018')

  # read in the csv file (source: FAOSTAT, http://www.fao.org/faostat/en/#data/QC/visualize)
  cr2 <- fread('data/production_Crops_E_All_Data_2018.csv')

  # dcast the file, and get area and production per country and crop
  cr2 <- dcast(cr2,country+country_code + crop_code+crop_name ~ Element, value.var = 'Y2018')
  setnames(cr2,c('Area harvested','Production'),c('area_harvest','production'))

  # filter on individual crops (remove aggregates / groups from crops and countries)
  cr2 <- cr2[crop_code < 1000 & country_code < 300 & !is.na(area_harvest)]

  # add total area per country (ha)
  cr2[,fao_area_tot := sum(area_harvest),by ='country']

  # what crops are classified as cereals?
  crop.cereals <- crops_id[GROUP=='cereals',FAOCODE]

  # estimate cereal area fraction per country
  cr2 <- cr2[,fao_cereal_fa := sum(area_harvest[crop_code %in% crop.cereals])/sum(area_harvest),by = 'country']

  # change names
  setnames(cr2,c('area_harvest','production'),c('fao_cr_ha','fao_cr_tons'))

  # add crop yield (in ton / ha)
  cr2[,fao_cr_yield := round(fao_cr_tons / fao_cr_ha,3)]

# read and summarize FAO data crop residues per crop per country (kg N per crop per year)

  # read the csv file with crop residue per country
  cr1 <- fread('data/emissions_agriculture_crop_residues_e_all_data_NOFLAG_2017.csv')

  # add crop residue N (kg N) to the FAO db
  cr2 <- merge(cr2,cr1[,.(country_code,crop_code,fao_cr_resN)],by = c('country_code','crop_code'),all.x=TRUE)

# read and summarize FAO data crop residues burned per crop per country (kg DM per crop per year)

  # read the csv file with burned crop residue per country
  cr1 <- fread('data/emissions_agriculture_burning_crop_residues_e_all_data_NOFLAG_2017.csv')

  # add tonnes burned DM to the FAO db
  cr2 <- merge(cr2,cr1[,.(country_code,crop_code,fao_cr_burnedDM)],by = c('country_code','crop_code'),all.x=TRUE)

  # change units from residue from kg N and ton DM burned residue into kg N /ha and in kg DM/ha
  cr2[,fao_crres_kgnha := fao_cr_resN / fao_cr_ha]
  cr2[,fao_crres_burn_kgdmha := fao_cr_burnedDM *1000 / fao_cr_ha]

  # calculate C fraction burned from N fraction (kg N per kg DM, IPCC2006, table 11.2)
  cr2[crop_code %in% c(15,56), fr_burn := fao_crres_burn_kgdmha /(fao_crres_kgnha / 0.006)]
  cr2[crop_code == 27, fr_burn := fao_crres_burn_kgdmha /(fao_crres_kgnha / 0.007)]
  cr2[crop_code == 156, fr_burn := fao_crres_burn_kgdmha /(fao_crres_kgnha / 0.0045)]
  cr2[fr_burn > 1, fr_burn := 1]
  cr2[is.na(fr_burn), fr_burn := 0]

  # remove columns not needed any more
  cr2[,c('fao_cr_resN','fao_cr_burnedDM','fao_cr_tons','fao_crres_burn_kgdmha') := NULL]

# step 4. add input data country based from latest FAO data to climate-country shape ---

  # add FAO data to country - climate shape

  # calculate relative crop fraction given ccID (only for Cofee and millet because SPAM is more detailed than FAO)
  cr.dup.crop = cc.new2[crop %in% c('PMIL','SMIL','ACOF','RCOF'),.(ccID,crop,area_till)][,.(area_tot = sum(area_till)),by=.(ccID,crop)]
  cr.dup.crop = dcast(cr.dup.crop,ccID~crop,value.var = 'area_tot',fill = 0)
  cr.dup.crop[,fr_COF := fifelse(ACOF==0,0,fifelse(RCOF==0,1,round(ACOF / (RCOF+ACOF),3)))]
  cr.dup.crop[,fr_MIL := fifelse(PMIL==0,0,fifelse(SMIL==0,1,round(PMIL / (SMIL+PMIL),3)))]
  cr.dup.crop[,c('PMIL','SMIL','ACOF','RCOF') := NULL]

  # calculate relative country fraction when more than 1 FAO country per GAUL name
  cr.sel = unique(country_id[duplicated(fao_country_code) &!is.na(fao_country_code),fao_country_code])
  cr.sel = country_id[fao_country_code %in% cr.sel]
  cr.dup.gaul <- unique(cc.new2[asap0_id %in% cr.sel$gaul_asap0_id][,area_tot := sum(area_till),by='asap0_id'][,.(name0,asap0_id,area_tot)])
  cr.dup.gaul <- merge(cr.dup.gaul,cr.sel,by.x=c('asap0_id','name0'),by.y=c('gaul_asap0_id','gaul_name0'))
  cr.dup.gaul[,fao_cf_area := round(area_tot / sum(area_tot),3), by = 'fao_country_code']

  # calculate relative country fraction when more than 1 GAUL polygon per country
  cr.sel = unique(country_id[duplicated(gaul_asap0_id) &!is.na(gaul_asap0_id),gaul_asap0_id])
  cr.sel = country_id[gaul_asap0_id %in% cr.sel]
  cr.dup.fao <- unique(cr2[country_code %in% cr.sel$fao_country_code,.(country_code,country,fao_area_tot)])
  cr.dup.fao <- merge(cr.dup.fao,cr.sel,by.x=c('country_code','country'),by.y=c('fao_country_code','fao_country'))
  cr.dup.fao[,gaul_cf_area := round(fao_area_tot / sum(fao_area_tot),3), by = 'gaul_asap0_id']

  # add SPAM crop names to FAO db (only crops: millet and coffee are duplicated)
  cr3 <- merge(cr2,crops_id[,.(SPAM_short_name,FAONAMES,FAOCODE,GROUP)],by.x = 'crop_code',by.y = 'FAOCODE',all.x = TRUE)

  # subset country db where multiple FAO countries belong to one shape
  cr3a <- cr3[country_code %in% cr.dup.fao$country_code & !country_code %in% cr.dup.gaul$fao_country_code]
  cr3a <- merge(cr3a,cr.dup.fao,by = c('country_code','country','fao_area_tot'),all.x = TRUE)
  cr3a.sel <- c('gaul_asap0_id','gaul_name0','SPAM_short_name')
  cr3a[,afrac := fao_cr_ha / sum(fao_cr_ha),by = cr3a.sel]
  cr3a = cr3a[,.(fao_cr_ha = sum(fao_cr_ha,na.rm=T),
                 fao_cereal_fa = sum(fao_cereal_fa * afrac,na.rm=T),
                 fao_cr_yield = sum(fao_cr_yield * afrac,na.rm=T),
                 fao_crres_kgnha = sum(fao_crres_kgnha * afrac,na.rm=T),
                 fr_burn = sum(fr_burn * afrac,na.rm=T)),by = cr3a.sel]
  cr3a <- unique(cr3a)
  setnames(cr3a,gsub('^gaul_','',colnames(cr3a)))

  # subset country db where one FAO country belongs to multiple shape
  cr3b <- cr3[country_code %in% cr.dup.gaul$fao_country_code & !country_code %in% cr.dup.fao$country_code]
  cr3b <- merge(cr3b,cr.dup.gaul,by.x = c('country_code','country'),
                by.y = c('fao_country_code','fao_country'),all.x = TRUE,allow.cartesian = TRUE)
  cr3b[,fao_cr_ha := fao_cr_ha * fao_cf_area]
  cr3b.sel <- c('asap0_id','name0','SPAM_short_name')
  cr3b[,afrac := fao_cr_ha / sum(fao_cr_ha),by = cr3b.sel]
  cr3b[,fao_cr_ha := sum(fao_cr_ha,na.rm=T),by = cr3b.sel]
  cr3b[,fao_cereal_fa := sum(fao_cereal_fa * afrac,na.rm=T),by = cr3b.sel]
  cr3b[,fao_cr_yield := sum(fao_cr_yield * afrac,na.rm=T),by = cr3b.sel]
  cr3b[,fao_crres_kgnha := sum(fao_crres_kgnha * afrac,na.rm=T),by = cr3b.sel]
  cr3b[,fr_burn := sum(fr_burn * afrac,na.rm=T),by = cr3b.sel]
  cr3b <- cr3b[,mget(c('asap0_id','name0','SPAM_short_name','fao_cereal_fa','fao_cr_ha','fao_cr_yield','fao_crres_kgnha','fr_burn'))]
  cr3b <- unique(cr3b)

  # subset country db where both situations occur
  cr3c <- cr3[country_code %in% cr.dup.gaul$fao_country_code & country_code %in% cr.dup.fao$country_code]
  cr3c <- merge(cr3c,cr.dup.fao,by = c('country_code','country','fao_area_tot'),all.x = TRUE)
  cr3c.sel <- c('gaul_asap0_id','gaul_name0','SPAM_short_name')
  cr3c[,afrac := fao_cr_ha / sum(fao_cr_ha),by = cr3c.sel]
  cr3c[,fao_cr_ha := sum(fao_cr_ha,na.rm=T),by = cr3c.sel]
  cr3c[,fao_cr_yield := sum(fao_cr_yield * afrac,na.rm=T),by = cr3c.sel]
  cr3c[,fao_cereal_fa := sum(fao_cereal_fa * afrac,na.rm=T),by = cr3c.sel]
  cr3c[,fao_crres_kgnha := sum(fao_crres_kgnha * afrac,na.rm=T),by = cr3c.sel]
  cr3c[,fr_burn := sum(fr_burn * afrac,na.rm=T),by = cr3c.sel]
  cr3c <- cr3c[,mget(c(cr3c.sel,'country_code','country','fao_cr_ha','fao_cr_yield','fao_crres_kgnha','fr_burn','fao_cereal_fa'))]
  cr3c <- unique(cr3c)
  cr3c <- merge(cr3c,cr.dup.gaul,by.x = c('country_code','country'),
                by.y = c('fao_country_code','fao_country'),all.x = TRUE,allow.cartesian = TRUE)
  cr3c[,fao_cr_ha := fao_cr_ha * fao_cf_area]
  cr3c.sel <- c('asap0_id','name0','SPAM_short_name')
  cr3c[,afrac := fao_cr_ha / sum(fao_cr_ha),by = cr3c.sel]
  cr3c[,fao_cr_ha := sum(fao_cr_ha,na.rm=T),by = cr3c.sel]
  cr3c[,fao_cereal_fa := sum(fao_cereal_fa * afrac,na.rm=T),by = cr3c.sel]
  cr3c[,fao_cr_yield := sum(fao_cr_yield * afrac,na.rm=T),by = cr3c.sel]
  cr3c[,fao_crres_kgnha := sum(fao_crres_kgnha * afrac,na.rm=T),by = cr3c.sel]
  cr3c[,fr_burn := sum(fr_burn * afrac,na.rm=T),by = cr3c.sel]
  cr3c <- cr3c[,mget(c('asap0_id','name0','SPAM_short_name','fao_cr_ha','fao_cereal_fa','fao_cr_yield','fao_crres_kgnha','fr_burn'))]
  cr3c <- unique(cr3c)

  # countries that are unique
  cr3d <- cr3[!(country_code %in% cr.dup.gaul$fao_country_code | country_code %in% cr.dup.fao$country_code)]
  cr3d <- merge(cr3d,country_id,by.x='country_code', by.y = 'fao_country_code',all.x=TRUE)
  cr3d.sel <- c('gaul_asap0_id','gaul_name0','SPAM_short_name')
  cr3d[,afrac := fao_cr_ha / sum(fao_cr_ha),by = cr3d.sel]
  cr3d[,fao_cr_ha := sum(fao_cr_ha,na.rm=T),by = cr3d.sel]
  cr3d[,fao_cr_yield := sum(fao_cr_yield * afrac,na.rm=T),by = cr3d.sel]
  cr3d[,fao_cereal_fa := sum(fao_cereal_fa * afrac,na.rm=T),by = cr3d.sel]
  cr3d[,fao_crres_kgnha := sum(fao_crres_kgnha * afrac,na.rm=T),by = cr3d.sel]
  cr3d[,fr_burn := sum(fr_burn * afrac,na.rm=T),by = cr3d.sel]
  cr3d <- cr3d[,mget(c('gaul_asap0_id','gaul_name0','SPAM_short_name','fao_cr_ha','fao_cereal_fa','fao_cr_yield','fao_crres_kgnha','fr_burn'))]
  setnames(cr3d,gsub('^gaul_','',colnames(cr3d)))
  cr3d <- unique(cr3d)

  # combine all these joins
  cr4 <- rbind(cr3a,cr3b,cr3c,cr3d)
  setnames(cr4,'SPAM_short_name','crop')

  # add to country climate shape
  cc.new3 <- merge(unique(cc.new2),cr4,by=c('asap0_id','name0','crop'),all.x = TRUE)

  # correct for millet and coffee
  cc.new3 <- merge(cc.new3,cr.dup.crop,by='ccID',all.x = TRUE)
  cc.new3[crop == 'PMIL', fao_cr_ha := fao_cr_ha * fr_MIL]
  cc.new3[crop == 'SMIL', fao_cr_ha := fao_cr_ha * (1-fr_MIL)]
  cc.new3[crop == 'ACOF', fao_cr_ha := fao_cr_ha * fr_COF]
  cc.new3[crop == 'RCOF', fao_cr_ha := fao_cr_ha * (1-fr_COF)]
  cc.new3[,c('fr_COF','fr_MIL') := NULL]

# step 5. add soil organic carbon content ----

  # merge with country-climate shapefile
  cc.new3 <- merge(cc.new3,cc.soc,by='ccID')

# step 6. making final db ----

  # add crop group
  cc.new3 <- merge(cc.new3,unique(crops_id[,.(SPAM_short_name,GROUP)]),by.x = 'crop',by.y = 'SPAM_short_name')

  # area correction (scaling SPAM 2005 to FAO 2018)
  cc.new3[, area_cf := fao_cr_ha / sum(area_till,na.rm=T),by=.(asap0_id,crop)]
  cc.new3[is.na(area_cf), area_cf := 1]
  cc.new3[,area_till_cr := area_cf * area_till]

  # relevant columns to be stored
  col.sel <- c('ccID','climBeckGroup','area_till_cr','TECH_INT','tillage','crop','GROUP','SOC',
               'fao_cr_yield','fao_crres_kgnha','fr_burn',
               'ndm_loss','nloss_kgha','ndm_kgha','nif_kgha','fao_cereal_fa')

  # subset database for only relevant columns
  cc.new4 <- cc.new3[,mget(col.sel)]

  # make category where crop residues are burned
  cc.new4[fr_burn > 0 & fao_cereal_fa > 0.5, cat_crb := 'BURN']
  cc.new4[is.na(cat_crb),cat_crb := 'INC']

  # make fertilizer categories (> 40 inorganic is Western Europe, Africa is below 5)
  cc.new4[,ninput := pmax(0,ndm_kgha,na.rm=T) + pmax(0,nif_kgha,na.rm=T)]
  cc.new4[ninput < 40, cat_fert := 'NIFNOF']
  cc.new4[(ndm_kgha <= 40 | is.na(ndm_kgha)) & ninput <= 100, cat_fert := 'MIFNOF']
  cc.new4[ndm_kgha > 40 & ninput <= 100, cat_fert := 'MIFHOF']
  cc.new4[(ndm_kgha <= 40 | is.na(ndm_kgha)) & ninput > 100, cat_fert := 'HIFNOF']
  cc.new4[ndm_kgha > 40 & ninput > 100, cat_fert := 'HIFHOF']

  # make categories for crop rotation
  cc.new4[,fao_cereal_fa := pmax(0,fao_cereal_fa,na.rm=T)]
  cc.new4[GROUP != 'cereals' & fao_cereal_fa <= 0.5, cat_ccr := 'CATCH']
  cc.new4[is.na(GROUP),cat_ccr := 'CATCH']
  cc.new4[GROUP=='cereals' | fao_cereal_fa > 0.5, cat_ccr := 'NOCATCH']

  # remove variables not needed  any more
  cc.new4[,c('fao_cereal_fa','ninput','fr_burn','fao_crres_kgnha') := NULL]

  # round area
  cc.new4[,area_till_cr := round(area_till_cr,3)]

  # save the database
  fwrite(cc.new4,file='data/210322DB001_ccID.csv')

# step 7. dcast the database and save the mean per ccID ----------

  # dcast database given relevant categories (nmd_loss is total sum per ccID here)
  cc.new5 <- dcast(cc.new4,cat_crb + cat_fert + cat_ccr + tillage + climBeckGroup ~ .,
                   fun = list(sum,mean,mean,mean,mean,mean),
                   value.var = list('area_till_cr','SOC','fao_cr_yield','ndm_loss','nloss_kgha','ndm_kgha'),
                   na.rm=T)

  # rename
  setnames(cc.new5,c('area_till_cr_sum','fao_cr_yield_mean'),c('area','yield'))

  # round area to an integer
  cc.new5[,area := round(area)]
  cc.new5[,yield := round(yield,2)]
  cc.new5[,SOC_mean := round(SOC_mean,2)]

  # save csv file
  fwrite(cc.new5,file='../data/products/210322DB001.csv')


