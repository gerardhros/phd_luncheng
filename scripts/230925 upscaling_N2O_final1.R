
# make one data.table with all site properties needed for upscaling meta-analytical results for global maps impact of measures on N2O, NH3 and N leaching

# load packages
require(sf);require(terra);require(data.table);require(metafor)


# clear environment
rm(list=ls())

# load all current practices and site properties

# read the raster with soil data
r.soil <- terra::rast('data/soil.tif')

# read the raster with climatic data
r.clim <- terra::rast('data/climate.tif')

# read the technology level (1 = low, 0 is high)
r.agtech <- terra::rast('products/r.agtech.tif')

# read the current NUE from IMAGE
r.nue <- terra::rast('products/r.nue.tif')


# This is the N does in the last version, I put it here, I don't know if need it this time?
# read the earlier prepared file with manure N dose
r.nman <- terra::rast('data/nofert.tif')

# read the earlier prepared file with fertilizer N dose
r.nfert <- terra::rast('data/nifert.tif')

# read the main crops used in the MA models of Luncheng
r.cwheat <- terra::rast('products/r.cwheat.tif')
r.cmaize <- terra::rast('products/r.cmaize.tif')
r.crice <- terra::rast('products/r.crice.tif')
r.cother <- terra::rast('products/r.cother.tif')

# read the main tillage practices per crop type
r.notill <- terra::rast('products/r.notill.tif')

# read in the crop intensity (1 = high, 0 = low)
r.cropint <- terra::rast('products/r.cropint.tif')

# read in where cover_catch cropping can be applied (1 = no catch crop applied, 0 = catch crop is current practice)
r.ccrop <- terra::rast('products/r.ccrop.tif')

# read in where crop residues can be applied (1 = no residue applied, 0 = incorporation is current practice)
r.residue <- terra::rast('products/r.residue.tif')

# read in whether there is sufficient manure not yet used (1 = there is manure available, 0 = there is no manure left for better practices)
r.ndmloss <- terra::rast('products/r.ndmloss.tif')

# stack all rasters
r.all <- c(r.soil,r.clim,r.agtech,r.nue,r.cwheat,r.cmaize,r.crice,r.cother,r.notill,r.cropint,r.ccrop,r.residue,r.ndmloss, r.nman, r.nfert)

# convert to data.table
r.dt <- as.data.table(as.data.frame(r.all,xy=TRUE, na.rm = FALSE))


# Here I've added mat,pre,nh4,no3,nam, similar to the previous version.
# setnames
setnames(r.dt,
         old = c('climate_mat', 'climate_pre','isric_phw_mean_0_5','isric_clay_mean_0_5','isric_soc_mean_0_5',
                 'nifert_nfert_nh4','nifert_nfert_no3','nofert_nofert'),
         new = c('mat','pre','phw','clay','soc','nh4','no3','nam'),skip_absent = T)

# select only land area
r.dt <- r.dt[!(is.na(mat)|is.na(pre))]
r.dt <- r.dt[!(is.na(cwheat) & is.na(cmaize) & is.na(crice) & is.na(cother))]

# replace missing values (adapt values due to (assume that measure can not be applied)
r.dt[,nue_ag := fifelse(is.na(nue_ag),median(nue_ag,na.rm=T),nue_ag)]
r.dt[is.na(NOTILL), NOTILL := 0]
r.dt[is.na(CROPINT), CROPINT := 0]
r.dt[is.na(CCROP), CCROP := 0]
r.dt[is.na(RES), RES := 0]
r.dt[is.na(ndm_loss_cat), ndm_loss_cat := 0]

r.dt[is.na(nfert_nh4), nfert_nh4 := 0]
r.dt[is.na(nfert_no3), nfert_no3 := 0]

# remove columns not needed
r.dt[,c('cropintensity','ccID') := NULL]

# If nue_ag is less than 0.5, assign it the value 1, otherwise assign it the value 0.
r.dt$nue_ag <- ifelse(r.dt$nue_ag < 0.5, 1, 0)
# a short description
# TECH_LOW: if value is 1 then you can apply RFP, EE and RFT. if value is 0 then these measures are already practice.
# if nue_ag < 0.5 then you can apply RFR
# NOTILL: if value is 1 then you can apply no-till. If value is zero then no-till is already practice
# RES: if value is 1 then you can apply crop residues. If value is zero then application of crop residues is already practice
# CATCH: if value is 1 then you can apply catch crops. If value is zero then application of use of catch crops is already practice or not possible.
# CROPINT: if value is 1 then there is a possiblity to reduce intensity. If value is zero then adding less crops is not possible.
# ndm_loss_cat: if the value is 1 then CF or OF can be applied. if value is zero, then there is no manure left to do so (since all manure is applied already)
# cwheat, cmaize, crice and cother show whether the crops are growing there (clusterd in these categories following your meta-analytical models)


##################################################################################################################################################################

# derive the meta-analytical model

    # read data
    d1 <- readxl::read_xlsx('articles/paper3/You_paper3_S2.xlsx',sheet = 2)
    d1 <- as.data.table(d1)


    # add CV for NUE treatment and estimate the SD for missing ones
    d2<-d1
    CV_n2ot_bar<-mean(d2$n2ot_sd[is.na(d2$n2ot_sd)==FALSE]/d2$n2ot_mean[is.na(d2$n2ot_sd)==FALSE])
    d2$n2ot_sd[is.na(d2$n2ot_sd)==TRUE]<-d2$n2ot_mean[is.na(d2$n2ot_sd)==TRUE]*1.25*CV_n2ot_bar

    CV_n2oc_bar<-mean(d2$n2oc_sd[is.na(d2$n2oc_sd)==FALSE]/d2$n2oc_mean[is.na(d2$n2oc_sd)==FALSE])
    d2$n2oc_sd[is.na(d2$n2oc_sd)==TRUE]<-d2$n2oc_mean[is.na(d2$n2oc_sd)==TRUE]*1.25*CV_n2oc_bar

    # clean up column names
    setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
    setnames(d2,tolower(colnames(d2)))

    #Supplement and update the missing values for n_dose
    d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

    # calculate effect size (N2O)
    es21 <- escalc(measure = "ROM", data = d2,
                   m1i = n2ot_mean, sd1i = n2ot_sd, n1i = replication,
                   m2i = n2oc_mean, sd2i = n2oc_sd, n2i = replication )

    # convert to data.tables
    d02 <- as.data.table(es21)

    # what are the treatments to be assessed
    d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))

    # scale the variables to unit variance
    d02[,clay_scaled := scale(clay)]
    d02[,soc_scaled := scale(soc)]
    d02[,ph_scaled := scale(ph)]
    d02[,mat_scaled := scale(mat)]
    d02[,map_scaled := scale(map)]
    d02[,n_dose_scaled := scale(n_dose)]


    d02[,cf := fifelse(management == 'CF'| fertilizer_type == 'CF','yes','no')]
    d02[,ee := fifelse(management == 'EE'| fertilizer_type == 'EE','yes','no')]
    d02[,of := fifelse(management == 'OF'| fertilizer_type == 'OF','yes','no')]
    d02[,ct := ifelse(!management %in% c('EE','CF','OF') | fertilizer_type == 'MF','yes','no')]
    d02[,bc := fifelse(management=='BC','yes','no')]
    d02[,rfr := fifelse(management=='RFR' | rfr == 'yes','yes','no')]
    d02[,rft := fifelse(management=='RFT' | rft == 'yes','yes','no')]
    d02[,rfp := fifelse(management=='RFP'|rfp =='yes','yes','no')]

    d02[,res := fifelse(management=='RES' | crop_residue == 'yes','yes','no')] #crop management measures
    d02[,cc := fifelse(management=='CC' | cover_crop == 'yes','yes','no')]
    d02[,rot := fifelse(management=='ROT' | crop_rotation == 'yes','yes','no')]

    d02[,nt := fifelse(management=='NT' | tillage == 'NT','yes','no')] #soil management measures
    d02[,rt := fifelse(management=='RT'| tillage == 'RT','yes','no')]
    d02[,ct := ifelse(!management %in% c('NT','RT') | tillage == 'CT','yes','no')]

    #Combining different factors
    d02[crop_type=='vegetable', crop_type := 'other']
    # d02[tillage=='reduced', tillage := 'no-till']


    # make metafor model

    m1 <- rma.mv(yi,vi,
                 mods = ~ee + cf + of + rfr + rft + rfp + bc + res + cc + rot + nt + rt + n_dose_scaled + crop_type +
                   ph_scaled + clay_scaled + soc_scaled + map_scaled + mat_scaled + ee : ph_scaled +
                   rfr : n_dose_scaled + rfr : crop_type + rfp : n_dose_scaled - 1,
                 data = d02,
                 random = list(~ 1|studyid), method="REML",sparse = TRUE)

    # see model structure that need to be filled in to predict NUE as function of the system properties
    p1 <- predict(m1,addx=T)

    m1# this is the order of input variables needed for model predictions (=newmods in predict function)
    m1.cols <- colnames(p1$X)

    # make prediction dataset for situation that soil is fertilized by both organic and inorganic fertilizers, conventional fertilizer strategy
    dt.new <- copy(r.dt)

    # add the columns required for the ma model, baseline scenario
    # baseline is here defined as the current practice, being derived from existing datasets
    dt.new[, eeno := fifelse(TECH_LOW == 1,1,0)]# in tech-low sites the default is "yes", in tech-high sites, the default is "no"
    dt.new[, eeyes :=  fifelse(TECH_LOW == 1,0,1)] # in tech-low sites the default is "no", in tech-high sites, the default is "yes"
    dt.new[, cfyes := fifelse(ndm_loss_cat == 1,0,1)]
    dt.new[, ofyes := fifelse(ndm_loss_cat == 1,0,1)]
    dt.new[, rfryes := fifelse(nue_ag == 1,0,1)] # in sites with high NUE the default is "no", in sites low in NUE, the default is "yes"
    dt.new[, rftyes :=  fifelse(TECH_LOW == 1,0,1)] # in tech-low sites the default is "no", in tech-high sites, the default is "yes"
    dt.new[, rfpyes := fifelse(TECH_LOW == 1,0,1)] # in tech-low sites the default is "no", in tech-high sites, the default is "yes"
    dt.new[, bcyes := 0]
    dt.new[, resyes := fifelse(RES == 1,0,1)] # in sites where residues are removed the default is "no", in sites where no residues can be applied, the default is "yes"
    dt.new[, ccyes :=  fifelse(CCROP == 1,0,1)]
    dt.new[, rotyes := fifelse(CROPINT == 1,0,1)]
    dt.new[, ntyes := fifelse(NOTILL == 1,0,1)] # in sites being ploughed the default is "no", in sites already being no-till, the default is "yes"
    dt.new[, rtyes := fifelse(NOTILL == 1,0,1)]

    dt.new[, n_dose_scaled := scale(nfert_nh4+nfert_no3+nofert)]

    dt.new[, crop_typeother := cother]
    dt.new[, crop_typerice := crice]
    dt.new[, crop_typewheat := cwheat]
    dt.new[, crop_typemaize := cmaize]

    dt.new[, ph_scaled := (phw * 0.1 - mean(d02$ph)) / sd(d02$ph)]
    dt.new[, clay_scaled := (clay * 0.1 - mean(d02$clay)) / sd(d02$clay)]
    dt.new[, soc_scaled := (soc * 0.1 - mean(d02$soc)) / sd(d02$soc)]
    dt.new[, map_scaled := (pre - mean(d02$map)) / sd(d02$map)]
    dt.new[, mat_scaled := (mat  - mean(d02$mat)) / sd(d02$mat)]
    dt.new[, `eeyes:ph_scaled` := eeyes*ph_scaled]
    dt.new[, `rfryes:n_dose_scaled` := rfryes*n_dose_scaled]
    dt.new[, `rfryes:crop_typeother` := rfryes*crop_typeother]
    dt.new[, `rfryes:crop_typerice` := rfryes*crop_typerice]
    dt.new[, `rfryes:crop_typewheat` := rfryes*crop_typewheat]
    dt.new[, `rfpyes:n_dose_scaled` := rfpyes*n_dose_scaled]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.new[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))

   # add predictions to the data.table
     cols <- c('pROMmean','pROMse','pROMcil','pROMciu','pROMpil','pROMpiu')

     dt.new[,c(cols) := dt.pred]

     # change ROM into the relative change due to a measure
     dt.new[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]



    ############################################## scenario 1 (EE) ################################################################

    # make local copy
    dt.s1 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s1[, eeyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s1[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s1 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s1[,c(cols) := dt.pred.s1]
    dt.s1[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # Here I changed the code.
    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s1[,.(x,y,s1 = pROMmeanAbs)],by=c('x','y'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s1 - base]

    # remove outliers (do not allow changes bigger than -150%)
    dt.fin[, improvement := pmax(-150,improvement)]

    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'products/scenario_n2o_ee_1.tif', overwrite = TRUE)



    ############################################## scenario S2 (CF) ################################################################
    # make local copy
    dt.s2 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s2[, cfyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s2[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s2 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s2[,c(cols) := dt.pred.s2]
    dt.s2[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s2[,.(x,y,s2 = pROMmeanAbs)],by=c('x','y'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s2 - base]

    # remove outliers (do not allow changes bigger than -150%)
    dt.fin[, improvement := pmax(-150,improvement)]

    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'products/scenario_n2o_cf_2.tif', overwrite = TRUE)


    ############################################## scenario S3 (OF) ################################################################
    # make local copy
    dt.s3 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s3[, ofyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s3[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s3 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s3[,c(cols) := dt.pred.s3]
    dt.s3[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,ndm_loss_cat)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s3[,.(x,y,s3 = pROMmeanAbs,ndm_loss_cat)],by=c('x','y','ndm_loss_cat'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s3 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = ndm_loss_cat)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_of_3.tif', overwrite = TRUE)


    ############################################## scenario S4 (RFR) ################################################################
    # make local copy
    dt.s4 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s4[, rfryes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s4[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s4 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s4[,c(cols) := dt.pred.s4]
    dt.s4[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,nue_ag)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s4[,.(x,y,s4 = pROMmeanAbs,nue_ag)],by=c('x','y','nue_ag'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s4 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = nue_ag)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_rfr_4.tif', overwrite = TRUE)

    ############################################## scenario s5 (RFT) ################################################################
    # make local copy
    dt.s5 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s5[, rftyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s5[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s5 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s5[,c(cols) := dt.pred.s5]
    dt.s5[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,TECH_LOW)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s5[,.(x,y,s5 = pROMmeanAbs,TECH_LOW)],by=c('x','y','TECH_LOW'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s5 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = TECH_LOW)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_rft_5.tif', overwrite = TRUE)

    ############################################## scenario s6 (RFP) ################################################################
    # make local copy
    dt.s6 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s6[, rfpyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s6[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s6 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s6[,c(cols) := dt.pred.s6]
    dt.s6[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,TECH_LOW)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s6[,.(x,y,s6 = pROMmeanAbs,TECH_LOW)],by=c('x','y','TECH_LOW'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s6 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = TECH_LOW)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_rfp_6.tif', overwrite = TRUE)


    ############################################## scenario s7 (BC) ################################################################
    # make local copy
    dt.s7 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s7[, bcyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s7[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s7 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s7[,c(cols) := dt.pred.s7]
    dt.s7[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cother,cmaize,cwheat,crice,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s7[,.(x,y,s7 = pROMmeanAbs,cother,cmaize,cwheat,crice)],by=c('x','y','cother','cmaize','cwheat','crice'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s7 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = 1)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_bc_7.tif', overwrite = TRUE)


    ############################################## scenario s8 (RES) ################################################################
    # make local copy
    dt.s8 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s8[, resyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s8[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s8 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s8[,c(cols) := dt.pred.s8]
    dt.s8[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,RES)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s8[,.(x,y,s8 = pROMmeanAbs,RES)],by=c('x','y','RES'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s8 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = RES)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_res_8.tif', overwrite = TRUE)


    ############################################## scenario S9 (CC) ################################################################
    # make local copy
    dt.s9 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s9[, ccyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s9[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s9 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s9[,c(cols) := dt.pred.s9]
    dt.s9[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,CCROP)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s9[,.(x,y,s9 = pROMmeanAbs,CCROP)],by=c('x','y','CCROP'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s9 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = CCROP)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_cc_9.tif', overwrite = TRUE)


    ############################################## scenario s10 (ROT) ################################################################
    # make local copy
    dt.s10 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s10[, rotyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s10[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s10 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s10[,c(cols) := dt.pred.s10]
    dt.s10[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,CROPINT)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s10[,.(x,y,s10 = pROMmeanAbs,CROPINT)],by=c('x','y','CROPINT'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s10 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = CROPINT)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_rot_10.tif', overwrite = TRUE)


    ############################################## scenario s11 (NT) ################################################################
    # make local copy
    dt.s11 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s11[, ntyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s11[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s11 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s11[,c(cols) := dt.pred.s11]
    dt.s11[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,NOTILL)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s11[,.(x,y,s11 = pROMmeanAbs,NOTILL)],by=c('x','y','NOTILL'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s11 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = NOTILL)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_nt_11.tif', overwrite = TRUE)

    ############################################## scenario s12 (RT) ################################################################
    # make local copy
    dt.s12<- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s12[, rtyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s12[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s12 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s12[,c(cols) := dt.pred.s12]
    dt.s12[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,NOTILL)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s12[,.(x,y,s12 = pROMmeanAbs,NOTILL)],by=c('x','y','NOTILL'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s12 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = NOTILL)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_rt_12.tif', overwrite = TRUE)

    ############### scenario run ((combined optimal management practices)) ###########################

    # make local copy
    dt.s0 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    # update actions taken for scenario 1
    dt.s0[, eeyes := 1]
    dt.s0[, cfyes := 1]
    dt.s0[, ofyes := 0]
    dt.s0[, rfryes := 1]
    dt.s0[, rftyes := 1]
    dt.s0[, rfpyes := 0]
    dt.s0[, bcyes := 1]
    dt.s0[, resyes := 0]
    dt.s0[, ccyes := 0]
    dt.s0[, rotyes := 0]
    dt.s0[, ntyes := 0]
    dt.s0[, rtyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s0[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred.s0 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s0[,c(cols) := dt.pred.s0]
    dt.s0[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cother,cmaize,cwheat,crice,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s0[,.(x,y,s0 = pROMmeanAbs,cother,cmaize,cwheat,crice)],by=c('x','y','cother','cmaize','cwheat','crice'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s0 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_n2o_combination.tif', overwrite = TRUE)

