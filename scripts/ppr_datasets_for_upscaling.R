# data extraction upscaling for Luncheng, paper 4
# this script contains the preparation of spatial datafiles
# data are prepared on a separate server, and original files are not available in this repo.
# source of the data are described above each block.
#
# Gerard H. Ros, september 2023

# clear environment
rm(list=ls())

# require packages
require(sf);require(raster);require(data.table);require(foreign)

# prepare combined shape from countries and climate zones
if(FALSE){

  # read in the country shape
  # source: https://data.europa.eu/euodp/data/dataset/jrc-10112-10004
  s1 <- st_read('data/gaul0_asap.shp',quiet = TRUE)

  # read in the climate shapes
  # source: http://koeppen-geiger.vu-wien.ac.at/present.htm)
  s2 <- st_read('data/1976-2000.shp',quiet = TRUE) %>% st_transform(4326)

  # intersect both shapes to get all spatial combinations
  cc <- st_intersection(s1,s2)

  # remove files not needed anymore
  rm(s1,s2)

  # save object
  saveRDS(cc,file='data/countryclimate.rds')

} else {

  cc <- readRDS('data/countryclimate.rds')
}


# prepare raster file Koppen-Geiger climate zones
# source: https://figshare.com/articles/Present_and_future_K_ppen-Geiger_climate_classification_maps_at_1-km_resolution/6396959/2
if(FALSE){

  # load the raster with Climate Zones
  climzone <- raster('data/Beck_KG_V1_present_0p0083.tif')

  # lower the resolution (for speed reasons)
  climzone <- aggregate(climzone, fact=10, fun = modal)

  # save the file to products directory
  writeRaster(climzone,file='data/climzone83km.tif',format="GTiff", overwrite=TRUE)

} else {

  # read the earlier prepared file with climate zones
  climzone <- raster('data/climzone83km.tif')
}

# prepare raster with cropland area
# source: https://www.esa-landcover-cci.org/?q=node/164)
if(FALSE){

  # load raster with cropland area
  cropland <- raster('data/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')

  # lower the resolution (for speed reasons)
  cropland <- aggregate(cropland,fact = 30, fun=modal)

  # remove water class from the raster
  cropland[cropland>=210] <- NA

  # remove trees and shrubland
  cropland[!cropland %in% c(10,20,30,40,200)] <- NA

  # save the file to products directory
  writeRaster(cropland,file='data/cropland83km_v2.tif',format="GTiff", overwrite=TRUE)

} else {

  # read the earlier prepared file with cropland area
  cropland <- raster('data/cropland83km_v2.tif')
}


# prepare raster with tillage practices
# source: ftp://datapub.gfz-potsdam.de/download/10.5880.PIK.2019.011/
if(FALSE){

  # tillage map names per crop
  crops = c('whea_till', 'rice_till', 'maiz_till', 'barl_till', 'rest_till', 'ooil_till', 'toba_till', 'teas_till',
            'coco_till', 'rcof_till', 'acof_till', 'ofib_till', 'cott_till', 'sugb_till', 'sugc_till', 'oilp_till',
            'vege_till', 'temf_till', 'trof_till', 'plnt_till', 'bana_till', 'cnut_till', 'grou_till', 'orts_till',
            'cass_till', 'yams_till', 'swpo_till', 'pota_till', 'sesa_till', 'rape_till', 'sunf_till', 'soyb_till',
            'opul_till', 'lent_till', 'pige_till', 'cowp_till', 'chic_till', 'bean_till', 'ocer_till', 'sorg_till',
            'smil_till', 'pmil_till', 'scenario_ca_area')

  # read in the rasters from nc4 file
  tillage <- raster('data/tillage_revised.nc4',varname=crops[1])

  # make rasterstack for all crops
  for(i in 2:length(crops)){tillage <- stack(tillage,raster('../data/tillage/tillage_revised.nc4',varname=crops[i]))}

  # make extend equal to other maps
  tillage <- extend(tillage,cropland)

  # save the file to products directory as RDS file
  saveRDS(tillage,file='data/tillage83km.rds')

} else {

  # read the earlier prepared file with tillage practices
  tillage <- readRDS('data/tillage83km.rds')

}

# prepare manure N dose on cropland (take latest year present)
# source: https://doi.pangaea.de/10.1594/PANGAEA.871980
# units: kg N / km2
if(FALSE){

  # read the file with manure application data (kg N / km2 grid cell)
  nman <- fread('data/manure_ndose_yy2014.txt')
  nman <- raster(nrows=2124,ncols=4320,xmn=-180,ymn=-88.5,crs = "+proj=longlat +datum=WGS84",
                 vals=as.matrix(nman))

  # retreive values for new raster object
  nman <- resample(nman,climzone,method='bilinear')

  # manure area of gridcell
  nman.area <- area(nman)

  # save the file to products directory
  saveRDS(nman,file='data/nman83km.rds')
  saveRDS(nman.area,file='data/nman_area83km.rds')

} else {

  # read the earlier prepared file with manure N dose on cropland
  nman <- readRDS('data/nman83km.rds')
  nman.area <- readRDS('data/nman_area83km.rds')
}


# prepare manure N production (take latest year present)
# source: https://doi.pangaea.de/10.1594/PANGAEA.871980
# units: kg N / km2
if(FALSE){

  # read the file with manure production data (kg N / km2 grid cell / year)
  nmanprod <- fread('data/manure_nproduction_yy2014.txt')
  nmanprod[nmanprod==-9999] <- NA
  nmanprod <- raster(nrows=2124,ncols=4320,xmn=-180,ymn=-88.5,crs = "+proj=longlat +datum=WGS84",
                     vals=as.matrix(nmanprod))

  # retreive values for new raster object, in kg / km2 gridcell
  nmanprod <- resample(nmanprod,climzone,method='ngb')

  # manure area of gridcell
  nmanprod.area <- area(nmanprod)

  # save the file to products directory
  saveRDS(nmanprod,file='data/nmanprod83km.rds')
  saveRDS(nmanprod.area,file='data/nmanprod_area83km.rds')

} else {

  # read the earlier prepared file with manure N dose production
  nmanprod <- readRDS('data/nmanprod83km.rds')
  nmanprod.area <- readRDS('data/nmanprod_area83km.rds')
}



# prepare manure N application and deposition (taking same year as cropland)
# source: https://doi.pangaea.de/10.1594/PANGAEA.892940
# units: kg N / km2
if(FALSE){


  # read the ascii data file with grid ifnormation
  this.d <- fread('data/manure_application_gld_2014.txt',skip = 6)

  # convert to raster, in values kg N per km2 raster cell
  # be aware raster cells have different areas
  this.r <- raster(nrows=354,ncols=720,xmn=-180,ymn=-88.5,crs = "+proj=longlat +datum=WGS84",
                   vals=as.matrix(this.d))

  # replace negative ones with NA
  this.r@data@values <- ifelse(this.r@data@values<0,NA,this.r@data@values)

  # retrieve values for new raster object (in kg N per km2 of raster cell)
  # can be interpolated since resolution of climzone is much smaller than for this.r
  # but nearest neighbour results overall in better agreement with global total
  nman_gld_app <- resample(this.r,climzone,method='ngb')

  # manure area of gridcell
  nman_gld_app.area <- area(nman_gld_app)

  # read the ascii data file with grid ifnormation
  this.d <- fread('data/manure_deposition_gld_2014.txt',skip = 6)

  # convert to raster, in values kg N per km2 raster cell
  # be aware raster cells have different areas
  this.r <- raster(nrows=354,ncols=720,xmn=-180,ymn=-88.5,crs = "+proj=longlat +datum=WGS84",
                   vals=as.matrix(this.d))

  # replace negative ones with NA
  this.r@data@values <- ifelse(this.r@data@values<0,NA,this.r@data@values)

  # retrieve values for new raster object (in kg N per km2 of raster cell)
  nman_gld_dep <- resample(this.r,climzone,method='ngb')

  # manure area of gridcell
  nman_gld_dep.area <- area(nman_gld_dep)

  # save the files to products directory
  saveRDS(nman_gld_dep,file='data/nman_gld_dep83km.rds')
  saveRDS(nman_gld_app,file='data/nman_gld_app83km.rds')
  saveRDS(nman_gld_app.area,file='data/nman_gld_app_area83km.rds')
  saveRDS(nman_gld_dep.area,file='data/nman_gld_dep_area83km.rds')

} else {

  # read the earlier prepared file with manure N dose (deposited via grazing and applied on grassland)
  # units are in kg/km2 grid cell
  nman_gld_dep <- readRDS('data/nman_gld_dep83km.rds')
  nman_gld_app <- readRDS('data/nman_gld_app83km.rds')
  nman_gld_dep.area <- readRDS('data/nman_gld_app_area83km.rds')
  nman_gld_app.area <- readRDS('data/nman_gld_dep_area83km.rds')

}

# estimate manure nitrogen not applied

# prepare manure N application and deposition (taking same year as cropland)
# source: https://doi.pangaea.de/10.1594/PANGAEA.892940
# units: kg N / km2
if(FALSE){


  # read the ascii data file with grid ifnormation
  this.d1 <- fread('data/manure_application_gld_2014.txt',skip = 6)
  this.d2 <- fread('data/manure_deposition_gld_2014.txt',skip = 6)
  this.d3 <- fread('data/manure_nproduction_yy2014.txt')
  this.d4 <- fread('data/manure_ndose_yy2014.txt')

  # convert to raster, in values kg N per km2 raster cell
  this.r1 <- raster(nrows=354,ncols=720,xmn=-180,ymn=-88.5,
                    crs = "+proj=longlat +datum=WGS84",
                    vals=as.matrix(this.d1))
  this.r2 <- raster(nrows=354,ncols=720,xmn=-180,ymn=-88.5,
                    crs = "+proj=longlat +datum=WGS84",
                    vals=as.matrix(this.d2))
  this.r3 <- raster(nrows=2124,ncols=4320,xmn=-180,ymn=-88.5,
                    crs = "+proj=longlat +datum=WGS84",
                    vals=as.matrix(this.d3))
  this.r4 <- raster(nrows=2124,ncols=4320,xmn=-180,ymn=-88.5,
                    crs = "+proj=longlat +datum=WGS84",
                    vals=as.matrix(this.d4))

  # replace negative ones with NA
  this.r1@data@values <- ifelse(this.r1@data@values<=0,NA,this.r1@data@values)
  this.r2@data@values <- ifelse(this.r2@data@values<=0,NA,this.r2@data@values)
  this.r3@data@values <- ifelse(this.r3@data@values<=0,NA,this.r3@data@values)
  this.r4@data@values <- ifelse(this.r4@data@values<=0,NA,this.r4@data@values)

  # aggregate inputs to the same resolution
  this.r1n <- disaggregate(this.r1,fact = 6, method='')
  this.r2n <- disaggregate(this.r2,fact = 6, method='')

  # calculate manure not yet applied, assuming 20% losses NH3 and NOx
  # assume that 50% of the applied manure on grassland can be applied on cropland
  # r1 = grasland, r2 = weiden, r3 = productie, r4 = cropland
  this.r5 <- this.r3 - this.r1n  - this.r2n - this.r4 / 0.75
  this.r5@data@values <- ifelse(this.r5@data@values<=0,NA,this.r5@data@values)

  # retrieve values for new raster object (in kg N per km2 of raster cell)
  # can be interpolated since resolution of climzone is much smaller than for this.r
  # but nearest neighbour results overall in better agreement with global total
  nman_nlos <- resample(this.r5,climzone,method='ngb')

  # save the files to products directory
  saveRDS(nman_nlos,file='data/nman_nlos83km.rds')

} else {

  # read the earlier prepared file
  nman_nlos <- readRDS('data/nman_nlos83km.rds')

}




# prepare global map of NH4+ and NO3- application in synthetic nitrogen fertilizer
# source: https://doi.pangaea.de/10.1594/PANGAEA.861203
# units: kg N /ha cropland (50 years since 1960, take the last one)
if(FALSE){

  # load total N fertilizer dose (kg N / ha of gri area) and take the latest year
  nfert = brick('data/TN_input_1961_2010.nc4')
  nfert = nfert[[50]]

  # retreive values for new raster object
  nfert <- resample(nfert,climzone,method='bilinear')

  # save the file to products directory
  saveRDS(nfert,file='data/nfert83km.rds')

  nfert = brick('data/NO3_input_ver1.nc4')

} else {

  # read the earlier prepared file with fertilizer N dose
  nfert <- readRDS('data/nfert83km.rds')
}

# prepare global map of synthetic nitrogen fertilizer
# https://journals.ametsoc.org/ei/article/14/2/1/427/Characterizing-the-Spatial-Patterns-of-Global
# https://sedac.ciesin.columbia.edu/data/set/ferman-v1-nitrogen-fertilizer-application/maps
if(FALSE){

  # load total N fertilizer dose (kg N / ha grid cel) and take the latest year
  nfert2 = raster('data/nfertilizer_global.tif')

  # retreive values for new raster object
  nfert2 <- resample(nfert2,climzone,method='bilinear')

  # estimate area
  nfert2.area <- area(nfert2)

  # save the file to products directory
  saveRDS(nfert2,file='data/nfert83km_v2.rds')
  saveRDS(nfert2.area,file='data/nfert_area83km_v2.rds')

} else {

  # read the earlier prepared file with fertilizer N dose
  nfert2 <- readRDS('data/nfert83km_v2.rds')
  nfert2.area <- readRDS('data/nfert_area83km_v2.rds')
}

# prepare soil organic carbon content (permille)
# source: https://files.isric.org/soilgrids/data/recent/
if(FALSE){

  # load the raster with SOC values
  soc <- raster('data/ORCDRC_M_sl1_250m_ll.tif')

  # change resolution
  soc <- aggregate(soc, fact = 40, fun = mean)

  # resample to lower resolution
  soc <- resample(soc,climzone,method='bilinear')

  # save the file to products directory
  writeRaster(soc,file='data/soc83km.tif',format="GTiff", overwrite=TRUE)


} else {

  # read the earlier prepared file with fertilizer N dose
  soc <- raster('data/soc83km.tif')

}

# read in spam-fao conversion table (adapted from: https://www.mapspam.info/methodology/)
crops_id <- fread('data/spam_crop_code_fao.csv')

# read in the gaul-fao country id table
country_id <- fread('data/map_gaul_fao_countriesid.csv',na.strings = '')


# prepare CROP DATA from Spatial Production Allocation Model smap2005
# source https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/DHXBJX/I7WJQ0

# prepare raster file with grid cells per 5 minutes
if(FALSE){

  # raster file of smap crops with cell-numers
  spam.r <- raster('data/CELL5M.asc')

  # which cell numbers belong to what country
  cc.spam <- extract(x = spam.r,y = cc,df=TRUE,progress = 'text')
  cc.spam <- as.data.table(cc.spam)

  # save
  saveRDS(cc.spam,file = 'data/spam_country_id.rds')

} else {

  # load prepared spam raster file
  cc.spam <- readRDS('data/spam_country_id.rds')
}

# prepare database with crop data SPAM
if(FALSE){

  # read database with SPAM2005 properties (cropland area in ha)

  # read db with total area per crop
  cr.area <- as.data.table(foreign::read.dbf('data/spam2005V3r1_global_H_TA.dbf'))
  cr.area.sel <- colnames(cr.area)[grepl('CELL|TECH_TYPE|_A$',colnames(cr.area))]
  cr.area <- cr.area[,mget(cr.area.sel)]
  setnames(cr.area,gsub('_A$','',colnames(cr.area)))

  # read db with total area with high input irrigated systems
  cr.area.hi1 <- as.data.table(foreign::read.dbf('data/spam2005V3r1_global_H_TI.dbf'))
  cr.area.sel <- colnames(cr.area.hi1)[grepl('CELL|TECH_TYPE|_I$|_H$',colnames(cr.area.hi1))]
  cr.area.hi1 <- cr.area.hi1[,mget(cr.area.sel)]
  setnames(cr.area.hi1,gsub('_I$','',colnames(cr.area.hi1)))

  # read db with total area with high input rainfed systems
  cr.area.hi2 <- as.data.table(foreign::read.dbf('data/spam2005V3r1_global_H_TH.dbf'))
  cr.area.sel <- colnames(cr.area.hi2)[grepl('CELL|TECH_TYPE|_I$|_H$',colnames(cr.area.hi2))]
  cr.area.hi2 <- cr.area.hi2[,mget(cr.area.sel)]
  setnames(cr.area.hi2,gsub('_H$','',colnames(cr.area.hi2)))

  # combine all
  cr.area.all <- rbind(cr.area,cr.area.hi1,cr.area.hi2)

  # melt database given technology status and crop
  cr.area.all <- melt(cr.area.all,id.vars = c('CELL5M','TECH_TYPE'),variable.name = 'crop',value.name = 'crop_ha')

  # dcast database for technology status
  cr.area.all <- dcast(cr.area.all,CELL5M + crop ~ TECH_TYPE, value.var = 'crop_ha',fill = 0)
  setnames(cr.area.all,c('A','I','H'),c('ALL','TECH_I','TECH_H'))

  # add area low intensity as difference between total and irrigated and high intensity rainfed
  cr.area.all[,TECH_L := ALL - TECH_H - TECH_I]

  # save file and clear objects from memory
  saveRDS(cr.area.all,file = 'data/spam_areas_full.rds')
  rm(cr.area,cr.area.hi1,cr.area.sel,cr.area.hi2)

} else {

  # load prepared db with crop data SPAM
  cr.area.all <- readRDS('data/spam_areas_full.rds')
}


# prepare database with tillage practices
if(FALSE){

  # extract the tillage practices
  cc.till <- extract(x = tillage,y = cc,df=TRUE,progress = 'text')
  cc.till <- as.data.table(cc.till)

  # clean up columns name from cc.till
  setnames(cc.till,gsub('\\.tillage','',colnames(cc.till)))

  # melt tillage
  cc.till <- melt(cc.till,id.vars = 'ID',variable.name = 'crop',value.name = 'till.class')

  # clean up tillage db
  cc.till <- cc.till[crop != 'Scenario.Conservation.Agriculture.area']
  cc.till <- cc.till[!is.na(till.class)]

  # calculate relative fraction per country
  cc.till[,till.tot := .N, by = c('ID','crop')]
  cc.till[,till.class.fr := 1 / till.tot]

  # dcast per tillage practice
  cc.till <- dcast(cc.till,ID + crop ~ till.class, fun.aggregate = sum,value.var = 'till.class.fr')
  setnames(cc.till,c('ccID','crop',paste0('till',1:6)))

  # local db to recode crop names to SPAM
  dt.cr.name <- data.table(
    spam = c("WHEA","RICE","MAIZ","BARL","REST","OOIL","TOBA","TEAS","COCO","RCOF","ACOF","OFIB","COTT","SUGB","SUGC","OILP","VEGE",
             "TEMF","TROF","PLNT","BANA","CNUT","GROU","OTRS","CASS","YAMS","SWPO","POTA","SESA","RAPE","SUNF","SOYB","OPUL","LENT",
             "PIGE","COWP","CHIC","BEAN","OCER","SORG","SMIL","PMIL"),
    till = c('wheat','rice','maize','barley','rest','other.oilcrops','tobacco','teas','cocoa','robusta.coffee',
             'arabica.coffee','other.fibre.crops','cotton','sugarbeet','sugarcane','oilpalm','vegetables',
             'temperate.fruit','tropical.fruit','plantain','banana','coconut','groundnut','other.roots',
             'cassava','yams','sweet.potato','potato','sesameseed','rapeseed','sunflower','soybean','other.pulses',
             'lentils','pigeopea','cowpea','chicpea','bean','other.cereals','sorghum','small.millet','pearl.millet'))
  # recode names
  cc.till[,crop:=tolower(crop)]
  cc.till <- merge(cc.till,dt.cr.name,by.x ='crop',by.y='till',all.x = TRUE)

  # remove cropname
  cc.till[,crop:=NULL]

  # save file
  saveRDS(cc.till,file = 'data/tillage_extracted_area_full.rds')

} else {

  # read file
  cc.till <- readRDS('data/tillage_extracted_area_full.rds')
}

# prepare N input via manure and fertilizer N on cropland
if(FALSE){

  # add nitrogen inputs to one rasterstack (manure in kg/km2 and fertilizer in kg/ha)
  inp.num <- stack(nmanprod,nman,nman_gld_app,nman_gld_dep,nman_nlos,nfert2)

  # what is grid area (in km2)
  inp.num.area <- area(inp.num)

  # multiply by area to get N input in Mg = tonnes
  inp.num <- inp.num * inp.num.area / 1000

  # add names
  names(inp.num) <- c('ndm_prod','ndm_cropland','ndm_grassland','ndm_grazing','ndm_loss','nfert')

  # check totals
  glob_nprod <- sum(getValues(inp.num$ndm_prod),na.rm=T)/1000000 # 136 Tg ipv 131 Tg
  glob_cropl <- sum(getValues(inp.num$ndm_cropland),na.rm=T)/1000000 # 24.63 Tg ipv 24.5 Tg N
  glob_gld <- sum(getValues(inp.num$ndm_grassland),na.rm=T)/1000000 # 8.50 Tg N
  glob_graz <- sum(getValues(inp.num$ndm_grazing),na.rm=T)/1000000 # 83.2 Tg N
  glob_potloss <- sum(getValues(inp.num$ndm_loss),na.rm=T)/1000000 # 7.90 Tg N

  # adjust the fertilizer input
  inp.num$nfert <- inp.num$nfert * 100

  # extract nitrogen input per country-climate zone (was mean, now sum) in tonnes
  cc.man <- extract(x = inp.num,y = cc,df=TRUE,fun = sum,na.rm=TRUE,progress = 'text')
  cc.man <- as.data.table(cc.man)
  setnames(cc.man,'ID','ccID')

  # replace NA with zero
  cc.man[is.na(cc.man)] <- 0

  # round to 1 decimal
  cols <- colnames(cc.man)[-1]
  cc.man[,c(cols) := lapply(.SD,function(x) round(x,1)),.SDcols=cols]

  # add nloss 2
  cc.man[,ndm_loss2 := ndm_prod - ndm_grassland - ndm_cropland/0.75 - ndm_grazing]

  # save file manure and fertilizer dose
  saveRDS(cc.man,file='data/ngift_extracted_full.rds')

} else {

  # load object with n dose manure and fertilizer
  cc.man <- readRDS('data/ngift_extracted_full.rds')

}

# prepare object with organic carbon levels in soil
if(FALSE){

  # extract soc (ppm) per country-climate zone
  cc.soc <- extract(x = soc,y = cc, df = TRUE,fun=mean,na.rm=TRUE,progress = 'text')
  cc.soc <- as.data.table(cc.soc)
  setnames(cc.soc,c('ccID','SOC'))

  # save file soc
  saveRDS(cc.soc,file='data/soc_extracted_full.rds')

} else {

  # load SOC object
  cc.soc <- readRDS('data/soc_extracted_full.rds')
}

# prepare database met FAOstat data
if(FALSE){

  # read in the csv file (source: FAOSTAT, http://www.fao.org/faostat/en/#data/QC/visualize)
  cr1 <- fread('data/Production_Crops_E_All_Data.csv')

  # select the data columns to be used
  cr.cols <- c('Area Code','Area','Item Code','Item','Element','Unit','Y2018')

  # subset, filter and rename the FAO db
  cr2 <- cr1[,mget(cr.cols)][Element %in% c('Area harvested','Production')]
  setnames(cr2,c('Item Code','Item','Area','Area Code'),c('crop_code','crop_name','country','country_code'))

  # save file
  fwrite(cr2,'data/production_Crops_E_All_Data_2018.csv')

} else {

  # save the faostat data
  cr2 <- fread('data/production_Crops_E_All_Data_2018.csv')
}


# read and summarize FAO data crop residues per crop per country (kg N per crop per year)
if(FALSE){

  # select the data columns to be used for year 2017
  cr.cols <- c('Area Code','Area','Item Code','Item','Element','Unit','Y2017')

  # read in the csv file (source: http://www.fao.org/faostat/en/#data/GA/visualize)
  cr1 <- fread('D:/ROSG/0000.N.09 Samenwerking WDV/data/fao/Emissions_Agriculture_Crop_Residues_E_All_Data_NOFLAG.csv')

  # subset, filter and rename the FAO db
  cr1 <- cr1[,mget(cr.cols)][Element=='Residues (Crop residues)' & Area != 'World' & Item != 'All Crops']
  setnames(cr1,c('Item Code','Item','Area','Area Code','Y2017'),c('crop_code','crop_name','country','country_code','fao_cr_resN'),skip_absent=TRUE)

  # save file
  fwrite(cr1,'data/emissions_agriculture_crop_residues_e_all_data_NOFLAG_2017.csv')

} else {

  # save the faostat data
  cr1 <- fread('data/emissions_agriculture_crop_residues_e_all_data_NOFLAG_2017.csv')
}

# read and summarize FAO data crop residues burned per crop per country (kg DM per crop per year)
if(FALSE){

  # select the data columns to be used for year 2017
  cr.cols <- c('Area Code','Area','Item Code','Item','Element','Unit','Y2017')

  # read in the csv file (source:http://www.fao.org/faostat/en/#data/GB/visualize)
  cr1 <- fread('D:/ROSG/0000.N.09 Samenwerking WDV/data/fao/Emissions_Agriculture_Burning_crop_residues_E_All_Data_NOFLAG.csv')

  # subset, filter and rename the FAO db
  cr1 <- cr1[,mget(cr.cols)][Element=='Biomass burned (dry matter)']
  setnames(cr1,c('Item Code','Item','Area','Area Code','Y2017'),c('crop_code','crop_name','country','country_code','fao_cr_burnedDM'))

  # save file
  fwrite(cr1,'data/emissions_agriculture_burning_crop_residues_e_all_data_NOFLAG_2017.csv')

} else {

  # load the earlier saved faostat data
  cr1 <- fread('data/emissions_agriculture_burning_crop_residues_e_all_data_NOFLAG_2017.csv')
}
