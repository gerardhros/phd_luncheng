# Figure 5

  # require packages
  require(terra)
  require(data.table)
  require(metafor)

  # empty environment
  rm(list=ls())

  # load in cropland area Copernicus (not used)
  if(FALSE){

    # source: https://www.esa-landcover-cci.org/?q=node/164)
    cropland <- terra::rast('data/cropland83km_v2.tif')

    cropland.dt <- as.data.table(cropland)
    setnames(cropland.dt,'cci_croptype')

    # describe crops
    cropland.dt[cci_croptype == 10, ctype := 'rainfed_cropland']
    cropland.dt[cci_croptype == 20, ctype := 'irrigated_cropland']
    cropland.dt[cci_croptype == 30, ctype := 'mosaic_cropland']
    cropland.dt[cci_croptype == 40, ctype := 'natural_vegetation']
  }

  # load crop area harvested with source SPAM (not used, raster is preferred)
  if(FALSE){

    # read db with total area per crop
    cr.area <- as.data.table(foreign::read.dbf('D:/DATA/04 crop/spam2010/spam2010V2r0_global_H_TA.DBF'))
    cr.area.sel <- colnames(cr.area)[grepl('CELL|TECH_TYPE|_A$',colnames(cr.area))]
    cr.area <- cr.area[,mget(cr.area.sel)]
    setnames(cr.area,gsub('_A$','',colnames(cr.area)))

    # melt crop database
    cr.area <- melt(cr.area,
                    id.vars = c('CELL5M','TECH_TYPE'),
                    variable.name = 'cropname',
                    value.name = 'area')

    # rename crops needed for MA models
    cr.area[,cropname := as.character(cropname)]
    cr.area[grepl('WHEA|MIL|BAR',cropname), ma_crop := 'wheat']
    cr.area[cropname == 'MAIZ', ma_crop := 'maize']
    cr.area[cropname == 'RICE', ma_crop := 'rice']
    cr.area[cropname == 'VEG', ma_crop := 'vegetable']
    cr.area[is.na(ma_crop), ma_crop := 'other']

    # sum per crop category
    cr.area <- cr.area[,list(area = sum(area)), by = c('CELL5M','ma_crop')]

    # save file and clear objects from memory
    saveRDS(cr.area,file = 'D:/DATA/04 crop/products/spam_areas_cropcategory.rds')
    rm(cr.area)

  } else {

    # load prepared db with crop data SPAM
    # cr.area <- as.data.table(readRDS('D:/DATA/04 crop/products/spam_areas_full.rds'))
  }

  # load climatic data
  if(FALSE){

    # downloaded via CRU, https://catalogue.ceda.ac.uk/uuid/89e1e34ec3554dc98594a5732622bce9
    rfiles <- list.files('D:/DATA/02 climate',pattern = 'tif|nc',full.names = T)
    rfiles <- rfiles[grepl('pre|tmp',rfiles)]
    clim <- terra::sds(rfiles)
    clim <- terra::rast(clim)

    clim.dt <- as.data.frame(clim,xy=TRUE)
    clim.dt <- as.data.table(clim.dt)

    # rearrange data
    d2.clim <- melt(clim.dt,id.vars = c('x','y'), variable.name = 'variable')
    d2.clim <- d2.clim[!grepl('_stn_',variable)]
    d2.clim[, cvar :=  stringr::str_extract_all(variable,"(?<=[0-9]{4}\\.[0-9]{4}\\.).+(?=\\.dat_)",simplify = T)]
    d2.clim[, years :=  stringr::str_extract_all(variable,"[0-9]{4}\\.[0-9]{4}",simplify = T)]
    d2.clim[, month :=  stringr::str_extract_all(variable,"(?<=[a-z]{3}_)\\d+",simplify = T)]

    # estimate mean global climate properties over 1991-2019
    # temperature in degrees (mean = tmp) and precipitation (pre) in mm/month
    d3.clim <- dcast(d2.clim,x+y+years+month~cvar,value.var = 'value')
    d3.clim[,month := as.numeric(month)]
    d3.clim[, year := floor((month - 1)/ 12) + as.numeric(substr(years,1,4))]

    # derive the mean and SD per gridcel over period 1991-2019
    d4.clim <- d3.clim[,list(mat = mean(tmp),pre = sum(pre)),by=c('x','y','year')]
    d4.clim <- d4.clim[,list(mat = mean(mat),pre = mean(pre)),by=c('x','y')]

    # set back to raster
    r.clim <- terra::rast(d4.clim,type='xyz')
    terra::crs(r.clim) <- 'epsg:4326'

    # write raster with MAT and MAP to disk
    terra::writeRaster(r.clim,'data/climate.tif', overwrite = TRUE)

  } else {

    # read the raster with climatic data
    r.clim <- terra::rast('data/climate.tif')

  }


  # load SPAM raster
  # spam <- as.data.table(foreign::read.dbf('D:/DATA/04 crop/spam2010/cell5m_allockey_xy.dbf'))

  # load crop area harvested as raster
  if(FALSE){

    # get the file names of the tiffs
    rfiles <- list.files('D:/DATA/04 crop/spam2010', pattern = '_H.tif$',full.names = TRUE)

    # read in all files and convert to spatrasters
    r.crop <- sds(rfiles)
    r.crop <- rast(r.crop)

    # aggregate to 0.5 x 0.5 degree
    r.crop <- terra::aggregate(r.crop,fact = 0.5/0.083333,fun = "sum", na.rm=T)

    # adjust names
    names(r.crop) <- stringr::str_extract(names(r.crop),'[A-Z]{4}')

    # crop names to combine in other
    scrop <- c('RICE','MAIZ')
    wheat <- c('WHEA','BARL','SMIL','PMIL')
    other <- names(r.crop)[!names(r.crop) %in% c(scrop,wheat)]

    # sum all other crops
    r.crop.other = app(r.crop[[other]],fun = sum,na.rm=T)
    names(r.crop.other) <- 'other'
    r.crop.wheat = app(r.crop[[wheat]],fun = sum,na.rm=T)
    names(r.crop.wheat) <- 'wheat'

    # combine again
    r.crop <- c(r.crop[[scrop]],r.crop.other,r.crop.wheat)

    # reproject to r.clim
    r.crop <- resample(r.crop,r.clim,method='bilinear')

    # write raster
    terra::writeRaster(r.crop,'data/ma_crops.tif', overwrite = TRUE)


  } else {

    # read the raster with cropping data
    r.crop <- rast('data/ma_crops.tif')

  }

  # load the relevant soil properties
  if(FALSE){

    # load in ph and clay as raster, WGS84, 4326, 0.5 x 0.5 degrees
    ph <- terra::rast('D:/DATA/01 soil/isric_phw_mean_0_5.tif')
    clay <- terra::rast('D:/DATA/01 soil/isric_clay_mean_0_5.tif')
    soc <-  terra::rast('D:/DATA/01 soil/isric_soc_mean_0_5.tif')
    soc <- terra::resample(soc,clay)

    # combine both in one raster
    r.soil <- c(ph,clay,soc)

    # reproject to r.clim
    r.soil <- terra::resample(r.soil,r.clim,method='bilinear')

    # write raster
    terra::writeRaster(r.soil,'data/soil.tif', overwrite = TRUE)

  } else {

    # read the raster with soil data
    r.soil <- terra::rast('data/soil.tif')
  }


  # prepare raster with tillage practices
  # source: https://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:4085895
  # https://datapub.gfz-potsdam.de/download/10.5880.PIK.2019.011/
  if(FALSE){

    # read in the rasters from nc4 file
    tillage <- terra::rast('D:/DATA/05 tillage/tillage_revised.nc4')
    terra::crs(tillage) <- 'epsg:4326'

    # tillage categories
    # 1 = Conventional annual tillage
    # 2 = Traditional annual tillage
    # 3 = Reduced tillage
    # 4 = Conservation Agriculture
    # 5 = Rotational tillage
    # 6 = Traditional rotational tillage
    # 7 = Scenario Conservation Agriculture area

    # change names
    names(tillage) <- c("WHEA","RICE","MAIZ","BARL","REST","OOIL","TOBA","TEAS","COCO","RCOF","ACOF","OFIB","COTT","SUGB","SUGC","OILP","VEGE",
                        "TEMF","TROF","PLNT","BANA","CNUT","GROU","OTRS","CASS","YAMS","SWPO","POTA","SESA","RAPE","SUNF","SOYB","OPUL","LENT",
                        "PIGE","COWP","CHIC","BEAN","OCER","SORG","SMIL","PMIL","scenario_ca_area")

    # crop names to combine in other
    scrop <- c('RICE','MAIZ')
    wheat <- c('WHEA','BARL','SMIL','PMIL')
    other <- names(tillage)[!names(tillage) %in% c(scrop,wheat)]

    # select the most frequent tillage pracice for all other crops
    r.till.other = app(tillage[[other]],fun = modal,na.rm=T)
    names(r.till.other) <- 'other'
    r.till.wheat = app(tillage[[wheat]],fun = modal,na.rm=T)
    names(r.till.wheat) <- 'wheat'

    # combine again
    r.till <- c(tillage[[scrop]],r.till.other,r.till.wheat)

    # reproject to r.clim
    r.till <- resample(r.till,r.clim,method='near')

    # write raster with tillage to disk
    terra::writeRaster(r.till,'data/tillage.tif', overwrite = TRUE)

  } else {

    # read the earlier prepared file with tillage practices
    r.till <- terra::rast('data/tillage.tif')

  }

  # prepare manure N dose on cropland (take latest year present)
  # source: https://doi.pangaea.de/10.1594/PANGAEA.871980
  # units: kg N / km2 / yr
  if(FALSE){

    # read the file with manure application data (kg N / km2 grid cell)
    nman <- data.table::fread('D:/DATA/06 inputs/DN_yy2010.txt')
    nman <- terra::rast(nrows=2124,ncols=4320,xmin=-180,ymin=-88.5,crs = "+proj=longlat +datum=WGS84",
                        vals=as.matrix(nman))

    # retreive values for new raster object
    r.nman <- terra::resample(nman,r.clim[[1]],method='bilinear')

    names(r.nman) <- 'nofert'

    # write raster with manure N dose to cropland to disk
    terra::writeRaster(r.nman,'data/nofert.tif', overwrite = TRUE)

  } else {

    # read the earlier prepared file with manure N dose
    r.nman <- terra::rast('data/nofert.tif')
  }

  # fertilizer N dose, NH4 and NO3, 0.5 x 0.5 resolution, 1961-2010
  # https://doi.pangaea.de/10.1594/PANGAEA.861203
  # units: kg N /ha cropland (50 years since 1960, take the last one)
  if(FALSE){

    # load total N fertilizer dose (kg N / ha of gri area) and take the latest year
    nfert_no3 = terra::rast('D:/DATA/06 inputs/NO3_input_ver1.nc4')
    nfert_no3 = nfert_no3[[589:600]]
    nfert_nh4 = terra::rast('D:/DATA/06 inputs/NH4_input_ver1.nc4')
    nfert_nh4 = nfert_nh4[[589:600]]

    # sum the monthly values
    nfert_nh4 = app(nfert_nh4,fun = sum,na.rm=T)
    nfert_no3 = app(nfert_no3,fun = sum,na.rm=T)
    names(nfert_nh4) <- 'nfert_nh4'
    names(nfert_no3) <- 'nfert_no3'

    # add both together
    r.nfert <- c(nfert_nh4,nfert_no3)

    # retreive values for new raster object
    r.nfert <- terra::resample(r.nfert,r.clim,method='bilinear')

    # write raster with inorganic N fertilization to disk
    terra::writeRaster(r.nfert,'data/nifert.tif', overwrite = TRUE)


  } else {

    # read the earlier prepared file with fertilizer N dose
    r.nfert <- terra::rast('data/nifert.tif')
  }

# read in crop rotation intensity derived from MODIS (from Liu et al 2021)
# works for arable systems, but not so well for grassland (manual check GR)
# https://doi.org/10.6084/m9.figshare.14099402
  if(FALSE){

    # read in the raster from year 2010
    r1 <- terra::rast('D:/DATA/04 crop/rotation/GCI_QC_2010.tif')

    # cropping intensity of 0 is NA
    r1[r1==0] <- NA

    # lower resolution to 0.5x0.5 degree
    r1.cropint <- terra::aggregate(r1,fact = 0.5/0.002209708,fun='mean',na.rm=TRUE)

    # resample to similar format as other files
    r1.cropint <-  terra::resample(r1.cropint,r.clim,method='near')

    # apply a round function
    r1.cropint <- app(r1.cropint,fun = round)
    names(r1.cropint) <- 'cropintensity'

    # write raster with inorganic N fertilization to disk
    terra::writeRaster(r1.cropint,'data/cropintensity.tif', overwrite = TRUE)

  } else {

    # read the earlier prepared file with cropping intensity
    r1.cropint <- terra::rast('data/cropintensity.tif')
  }


#############################################################################################################
  # require packages
  require(terra)
  require(data.table)
  require(metafor)

  # Luncheng add
  
  # read the raster with climatic data 
  r.clim <- terra::rast('C:/Users/86188/Desktop/Figures/upscaling/climate.tif')
  
  # read the raster with cropping data
  r.crop <- rast('C:/Users/86188/Desktop/Figures/upscaling/ma_crops.tif')
  
  # read the raster with soil data
  r.soil <- terra::rast('C:/Users/86188/Desktop/Figures/upscaling/soil.tif')
  
  # read the earlier prepared file with tillage practices
  r.till <- terra::rast('C:/Users/86188/Desktop/Figures/upscaling/tillage.tif')
  
  # read the earlier prepared file with manure N dose
  r.nman <- terra::rast('C:/Users/86188/Desktop/Figures/upscaling/nofert.tif')
  
  # read the earlier prepared file with fertilizer N dose
  r.nfert <- terra::rast('C:/Users/86188/Desktop/Figures/upscaling/nifert.tif')
  
  # read the earlier prepared file with cropping intensity
  r.cropint <- terra::rast('C:/Users/86188/Desktop/Figures/upscaling/cropintensity.tif')
  
#########################################################################
# add all rasters

    # clear environment
    rm(list= ls())

    # what rasters are in data
    rfiles <- list.files('C:/Users/86188/Desktop/Figures/upscaling', pattern = 'tif$',full.names = TRUE)
    rfiles <- rfiles[!grepl('cropland',rfiles)]

    # read in raster files
    r.ma <- terra::sds(rfiles)

    # convert to raster
    r.ma <- terra::rast(r.ma)

# convert rasters to data.table

    # set first to xy data.frame (NA=FALSE otherwise gridcels are removed) 
    r.df <- as.data.frame(r.ma,xy = TRUE, na.rm = FALSE)

    # convert to data.table
    r.dt <- as.data.table(r.df)

    # setnames
    setnames(r.dt,old = c('climate_mat', 'climate_pre','soil_isric_phw_mean_0_5','soil_isric_clay_mean_0_5','soil_isric_soc_mean_0_5',
                          'nifert_nfert_nh4','nifert_nfert_no3','nofert_nofert','cropintensity_cropintensity'),
             new = c('mat','pre','phw','clay','soc','nh4','no3','nam','cropintensity'),skip_absent = T)

    # select only land area
    r.dt <- r.dt[!(is.na(mat)|is.na(pre))]
    r.dt <- r.dt[!(is.na(tillage_RICE) & is.na(tillage_MAIZ) & is.na(tillage_other) & is.na(tillage_wheat))]
    r.dt <- r.dt[!(is.na(ma_crops_RICE) & is.na(ma_crops_MAIZ) & is.na(ma_crops_other) & is.na(ma_crops_wheat))]

    # replace area with 0 when missing
    cols <- colnames(r.dt)[grepl('^ma_|nh4|no3|nam',colnames(r.dt))]
    r.dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = cols]
    cols <- colnames(r.dt)[grepl('^tillage',colnames(r.dt))]
    r.dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),1,x)),.SDcols = cols]
    r.dt[is.na(cropintensity), cropintensity := 1]

    # melt the data.table
    r.dt.melt <- melt(r.dt,
                      id.vars = c('x','y','mat', 'pre','phw','clay','nh4','no3','nam','soc','cropintensity'),
                      measure=patterns(area="^ma_crops", tillage ="^tillage_"),
                      variable.factor = FALSE,
                      variable.name = 'croptype')

    # set the crop names (be aware, its the order in ma_crops)
    r.dt.melt[,cropname := c('rice','maize','other','wheat')[as.numeric(croptype)]]

    # set names to tillage practices 
    r.dt.melt[, till_name := 'conventional']
    r.dt.melt[tillage %in% c(3,4,7), till_name := 'no-till']

# derive the meta-analytical model

    # read data  
    d1 <- readxl::read_xlsx('C:/Users/86188/Desktop/Source Data.xlsx',sheet = "Figure5")
    d1 <- as.data.table(d1)

    
    # add CV for NUE treatment and estimate the SD for missing ones 
    d2<-d1
    CV_nuet_bar<-mean(d2$nuet_sd[is.na(d2$nuet_sd)==FALSE]/d2$nuet_mean[is.na(d2$nuet_sd)==FALSE])
    d2$nuet_sd[is.na(d2$nuet_sd)==TRUE]<-d2$nuet_mean[is.na(d2$nuet_sd)==TRUE]*1.25*CV_nuet_bar
    
    CV_nuec_bar<-mean(d2$nuec_sd[is.na(d2$nuec_sd)==FALSE]/d2$nuec_mean[is.na(d2$nuec_sd)==FALSE])
    d2$nuec_sd[is.na(d2$nuec_sd)==TRUE]<-d2$nuec_mean[is.na(d2$nuec_sd)==TRUE]*1.25*CV_nuec_bar
    
    
    # clean up column names
    setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
    setnames(d2,tolower(colnames(d2)))
    

    # calculate effect size (NUE)
    es21 <- escalc(measure = "MD", data = d2,
                   m1i = nuet_mean, sd1i = nuet_sd, n1i = replication,
                   m2i = nuec_mean, sd2i = nuec_sd, n2i = replication )

    # convert to data.tables
    d02 <- as.data.table(es21)

    # what are the treatments to be assessed
    d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))

    # what are labels
    d02.treat[treatment=='ALL',desc := 'All']
    d02.treat[treatment=='EE',desc := 'Enhanced Efficiency']
    d02.treat[treatment=='CF',desc := 'Combined fertilizer']
    d02.treat[treatment=='RES',desc := 'Residue retention']
    d02.treat[treatment=='RFP',desc := 'Fertilizer placement']
    d02.treat[treatment=='RFR',desc := 'Fertilizer rate']
    d02.treat[treatment=='ROT',desc := 'Crop rotation']
    d02.treat[treatment=='RFT',desc := 'Fertilizer timing']
    d02.treat[treatment=='OF',desc := 'Organic fertilizer']
    d02.treat[treatment=='RT',desc := 'Reduced tillage']
    d02.treat[treatment=='NT',desc := 'No tillage']
    d02.treat[treatment=='CC',desc := 'Crop cover']

    
    # update the missing values for n_dose and p2o5_dose (as example)
    d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]
   
    # scale the variables to unit variance
    d02[,clay_scaled := scale(clay)]
    d02[,soc_scaled := scale(soc)]
    d02[,ph_scaled := scale(ph)]
    d02[,mat_scaled := scale(mat)]
    d02[,map_scaled := scale(map)]
    d02[,n_dose_scaled := scale(n_dose)]
    
    # update the database (it looks like typos)
    d02[g_crop_type=='marize', g_crop_type := 'maize']

    
    #Combining different factors

    d02[tillage=='reduced', tillage := 'no-till']

    # # Combining different factors
    
    d02[,fertilizer_type := factor(fertilizer_type,
                                  levels = c('mineral','organic', 'combined','enhanced'))]
    d02[,fertilizer_strategy := factor(fertilizer_strategy,
                                      levels = c("conventional", "placement","rate","timing"))]
    d02[,g_crop_type := factor(g_crop_type,
                              levels = c('maize','wheat','rice'))]
    
    d02[,rfp := fifelse(fertilizer_strategy=='placement','yes','no')]
    d02[,rft := fifelse(fertilizer_strategy=='timing','yes','no')]
    d02[,rfr := fifelse(fertilizer_strategy=='rate','yes','no')]
    d02[,ctm := fifelse(g_crop_type=='maize','yes','no')]
    d02[,ctw := fifelse(g_crop_type=='wheat','yes','no')]
    d02[,ctr := fifelse(g_crop_type=='rice','yes','no')]
    #d02[,cto := fifelse(g_crop_type=='other','yes','no')]
    d02[,ndose2 := scale(n_dose^2)]
    

    # make metafor model
    
    m1 <- rma.mv(yi,vi,
                       mods = ~fertilizer_type + rfp + rft + rfr + crop_residue + tillage +
                         cover_crop_and_crop_rotation + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled + soc_scaled +
                         n_dose_scaled:soc_scaled + ctm:rfp + ctm + ctw + ctr + ctm:mat_scaled  + ndose2 -1,
                       data = d02,
                       random = list(~ 1|studyid), method="REML",sparse = TRUE)

    # see model structure that need to be filled in to predict NUE as function of the system properties 
    p1 <- predict(m1,addx=T)

    # this is the order of input variables needed for model predictions (=newmods in predict function) 
    m1.cols <- colnames(p1$X)

    # make prediction dataset for situation that soil is fertilized by both organic and inorganic fertilizers, conventional fertilizer strategy 
    dt.new <- copy(r.dt.melt)

    # add the columns required for the ma model, baseline scenario 
    # baseline is here defined as "strategy conventional", and mineral fertilizers, no biochar, no crop residue, no cover crops 
    dt.new[, fertilizer_typeenhanced := 0]
    dt.new[, fertilizer_typemineral := 1]
    dt.new[, fertilizer_typeorganic := 0]
    dt.new[, fertilizer_typecombined := 0]
    dt.new[, rfpyes := 0]
    dt.new[, rftyes := 0]
    dt.new[, rfryes := 0]
    dt.new[, crop_residueyes := 0]
    dt.new[, cover_crop_and_crop_rotationyes := 0]
    dt.new[, cover_crop_and_crop_rotationyes := fifelse(cropintensity>1,1,0)]
    dt.new[, `tillageno-till` := fifelse(till_name =='no-till',1,0)]
    #dt.new[,`tillageno-till` := 0]
    dt.new[, ctryes := fifelse(cropname=='rice',1,0)]
    dt.new[, ctwyes := fifelse(cropname=='wheat',1,0)]
    dt.new[, ctmyes := fifelse(cropname=='maize',1,0)]
    dt.new[, ph_scaled := (phw * 0.1 - mean(d02$ph)) / sd(d02$ph)]
    dt.new[, clay_scaled := (clay * 0.1 - mean(d02$clay)) / sd(d02$clay)]
    dt.new[, soc_scaled := (soc * 0.1 - mean(d02$soc)) / sd(d02$soc)]
    dt.new[, n_dose_scaled := scale(nh4+no3+nam)]
    dt.new[, ndose2 := scale((nh4+no3+nam)^2)]
    dt.new[, map_scaled := (pre - mean(d02$map)) / sd(d02$map)]
    dt.new[, mat_scaled := (mat  - mean(d02$mat)) / sd(d02$mat)]
    dt.new[, `n_dose_scaled:soc_scaled` := n_dose_scaled*soc_scaled]
    dt.new[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.new[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    # convert to matrix, needed for rma models 
    dt.newmod <- as.matrix(dt.new[,mget(c(m1.cols))])

    # predict the NUE via MD model
    dt.pred <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))

    # add predictions to the data.table
    cols <- c('pMDmean','pMDse','pMDcil','pMDciu','pMDpil','pMDpiu')
    dt.new[,c(cols) := dt.pred]

    
    ####################################################################################################################################################    
    ############################################## scenario 1 (Nutrient management) ################################################################
    # scenario 1. the combination of measures with change in RFR, RFT and BC. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)
    
    # make local copy 
    dt.s1 <- copy(dt.new)
    
    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]
    
    # update actions taken for scenario 1 
    dt.s1[, fertilizer_typeenhanced := 1]
    dt.s1[, fertilizer_typemineral := 0]
    dt.s1[, fertilizer_typeorganic := 1]
    dt.s1[, fertilizer_typecombined := 1]
    dt.s1[, rfpyes := 1]
    dt.s1[, rftyes := 1]
    dt.s1[, rfryes := 1]
    dt.s1[, crop_residueyes := 0]
    dt.s1[, cover_crop_and_crop_rotationyes := 0]
    dt.s1[, tillageno_till := 0]
    dt.s1[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s1[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s1[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s1[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]
    
    # convert to matrix, needed for rma models 
    dt.newmod <- as.matrix(dt.s1[,mget(c(m1.cols))])
    
    # predict the NUE via MD model 
    dt.pred.s1 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s1[,c(cols) := dt.pred.s1]
    
    # compare baseline with scenario 
    
    # select relevant columns of the baseline 
    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]
    
    # select relevant columns of scenario 1 and merge 
    dt.fin <- merge(dt.fin,dt.s1[,.(x,y,s1 = pMDmean,cropname)],by=c('x','y','cropname'))
    
    # estimate relative improvement via senario 1 
    dt.fin[, improvement := s1 - base]
    
    # estimate area weighted mean relative improvement 
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]
    
    
    # make spatial raster of the estimated improvement 
    
    # convert to spatial raster 
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'C:/Users/86188/Desktop/Figures/scenario_1.tif', overwrite = TRUE)
    
    
    ############################################## scenario 2 (crop management)################################################################
    # scenario 2. the combination of measures with change in RES, CC, ROT (Optimized crop management vs. Conventional crop management)
    
    # make local copy 
    dt.s2 <- copy(dt.new)
    
    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]
    
    # update actions taken for scenario 3 
    dt.s2[, fertilizer_typeenhanced := 0]
    dt.s2[, fertilizer_typemineral := 1]
    dt.s2[, fertilizer_typeorganic := 0]
    dt.s2[, fertilizer_typecombined := 0]
    dt.s2[, rfpyes := 0]
    dt.s2[, rftyes := 0]
    dt.s2[, rfryes := 0]
    dt.s2[, crop_residueyes := 1]
    dt.s2[, cover_crop_and_crop_rotationyes := 1]
    dt.s2[, tillageno_till := 0]
    dt.s2[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s2[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s2[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s2[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]
    
    
    # convert to matrix, needed for rma models 
    dt.newmod <- as.matrix(dt.s2[,mget(c(m1.cols))])
    
    # predict the NUE via MD model 
    dt.pred.s2 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s2[,c(cols) := dt.pred.s2]
    
    # compare baseline with scenario 
    
    # select relevant columns of the baseline 
    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]
    
    # select relevant columns of scenario 1 and merge 
    dt.fin <- merge(dt.fin,dt.s2[,.(x,y,s2 = pMDmean,cropname)],by=c('x','y','cropname'))
    
    # estimate relative improvement via senario 1 
    dt.fin[, improvement := s2 - base]
    
    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]
    
    
    # make spatial raster of the estimated improvement 
    
    # convert to spatial raster 
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'C:/Users/86188/Desktop/Figures/scenario_2.tif', overwrite = TRUE)
    
    
    ############################################## scenario 3 (NT/RT) ################################################################
    # scenario 3. the combination of measures with change in NT/RT. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)
    
    # make local copy 
    dt.s3 <- copy(dt.new)
    
    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]
    
    # update actions taken for scenario 1 
    dt.s3[, fertilizer_typeenhanced := 0]
    dt.s3[, fertilizer_typemineral := 1]
    dt.s3[, fertilizer_typeorganic := 0]
    dt.s3[, fertilizer_typecombined := 0]
    dt.s3[, rfpyes := 0]
    dt.s3[, rftyes := 0]
    dt.s3[, rfryes := 0]
    dt.s3[, crop_residueyes := 0]
    dt.s3[, cover_crop_and_crop_rotationyes := 0]
    dt.s3[, `tillageno-till` := 1]
    dt.s3[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s3[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s3[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s3[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]
    
    # convert to matrix, needed for rma models 
    dt.newmod <- as.matrix(dt.s3[,mget(c(m1.cols))])
    
    # predict the NUE via MD model 
    dt.pred.s3 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s3[,c(cols) := dt.pred.s3]
    
    # compare baseline with scenario 
    
    # select relevant columns of the baseline 
    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]
    
    # select relevant columns of scenario 1 and merge 
    dt.fin <- merge(dt.fin,dt.s3[,.(x,y,s3 = pMDmean,cropname)],by=c('x','y','cropname'))
    
    # estimate relative improvement via senario 1 
    dt.fin[, improvement := s3 - base]
    
    # estimate area weighted mean relative improvement 
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]
    
    
    # make spatial raster of the estimated improvement 
    
    # convert to spatial raster 
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'C:/Users/86188/Desktop/Figures/scenario_3.tif', overwrite = TRUE)
    
    
    
###############################################################################################################################
    # plotting
    
    library(ggplot2)
    library(sf)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(terra)
    library(cowplot)
    library(vcd)
    
   
    
    ######################################### scenario_1 (optimal nutrient management) ##########################################################
    # set theme
    theme_set(theme_bw())
    
    # get the raster to plot
    r1 <- terra::rast('C:/Users/86188/Desktop/Figures/scenario_1.tif')
    
    # convert to data.frame
    r1.p <- as.data.frame(r1,xy=TRUE)
    # Exclude outliers greater than 70%
    r1.p <- r1.p[r1.p$improvement <70,]
    #r1.p.mean <- paste0(round(mean(r1.p$improvement)),'%')
    
    
    # get base world map
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    #plot a basic world map plot
    p1 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r1.p,aes(x=x,y=y, name ='none',
                                fill = cut(improvement,breaks= c(15,25,30,35,800),
                                           labels = c('< 25','25-30','30-35','> 35') ))) +
      
      # scale_fill_gradientn(colours = rainbow(3)) +
      #scale_fill_viridis_c()+ 
      theme_void() +
      theme(legend.position = c(0.05,0.4), text = element_text(size = 12),
            legend.background = element_rect(fill = NA,color = NA),
            panel.border = element_blank()) +
      labs(fill = 'NUEr increased (%)') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Optimal nutrient management") +
      theme(plot.title = element_text(size = 16))+ 
      theme(plot.title = element_text(hjust = 0.5))+
      annotate("text",x=0.5,y=-50,label="Mean: 26.9%",size=5, colour="#0070C0",fontface = "bold")+
      coord_sf(crs = 4326) 
    
    ########################################### scenario_2 (optimal crop management) ########################################################
    
    # set theme
    theme_set(theme_bw())
    
    # get the raster to plot
    r2 <- terra::rast('C:/Users/86188/Desktop/Figures/scenario_2.tif')
    
    # convert to data.frame
    r2.p <- as.data.frame(r2,xy=TRUE)
    # Exclude outliers greater than 70%
    r2.p <- r2.p[r2.p$improvement <70,]
    #r2.p.mean <- paste0(round(mean(r2.p$improvement)),'%')
    
    
    # get base world map
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # plot a basic world map plot
    p2 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r2.p,aes(x=x,y=y, name ='none',
                                fill = cut(improvement,breaks= c(0,5,10,15,800), 
                                           labels = c('< 5','5-10','10-15','> 15') ))) +
      theme_void() +
      theme(legend.position = c(0.05,0.4), text = element_text(size = 12),
            legend.background = element_rect(fill = NA,color = NA),
            panel.border = element_blank()) +
      labs(fill = 'NUEr increased (%)') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Optimal crop management") +
      theme(plot.title = element_text(size = 16))+ 
      theme(plot.title = element_text(hjust = 0.5))+
      annotate("text",x=0.5,y=-50,label="Mean: 6.6%",size=5, colour="#0070C0",fontface = "bold")+
      coord_sf(crs = 4326)
    
    ########################################### scenario_3 (optimal soil management) ########################################################
    
    # set theme
    theme_set(theme_bw())
    
    # get the raster to plot
    r3 <- terra::rast('C:/Users/86188/Desktop/Figures/scenario_3.tif')
    
    # convert to data.frame
    r3.p <- as.data.frame(r3,xy=TRUE)
    # Exclude outliers greater than 70%
    r3.p <- r3.p[r3.p$improvement <70,]
    #r3.p.mean <- paste0(round(mean(r3.p$improvement)),'%')
    
    
    # write.csv(r3.p, file="E:/phD/Papers/paper2/You_et_al_2022/NC/tillage.csv")
    
    # get base world map
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # plot a basic world map plot
    p3 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r3.p,aes(x=x,y=y, name ='none',
                                fill = cut(improvement,breaks= c(-7,0,5,10,800), 
                                           labels = c('< 0','0-5','5-10','> 10') ))) +
      theme_void() +
      theme(legend.position = c(0.05,0.4), text = element_text(size = 12),
            legend.background = element_rect(fill = NA,color = NA),
            panel.border = element_blank()) +
      labs(fill = 'NUEr increased (%)') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Optimal soil management") +
      theme(plot.title = element_text(size = 16))+ 
      theme(plot.title = element_text(hjust = 0.5))+
      annotate("text",x=0.5,y=-50,label="Mean: 0.6%",size=5, colour="#0070C0",fontface = "bold")+
      coord_sf(crs = 4326)
    
    
    #2*2
    library(ggpubr)
    p<-ggarrange(p1, p2, p3, ncol = 1, nrow = 3, #common.legend = TRUE,legend = "bottom",
                 labels = c("a", "b","c"), font.label=list(size=14),hjust = 0, vjust = 1)
    
    p
    
    ggsave(p, file = "C:/Users/86188/Desktop/Figures/Figure_5.png",width = 183,height = 247, units = "mm")
    
    