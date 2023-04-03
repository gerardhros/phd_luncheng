# upscaling results for global impacts of management on NUE based on the MD model of Luncheng

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




  
#################################################################################
  ############################################################################
#################################################################################
  require(terra)
  require(data.table)
  require(metafor)

  # Luncheng add
  
  # read the raster with climatic data 
  r.clim <- terra::rast('220629_upscaling/220621_data_for_upscaling/climate.tif')
  
  # read the raster with cropping data
  r.crop <- rast('220629_upscaling/220621_data_for_upscaling/ma_crops.tif')
  
  # read the raster with soil data
  r.soil <- terra::rast('220629_upscaling/220621_data_for_upscaling/soil.tif')
  
  # read the earlier prepared file with tillage practices
  r.till <- terra::rast('220629_upscaling/220621_data_for_upscaling/tillage.tif')
  
  # read the earlier prepared file with manure N dose
  r.nman <- terra::rast('220629_upscaling/220621_data_for_upscaling/nofert.tif')
  
  # read the earlier prepared file with fertilizer N dose
  r.nfert <- terra::rast('220629_upscaling/220621_data_for_upscaling/nifert.tif')
  
  # read the earlier prepared file with cropping intensity
  r.cropint <- terra::rast('220629_upscaling/220621_data_for_upscaling/cropintensity.tif')
  
#########################################################################
# add all rasters 

    # clear environment
    rm(list= ls())

    # what rasters are in data
    rfiles <- list.files('220629_upscaling/220621_data_for_upscaling', pattern = 'tif$',full.names = TRUE)
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
    d1 <- readxl::read_xlsx('Database.xlsx',sheet = "Figure4&S4")
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
    d02.treat[treatment=='BC',desc := 'Biochar']
 
    d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]


    # scale the variables to unit variance
    d02[,clay_scaled := scale(clay)]
    d02[,soc_scaled := scale(soc)]
    d02[,ph_scaled := scale(ph)]
    d02[,mat_scaled := scale(mat)]
    d02[,map_scaled := scale(map)]
    d02[,n_dose_scaled := scale(n_dose)]

    #Combining different factors
    d02[g_crop_type=='vegetable', g_crop_type := 'other']
    d02[tillage=='reduced', tillage := 'no-till']

    d02[,fertilizer_type := factor(fertilizer_type,
                                  levels = c('mineral','organic', 'combined','enhanced'))] 
    d02[,fertilizer_strategy := factor(fertilizer_strategy,
                                      levels = c("conventional", "placement","rate","timing"))]
    d02[,g_crop_type := factor(g_crop_type,
                              levels = c('other','maize','wheat','rice'))]
    
    d02[,rfp := fifelse(fertilizer_strategy=='placement','yes','no')]
    d02[,rft := fifelse(fertilizer_strategy=='timing','yes','no')]
    d02[,rfr := fifelse(fertilizer_strategy=='rate','yes','no')]
    d02[,ctm := fifelse(g_crop_type=='maize','yes','no')]
    d02[,ctw := fifelse(g_crop_type=='wheat','yes','no')]
    d02[,ctr := fifelse(g_crop_type=='rice','yes','no')]
    d02[,cto := fifelse(g_crop_type=='other','yes','no')]
    d02[,ndose2 := scale(n_dose^2)]

    m1 <- rma.mv(yi,vi,
                       mods = ~fertilizer_type + rfp + rft + rfr + biochar + crop_residue + tillage +
                         cover_crop_and_crop_rotation + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled +
                         n_dose_scaled:soc_scaled + ctm:rfp + ctm + ctw + ctr + cto + ctm:mat_scaled  + ndose2 -1,
                       data = d02,
                       random = list(~ 1|studyid/g_crop_type), method="REML",sparse = TRUE)

    # see model structure that need to be filled in to predict NUE as function of the system properties
    p1 <- predict(m1,addx=T)

    # this is the order of input variables needed for model predictions (=newmods in predict function) 
    m1.cols <- colnames(p1$X)

    # make prediction dataset for situation that soil is fertilized by both organic and inorganic fertilizers, conventional fertilizer strategy
    dt.new <- copy(r.dt.melt)

    # add the columns required for the ma model, baseline scenario
    # baseline is here defined as "strategy conventional", and mineral fertilizers, no biochar, no crop residue, no cover crops 
    dt.new[, fertilizer_typemineral := 1]
    dt.new[, fertilizer_typeorganic := 0]
    dt.new[, fertilizer_typecombined := 0]
    dt.new[, rfpyes := 0]
    dt.new[, rftyes := 0]
    dt.new[, rfryes := 0]
    dt.new[, biocharyes := 0]
    dt.new[, crop_residueyes := 0]
    dt.new[, cover_crop_and_crop_rotationyes := 0]
    dt.new[, tillageno_till := 0]
    dt.new[, cover_crop_and_crop_rotationyes := fifelse(cropintensity>1,1,0)]
    dt.new[, `tillageno-till` := fifelse(till_name =='no-till',1,0)]
    dt.new[, ctoyes := fifelse(cropname=='other',1,0)]
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

    ############################################## scenario SI0 (EE) ################################################################

    dt.s10 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]
    

    dt.s10[, fertilizer_typeenhanced := 1]
    dt.s10[, fertilizer_typemineral := 0]
    dt.s10[, fertilizer_typeorganic := 0]
    dt.s10[, fertilizer_typecombined := 0]
    dt.s10[, rfpyes := 0]
    dt.s10[, rftyes := 0]
    dt.s10[, rfryes := 0]
    dt.s10[, biocharyes := 0]
    dt.s10[, crop_residueyes := 0]
    dt.s10[, cover_crop_and_crop_rotationyes := 0]
    dt.s10[, tillageno_till := 0]
    dt.s10[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s10[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s10[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s10[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]
    
    dt.newmod <- as.matrix(dt.s10[,mget(c(m1.cols))])

    dt.pred.s10 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s10[,c(cols) := dt.pred.s10]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s10[,.(x,y,s10 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s10 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_SI0.tif', overwrite = TRUE)
    
    ############################################## scenario SI1 (CF) ################################################################
    # scenario SI1. the  combination of measures with change in Combined fertilizer (CF vs. MF)

    dt.s11 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s11[, fertilizer_typeenhanced := 0]
    dt.s11[, fertilizer_typemineral := 0]
    dt.s11[, fertilizer_typeorganic := 0]
    dt.s11[, fertilizer_typecombined := 1]
    dt.s11[, rfpyes := 0]
    dt.s11[, rftyes := 0]
    dt.s11[, rfryes := 0]
    dt.s11[, biocharyes := 0]
    dt.s11[, crop_residueyes := 0]
    dt.s11[, cover_crop_and_crop_rotationyes := 0]
    dt.s11[, tillageno_till := 0]
    dt.s11[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s11[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s11[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s11[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s11[,mget(c(m1.cols))])

    dt.pred.s11 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s11[,c(cols) := dt.pred.s11]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s11[,.(x,y,s11 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s11 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_SI1.tif', overwrite = TRUE)
    
    
    ############################################## scenario SI2 (OF) ################################################################
    # scenario SI2. the  combination of measures with change in OF (OF vs. MF)

    dt.s12 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s12[, fertilizer_typeenhanced := 0]
    dt.s12[, fertilizer_typemineral := 0]
    dt.s12[, fertilizer_typeorganic := 1]
    dt.s12[, fertilizer_typecombined := 0]
    dt.s12[, rfpyes := 0]
    dt.s12[, rftyes := 0]
    dt.s12[, rfryes := 0]
    dt.s12[, biocharyes := 0]
    dt.s12[, crop_residueyes := 0]
    dt.s12[, cover_crop_and_crop_rotationyes := 0]
    dt.s12[, tillageno_till := 0]
    dt.s12[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s12[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s12[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s12[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s12[,mget(c(m1.cols))])

    dt.pred.s12 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s12[,c(cols) := dt.pred.s12]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s12[,.(x,y,s12 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s12 - base]
    
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_SI2.tif', overwrite = TRUE)
    
    
    ############################################## scenario SI3 (RFP) ################################################################
    # scenario SI3. the combination of measures with change in RFP. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)

    dt.s13 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s13[, fertilizer_typeenhanced := 0]
    dt.s13[, fertilizer_typemineral := 1]
    dt.s13[, fertilizer_typeorganic := 0]
    dt.s13[, fertilizer_typecombined := 0]
    dt.s13[, rfpyes := 1]
    dt.s13[, rftyes := 0]
    dt.s13[, rfryes := 0]
    dt.s13[, biocharyes := 0]
    dt.s13[, crop_residueyes := 0]
    dt.s13[, cover_crop_and_crop_rotationyes := 0]
    dt.s13[, tillageno_till := 0]
    dt.s13[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s13[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s13[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s13[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s13[,mget(c(m1.cols))])

    dt.pred.s13 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s13[,c(cols) := dt.pred.s13]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s13[,.(x,y,s13 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s13 - base]
 
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_SI3.tif', overwrite = TRUE)
    
    
    
    
    ############################################## scenario SI4 (RFR) ################################################################
    # scenario SI4. the combination of measures with change in RFR. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)
    
    # make local copy 
    dt.s14 <- copy(dt.new)
    
    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]
    

    dt.s14[, fertilizer_typeenhanced := 0]
    dt.s14[, fertilizer_typemineral := 1]
    dt.s14[, fertilizer_typeorganic := 0]
    dt.s14[, fertilizer_typecombined := 0]
    dt.s14[, rfpyes := 0]
    dt.s14[, rftyes := 0]
    dt.s14[, rfryes := 1]
    dt.s14[, biocharyes := 0]
    dt.s14[, crop_residueyes := 0]
    dt.s14[, cover_crop_and_crop_rotationyes := 0]
    dt.s14[, tillageno_till := 0]
    dt.s14[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s14[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s14[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s14[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]
    

    dt.newmod <- as.matrix(dt.s14[,mget(c(m1.cols))])
    

    dt.pred.s14 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s14[,c(cols) := dt.pred.s14]
    
    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]
    
    dt.fin <- merge(dt.fin,dt.s14[,.(x,y,s14 = pMDmean,cropname)],by=c('x','y','cropname'))
    
    dt.fin[, improvement := s14 - base]
    
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    terra::writeRaster(r.fin,'scenario_SI4.tif', overwrite = TRUE)
    
    
    ############################################## scenario SI5 (RFT) ################################################################
    # scenario SI5. the combination of measures with change in RFT. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)

    dt.s15 <- copy(dt.new)
    
    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s15[, fertilizer_typeenhanced := 0]
    dt.s15[, fertilizer_typemineral := 1]
    dt.s15[, fertilizer_typeorganic := 0]
    dt.s15[, fertilizer_typecombined := 0]
    dt.s15[, rfpyes := 0]
    dt.s15[, rftyes := 1]
    dt.s15[, rfryes := 0]
    dt.s15[, biocharyes := 0]
    dt.s15[, crop_residueyes := 0]
    dt.s15[, cover_crop_and_crop_rotationyes := 0]
    dt.s15[, tillageno_till := 0]
    dt.s15[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s15[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s15[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s15[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s15[,mget(c(m1.cols))])

    dt.pred.s15 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s15[,c(cols) := dt.pred.s15]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s15[,.(x,y,s15 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s15 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_SI5.tif', overwrite = TRUE)
    
    
    ############################################## scenario SI6 (BC) ################################################################
    # scenario SI6. the combination of measures with change in BC. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)
    
    dt.s16 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s16[, fertilizer_typeenhanced := 0]
    dt.s16[, fertilizer_typemineral := 1]
    dt.s16[, fertilizer_typeorganic := 0]
    dt.s16[, fertilizer_typecombined := 0]
    dt.s16[, rfpyes := 0]
    dt.s16[, rftyes := 0]
    dt.s16[, rfryes := 0]
    dt.s16[, biocharyes := 1]
    dt.s16[, crop_residueyes := 0]
    dt.s16[, cover_crop_and_crop_rotationyes := 0]
    dt.s16[, tillageno_till := 0]
    dt.s16[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s16[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s16[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s16[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s16[,mget(c(m1.cols))])

    dt.pred.s16 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s16[,c(cols) := dt.pred.s16]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s16[,.(x,y,s16 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s16 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_SI6.tif', overwrite = TRUE)
    
    
    
    ############################################## scenario SI7 (RES) ################################################################
    # scenario SI7. the combination of measures with change in RES. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)

    dt.s17 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s17[, fertilizer_typeenhanced := 0]
    dt.s17[, fertilizer_typemineral := 1]
    dt.s17[, fertilizer_typeorganic := 0]
    dt.s17[, fertilizer_typecombined := 0]
    dt.s17[, rfpyes := 0]
    dt.s17[, rftyes := 0]
    dt.s17[, rfryes := 0]
    dt.s17[, biocharyes := 0]
    dt.s17[, crop_residueyes := 1]
    dt.s17[, cover_crop_and_crop_rotationyes := 0]
    dt.s17[, tillageno_till := 0]
    dt.s17[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s17[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s17[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s17[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s17[,mget(c(m1.cols))])

    dt.pred.s17 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s17[,c(cols) := dt.pred.s17]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s17[,.(x,y,s17 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s17 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'scenario_SI7.tif', overwrite = TRUE)
    
    
    ############################################## scenario SI8 (CC/ROT) ################################################################
    # scenario SI8. the combination of measures with change in CC/ROT. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)

    dt.s18 <- copy(dt.new)
    
    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s18[, fertilizer_typeenhanced := 0]
    dt.s18[, fertilizer_typemineral := 1]
    dt.s18[, fertilizer_typeorganic := 0]
    dt.s18[, fertilizer_typecombined := 0]
    dt.s18[, rfpyes := 0]
    dt.s18[, rftyes := 0]
    dt.s18[, rfryes := 0]
    dt.s18[, biocharyes := 0]
    dt.s18[, crop_residueyes := 0]
    dt.s18[, cover_crop_and_crop_rotationyes := 1]
    dt.s18[, tillageno_till := 0]
    dt.s18[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s18[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s18[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s18[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s18[,mget(c(m1.cols))])

    dt.pred.s18 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s18[,c(cols) := dt.pred.s18]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s18[,.(x,y,s18 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s18 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'scenario_SI8.tif', overwrite = TRUE)
    
    ############################################## scenario SI9 (NT/RT) ################################################################
    # scenario SI9. the combination of measures with change in NT/RT. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)

    dt.s19 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s19[, fertilizer_typeenhanced := 0]
    dt.s19[, fertilizer_typemineral := 1]
    dt.s19[, fertilizer_typeorganic := 0]
    dt.s19[, fertilizer_typecombined := 0]
    dt.s19[, rfpyes := 0]
    dt.s19[, rftyes := 0]
    dt.s19[, rfryes := 0]
    dt.s19[, biocharyes := 0]
    dt.s19[, crop_residueyes := 0]
    dt.s19[, cover_crop_and_crop_rotationyes := 0]
    dt.s19[, tillageno_till := 1]
    dt.s19[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s19[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s19[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s19[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s19[,mget(c(m1.cols))])

    dt.pred.s19 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s19[,c(cols) := dt.pred.s19]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s19[,.(x,y,s19 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s19 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_SI9.tif', overwrite = TRUE)
    
    
    ####################################################################################################################################################    
    ############################################## scenario 1 (Nutrient management) ################################################################
    # scenario 1. the combination of measures with change in RFR, RFT and BC. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)

    dt.s1 <- copy(dt.new)
    
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s1[, fertilizer_typeenhanced := 1]
    dt.s1[, fertilizer_typemineral := 0]
    dt.s1[, fertilizer_typeorganic := 1]
    dt.s1[, fertilizer_typecombined := 1]
    dt.s1[, rfpyes := 1]
    dt.s1[, rftyes := 1]
    dt.s1[, rfryes := 1]
    dt.s1[, biocharyes := 1]
    dt.s1[, crop_residueyes := 0]
    dt.s1[, cover_crop_and_crop_rotationyes := 0]
    dt.s1[, tillageno_till := 0]
    dt.s1[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s1[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s1[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s1[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s1[,mget(c(m1.cols))])

    dt.pred.s1 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s1[,c(cols) := dt.pred.s1]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s1[,.(x,y,s1 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s1 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    terra::writeRaster(r.fin,'scenario_1.tif', overwrite = TRUE)
    
    
    ############################################## scenario 2 (crop management)################################################################
    # scenario 2. the combination of measures with change in RES, CC, ROT (Optimized crop management vs. Conventional crop management)

    dt.s2 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s2[, fertilizer_typeenhanced := 0]
    dt.s2[, fertilizer_typemineral := 1]
    dt.s2[, fertilizer_typeorganic := 0]
    dt.s2[, fertilizer_typecombined := 0]
    dt.s2[, rfpyes := 0]
    dt.s2[, rftyes := 0]
    dt.s2[, rfryes := 0]
    dt.s2[, biocharyes := 0]
    dt.s2[, crop_residueyes := 1]
    dt.s2[, cover_crop_and_crop_rotationyes := 1]
    dt.s2[, tillageno_till := 0]
    dt.s2[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s2[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s2[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s2[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s2[,mget(c(m1.cols))])

    dt.pred.s2 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s2[,c(cols) := dt.pred.s2]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s2[,.(x,y,s2 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s2 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_2.tif', overwrite = TRUE)
    
    
    ############################################## scenario 3 (NT/RT) ################################################################
    # scenario 3. the combination of measures with change in NT/RT. (Optimized fertilizer strategy vs.Conventional fertilizer strategies)

    dt.s3 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s3[, fertilizer_typeenhanced := 0]
    dt.s3[, fertilizer_typemineral := 1]
    dt.s3[, fertilizer_typeorganic := 0]
    dt.s3[, fertilizer_typecombined := 0]
    dt.s3[, rfpyes := 0]
    dt.s3[, rftyes := 0]
    dt.s3[, rfryes := 0]
    dt.s3[, biocharyes := 0]
    dt.s3[, crop_residueyes := 0]
    dt.s3[, cover_crop_and_crop_rotationyes := 0]
    dt.s3[, tillageno_till := 1]
    dt.s3[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s3[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s3[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s3[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s3[,mget(c(m1.cols))])

    dt.pred.s3 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s3[,c(cols) := dt.pred.s3]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s3[,.(x,y,s3 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s3 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_3.tif', overwrite = TRUE)
    
    
    ############################################## scenario 4 (combination)################################################################
    # scenario 4. the combination of measures with change in EE, CF, RFR, RFT, BC, RES, CC, ROT

    dt.s4 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    dt.s4[, fertilizer_typeenhanced := 1]
    dt.s4[, fertilizer_typemineral := 0]
    dt.s4[, fertilizer_typeorganic := 1]
    dt.s4[, fertilizer_typecombined := 1]
    dt.s4[, rfpyes := 1]
    dt.s4[, rftyes := 1]
    dt.s4[, rfryes := 1]
    dt.s4[, biocharyes := 1]
    dt.s4[, crop_residueyes := 1]
    dt.s4[, cover_crop_and_crop_rotationyes := 1]
    dt.s4[, tillageno_till := 1]
    dt.s4[, n_dose_scaled := ((nh4+no3+nam) * 0.7 - dt.fert.bs$mean)/ dt.fert.bs$sd ]
    dt.s4[, `n_dose_scaled:soc_scaled` := (n_dose_scaled - 0.1 )*soc_scaled]
    dt.s4[, `rfpyes:ctmyes` := rfpyes*ctmyes]
    dt.s4[, `mat_scaled:ctmyes` := mat_scaled*ctmyes]

    dt.newmod <- as.matrix(dt.s4[,mget(c(m1.cols))])

    dt.pred.s4 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s4[,c(cols) := dt.pred.s4]

    dt.fin <- dt.new[,.(x,y,base = pMDmean,cropname,area)]

    dt.fin <- merge(dt.fin,dt.s4[,.(x,y,s4 = pMDmean,cropname)],by=c('x','y','cropname'))

    dt.fin[, improvement := s4 - base]

    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    terra::writeRaster(r.fin,'scenario_4.tif', overwrite = TRUE)   
    
    
################################################### Plot ####################################################
    theme_set(theme_bw())
    
    library(ggplot2)
    library(sf)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(terra)
    library(cowplot)
    library(vcd)
    
    ######################################### scenario_SI0 ##########################################################

    theme_set(theme_bw())

    r10 <- terra::rast('scenario_SI0.tif')

    r10.p <- as.data.frame(r10,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p10 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r10.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-4,0,3,6,300),
                                            labels = c('< 0','0-3','3-6','> 6') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Enhanced efficiency fertilizer (EE)") +
      coord_sf(crs = 4326) 
    
    ggsave(plot = p10, filename = 'nue_effect_sI0.png')
    
    ########################################### scenario_SI1 ########################################################

    theme_set(theme_bw())

    r11 <- terra::rast('scenario_SI1.tif')

    r11.p <- as.data.frame(r11,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p11 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r11.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-5,0,3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6')))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Combined fertilizer (CF)") +
      coord_sf(crs = 4326)
    ggsave(plot = p11, filename = 'nue_effect_sI1.png')
    
    
    ########################################### scenario_SI2 ########################################################

    theme_set(theme_bw())

    r12 <- terra::rast('scenario_SI2.tif')

    r12.p <- as.data.frame(r12,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p12 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r12.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-6,0,-3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6')))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Organic fertilizer (OF)") +
      coord_sf(crs = 4326)
    ggsave(plot = p12, filename = 'nue_effect_sI2.png')
    
    
    ########################################### scenario_SI3 ########################################################
    
    theme_set(theme_bw())

    r13 <- terra::rast('scenario_SI3.tif')

    r13.p <- as.data.frame(r13,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p13 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r13.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-5,0,3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6')))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Right fertilizer placement (RFP)") +
      coord_sf(crs = 4326)
    ggsave(plot = p13, filename = 'nue_effect_sI3.png')
    ########################################### scenario_SI4 ########################################################

    theme_set(theme_bw())

    r14 <- terra::rast('scenario_SI4.tif')

    r14.p <- as.data.frame(r14,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p14 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r14.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-4,0,3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Right fertilizer rate (RFR)") +
      coord_sf(crs = 4326)
    ggsave(plot = p14, filename = 'nue_effect_sI4.png')
    ########################################### scenario_SI5 ########################################################

    theme_set(theme_bw())

    r15 <- terra::rast('scenario_SI5.tif')

    r15.p <- as.data.frame(r15,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p15 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r15.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-5,0,3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Right fertilizer timing (RFT)") +
      coord_sf(crs = 4326)
    ggsave(plot = p15, filename = 'nue_effect_sI5.png')
    ########################################### scenario_SI6 ########################################################

    theme_set(theme_bw())

    r16 <- terra::rast('scenario_SI6.tif')

    r16.p <- as.data.frame(r16,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p16 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r16.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-2,0,3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Biochar (BC)") +
      coord_sf(crs = 4326)
    ggsave(plot = p16, filename = 'nue_effect_sI6.png')
    ########################################### scenario_SI7 ########################################################

    theme_set(theme_bw())

    r17 <- terra::rast('scenario_SI7.tif')

    r17.p <- as.data.frame(r17,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p17 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r17.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-6,0,3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Residue retention (RES)") +
      coord_sf(crs = 4326)
    ggsave(plot = p17, filename = 'nue_effect_sI7.png')
    ########################################### scenario_SI8 ########################################################

    theme_set(theme_bw())

    r18 <- terra::rast('scenario_SI8.tif')

    r18.p <- as.data.frame(r18,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p18 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r18.p,aes(x=x,y=y, name ='none',
                                 fill = cut(improvement,breaks= c(-5,0,3,6,300), 
                                            labels = c('< 0','0-3','3-6','> 6')))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 15),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = ' ') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Cover cropping or Crop rotation (CC/ROT)") +
      coord_sf(crs = 4326)
    ggsave(plot = p18, filename = 'nue_effect_sI8.png')

    library(ggpubr)
    p_S4<-ggarrange(p10,p11, p12, p13, p14,p15,p16,p17,p18, heights = c(4, 4, 4, 4), ncol = 3, nrow = 3, #common.legend = TRUE,legend = "bottom",
                    labels = c("a", "b","c","d","e","f","g","h","i"), font.label=list(size=28),hjust = -0.2, vjust = 1)
    p_S4
    
    ggsave(file = "p_S4.png",width = 410,height = 197, units = "mm")
    
    
    
    
    
    ######################################### scenario_1 ##########################################################

    theme_set(theme_bw())

    r1 <- terra::rast('scenario_1.tif')

    r1.p <- as.data.frame(r1,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    #plot a basic world map plot
    p1 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r1.p,aes(x=x,y=y, name ='none',
                                fill = cut(improvement,breaks= c(18,20,25,30,300),
                                           labels = c('< 20','20-25','25-30','>30') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 18),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = 'NUE increased (%)') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Optimal nutrient management") +
      coord_sf(crs = 4326) 
    
    ggsave(plot = p1, filename = 'nue_effect_s1.png')
    
    
    ########################################### scenario_2 ########################################################

    theme_set(theme_bw())

    r2 <- terra::rast('scenario_2.tif')

    r2.p <- as.data.frame(r2,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p2 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r2.p,aes(x=x,y=y, name ='none',
                                fill = cut(improvement,breaks= c(-4,0,3,6,300), 
                                           labels = c('< 0','0-3','3-6','> 6') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 18),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = 'NUE increased (%)') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Optimal crop management") +
      coord_sf(crs = 4326)
    ggsave(plot = p2, filename = 'nue_effect_s2.png')
    
    ########################################### scenario_3 ########################################################

    theme_set(theme_bw())

    r3 <- terra::rast('scenario_3.tif')

    r3.p <- as.data.frame(r3,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # plot a basic world map plot
    p3 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r3.p,aes(x=x,y=y, name ='none',
                                fill = cut(improvement,breaks= c(-7,0,3,6,300), 
                                           labels = c('< 0','0-3','3-6','> 6') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 18),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = 'NUE increased (%)') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Optimal soil management") +
      coord_sf(crs = 4326)
    ggsave(plot = p3, filename = 'nue_effect_s3.png')
    
    
    
    ########################################### scenario_4 ########################################################

    theme_set(theme_bw())

    r4 <- terra::rast('scenario_4.tif')

    r4.p <- as.data.frame(r4,xy=TRUE)

    world <- ne_countries(scale = "medium", returnclass = "sf")

    p4 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
      geom_tile(data = r4.p,aes(x=x,y=y, name ='none',
                                fill = cut(improvement,breaks= c(22,25,30,35,300), 
                                           labels = c('< 25','25-30','30-35','>35') ))) +
      theme_void() +
      theme(legend.position = c(0.1,0.4), text = element_text(size = 18),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank()) +
      labs(fill = 'NUE increased (%)') +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("Combined optimal management practices") +
      coord_sf(crs = 4326)
    ggsave(plot = p4, filename = 'nue_effect_s4.png')
    
    library(ggpubr)
    p<-ggarrange(p1, p2, p3, p4, heights = c(4, 4, 4, 4), ncol = 2, nrow = 2, #common.legend = TRUE,legend = "bottom",
                 labels = c("a", "b","c","d"), font.label=list(size=28),hjust = -0.2, vjust = 1)
    p
    
    ggsave(file = "p.png",width = 410,height = 197, units = "mm")
    
    