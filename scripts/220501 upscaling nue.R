# upscaling results for global impacts of management on NUE based on the MD model of Luncheng

# require packages
require(terra)

# the final model has the following variables
# these need to be downloaded on spatial map, 05 degree resolution


  # load in relevant data global maps (preprocessed for article Malte)

    # source: https://www.esa-landcover-cci.org/?q=node/164)
    cropland <- terra::rast('data/cropland83km_v2.tif')

    cropland.dt <- as.data.table(cropland)
    setnames(cropland.dt,'cci_croptype')

    # describe crops
    cropland.dt[cci_croptype == 10, ctype := 'rainfed_cropland']
    cropland.dt[cci_croptype == 20, ctype := 'irrigated_cropland']
    cropland.dt[cci_croptype == 30, ctype := 'mosaic_cropland']
    cropland.dt[cci_croptype == 40, ctype := 'natural_vegetation']

  # load crop area harvested with source SPAM
    if(FALSE){

      # read database with SPAM2005 properties (cropland area in ha)


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
      cr.area <- as.data.table(readRDS('D:/DATA/04 crop/products/spam_areas_full.rds'))
    }

  # load SPAM raster
  spam <- as.data.table(foreign::read.dbf('D:/DATA/04 crop/spam2010/cell5m_allockey_xy.dbf'))

  # load the relevant soil properties
  if(FALSE){

    # load in ph and clay as raster, WGS84, 4326, 0.5 x 0.5 degrees
    ph <- terra::rast('D:/DATA/01 soil/isric_phw_mean_0_5.tif')
    clay <- terra::rast('D:/DATA/01 soil/isric_clay_mean_0_5.tif')

    # combine both in one raster
    soil <- c(ph,clay)

    # write raster
    terra::writeRaster(r.clim,'data/soil.tif', overwrite = TRUE)
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


  # prepare raster with tillage practices
  # source: https://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:4085895
  # https://datapub.gfz-potsdam.de/download/10.5880.PIK.2019.011/
  if(FALSE){

    # read in the rasters from nc4 file
    tillage <- terra::rast('D:/DATA/05 tillage/tillage_revised.nc4')
    terra::crs(tillage) <- 'epsg:4326'

    # change resolution to 0.5 x 0.5 degree
    r.till <- terra::aggregate(tillage,fact = 0.5/0.08333333, fun = "modal")

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
    nman <- terra::resample(nman,r.clim[[1]],method='bilinear')

    # write raster with manure N dose to cropland to disk
    terra::writeRaster(nman,'data/nofert.tif', overwrite = TRUE)

  } else {

    # read the earlier prepared file with manure N dose
    nfert <- terra::rast('data/nofert.tif')
  }


  # biochar / crop residue / cover crop and rotation: yes or no

  # tillage
  # options: conventional, no-till and reduced

  # fertilizer strategy
  # options: conventional, placement, rate, timing

  # fertilizer type
  # options: combined, enhanced, mineral, organic

  # g_crop
  # options: maize, other, rice, vegetable, wheat

  # nitrogen dose



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

    # add both together
    nfert <- c(nfert_nh4,nfert_no3)

    # write raster with inorganic N fertilization to disk
    terra::writeRaster(nfert,'data/nifert.tif', overwrite = TRUE)


  } else {

    # read the earlier prepared file with fertilizer N dose
    nfert <- terra::rast('data/nifert.tif')
  }


fertilizer_type + fertilizer_strategy + biochar + crop_residue + tillage + cover_crop_and_crop_rotation +
  g_crop_type + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled + soc_scaled : n_dose_scaled
