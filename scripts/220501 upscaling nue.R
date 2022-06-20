# upscaling results for global impacts of management on NUE based on the MD model of Luncheng


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

      # save file and clear objects from memory
      saveRDS(cr.area,file = 'D:/DATA/04 crop/products/spam_areas_full.rds')
      rm(cr.area)

    } else {

      # load prepared db with crop data SPAM
      cr.area <- as.data.table(readRDS('D:/DATA/04 crop/products/spam_areas_full.rds'))
    }


  # load in ph
  ph <- raster('data/isric')

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

  # clay content

  # map

  # mat

  # soil organic carbon


# clay


fertilizer_type + fertilizer_strategy + biochar + crop_residue + tillage + cover_crop_and_crop_rotation +
  g_crop_type + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled + soc_scaled : n_dose_scaled
