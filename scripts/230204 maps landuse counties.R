# require package
require(terra)

# load vectors for Qiyang and Quzhou, and reproject to 4326 CRS
v1 <- vect('../01 data/Qiyang_cropland.shp')
v1 <- project(v1,'epsg:4326')
v2 <- vect('../01 data/Quzhou_cropland.shp')
v2 <- project(v2,'epsg:4326')

# get all tiff files with total harvested areas per crop
rfiles <- list.files('D:/DATA/04 crop/spam2010/',pattern ='_A.tif',full.names = T)

  # crop and mask the SPAM raster for Qiyang only
  crops <- rast(rfiles)
  crops.crop <- terra::crop(crops,ext(v1) + 0.01)
  crops.mask <- terra::mask(crops.crop,v1)

  # make local copy, update names and write rater to disk
  crops.qiyang <- crops.mask
  names(crops.qiyang) <- gsub('spam2010V2r0_global_H_|_A$','',names(crops.qiyang))
  terra::writeRaster(crops.qiyang,'products/crops_qiyang_all.tif', overwrite = TRUE)

  # crop and mask the SPAM raster for Quzhou
  crops.crop <- terra::crop(crops,ext(v2) + 0.01)
  crops.mask <- terra::mask(crops.crop,v2)

  # make local copy, update names and write rater to disk
  crops.quzhou <- crops.mask
  names(crops.quzhou) <- gsub('spam2010V2r0_global_H_|_A$','',names(crops.quzhou))
  terra::writeRaster(crops.quzhou,'products/crops_quzhou_all.tif', overwrite = TRUE)

# get all tiff files with total harvested irrigated areas per crop
  rfiles <- list.files('D:/DATA/04 crop/spam2010/',pattern ='_I.tif',full.names = T)

  # crop and mask the SPAM raster for Qiyang only
  crops <- rast(rfiles)
  crops.crop <- terra::crop(crops,ext(v1) + 0.01)
  crops.mask <- terra::mask(crops.crop,v1)

  # make local copy, update names and write rater to disk
  crops.qiyang <- crops.mask
  names(crops.qiyang) <- gsub('spam2010V2r0_global_H_|_I$','',names(crops.qiyang))
  terra::writeRaster(crops.qiyang,'products/crops_qiyang_irrigated.tif', overwrite = TRUE)

  # crop and mask the SPAM raster for Quzhou
  crops.crop <- terra::crop(crops,ext(v2) + 0.01)
  crops.mask <- terra::mask(crops.crop,v2)

  # make local copy, update names and write rater to disk
  crops.quzhou <- crops.mask
  names(crops.quzhou) <- gsub('spam2010V2r0_global_H_|_I$','',names(crops.quzhou))

  # write raster with tillage to disk
  terra::writeRaster(crops.quzhou,'products/crops_quzhou_irrigated.tif', overwrite = TRUE)

# analyse cropping frequency

  require(data.table)
  r1 <- rast('products/crops_quzhou_all.tif')

  r1.dt <- as.data.frame(r1,xy=TRUE)
  r1.dt <- as.data.table(r1.dt)
  r1.dt[, id:= .I]
  r2 <- melt(r1.dt,id.vars = c('x','y','id'))
  r2[variable %in% c('MAIZ','WHEA'),cropcat := 'maize_wheat']
  r2[variable %in% c('VEGE'), cropcat := 'vegatable']
  r2[is.na(cropcat),cropcat := 'other']
  r2[,maxArea := sum(value),by=id]
  r3 <- r2[,list(area = sum(value)/maxArea[1]),by=c('id','x','y','cropcat')]
  r3 <- dcast(r3,id + x + y ~cropcat,value.var = 'area')
  r3[,id := NULL]

  # this is the raster with the fraction per crop category
  r4 <- rast(as.data.frame(r3), type="xyz")

  # add here the tiff file for the interpolated n dose
  mnup1 <- rast('products/quzhou_maize.tif')

  # resample from 8x8km (the original resolution) to 30x30m (the resolution of mnup1)
  r4fr <-  resample(r4,mnup,method='bilinear')

  # calcualte the corrected N dose by multiplying the first crop times the area fraction
  mnupcorrected <- mnup * r4fr$maize_wheat

  mnupcorrected <-  mnup * r4fr$maize_wheat + mnup2 * r4fr$vegatable + mnup3 * r4fr$other


