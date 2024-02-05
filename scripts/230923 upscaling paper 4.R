# make one data.table with all site properties needed for upscaling meta-analytical results for global maps impact of measures on N2O, NH3 and N leaching

  # load packages
  require(sf);require(terra);require(data.table)

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
  r.all <- c(r.soil,r.clim,r.agtech,r.nue,r.cwheat,r.cmaize,r.crice,r.cother,r.notill,r.cropint,r.ccrop,r.residue,r.ndmloss)

# convert to data.table
  d1 <- as.data.table(as.data.frame(r.all,xy=TRUE, na.rm = FALSE))

  # setnames
  setnames(d1,
           old = c('isric_phw_mean_0_5','isric_clay_mean_0_5','isric_soc_mean_0_5'),
           new = c('phw','clay','soc'),skip_absent = T)

  # select only land area
  d1 <- d1[!(is.na(mat)|is.na(pre))]
  d1 <- d1[!(is.na(cwheat) & is.na(cmaize) & is.na(crice) & is.na(cother))]

  # replace missing values (adapt values due to (assume that measure can not be applied)
  d1[,nue_ag := fifelse(is.na(nue_ag),median(nue_ag,na.rm=T),nue_ag)]
  d1[is.na(NOTILL), NOTILL := 0]
  d1[is.na(CROPINT), CROPINT := 0]
  d1[is.na(CCROP), CCROP := 0]
  d1[is.na(RES), RES := 0]
  d1[is.na(ndm_loss_cat), ndm_loss_cat := 0]

  # remove columns not needed
  d1[,c('cropintensity','ccID') := NULL]

  # a short description
  # TECH_LOW: if value is 1 then you can apply RFP, EE and RFT. if value is 0 then these measures are already practice.
  # if nue_ag < 0.5 then you can apply RFR
  # NOTILL: if value is 1 then you can apply no-till. If value is zero then no-till is already practice
  # RES: if value is 1 then you can apply crop residues. If value is zero then application of crop residues is already practice
  # CATCH: if value is 1 then you can apply catch crops. If value is zero then application of use of catch crops is already practice or not possible.
  # CROPINT: if value is 1 then there is a possiblity to reduce intensity. If value is zero then adding less crops is not possible.
  # ndm_loss_cat: if the value is 1 then CF or OF can be applied. if value is zero, then there is no manure left to do so (since all manure is applied already)
  # cwheat, cmaize, crice and cother show whether the crops are growing there (clusterd in these categories following your meta-analytical models)





