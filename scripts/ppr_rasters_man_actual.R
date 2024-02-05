# load in the earlier prepared datasets with global data
# input data objects have been prepared by ppr_datasets_for_upscaling

# spatial object with country-climate regions
cc <- readRDS('data/countryclimate.rds')

# sPAM raster ID per country-climate-ID
cc.spam <- readRDS('data/spam_country_id.rds')

# harvested crop areas (in ha) SPAM per technology level
cr.area.all <- readRDS('data/spam_areas_full.rds')

# nitrogen inputs for cropland (manure and fertilizer) and potential N to be avoided
cc.man <- readRDS('data/ngift_extracted_full.rds')

# tillage ()
cc.till <- readRDS('data/tillage_extracted_area_full.rds')

# assumptions to be made for upscaling Lunchengs data

# measures CF and OF: on areas where there is manure disposed that can be applied: n_loss
# RFR only as NUE < 50%
# EE, RFP, RFT are applied in high technology level
# RES, CC and ROT

# read the raster with soil data (and use this to project all other files, so that extent and crs are similar)
r.soil <- terra::rast('data/soil.tif')

# raster file with NUE from IMAGE (total NUE = Nup / Nfert + Nman_eff + Ndep + Nfix) for both grassland and cropland
r.nue <- terra::rast('D:/DATA/07 image/nue_ag.asc')
r.nue <- project(r.nue,r.soil)
terra::writeRaster(r.nue,'products/r.nue.tif', overwrite = TRUE)

# make raster file for technology level (1 = low, 0 is high)
r.techlevel <- copy(cr.area.all)
r.techlevel <- r.techlevel[,lapply(.SD,sum),.SDcols = c('ALL','TECH_I','TECH_H','TECH_L'),by='CELL5M']
r.techlevel[,TECH_LOW := fifelse(TECH_L > 0.5 * ALL,1,0)]
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,r.techlevel[,.(CELL5M,TECH_LOW)],by='CELL5M',all.x=TRUE)
setcolorder(r.spam.dt,c('x','y'))
r.spam.dt[,CELL5M := NULL]
r.agtech <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.agtech) <- 'epsg:4326'
r.agtech <- project(r.agtech,r.soil,method='near')
terra::writeRaster(r.agtech,'products/r.agtech.tif', overwrite = TRUE)

# make raster where crop residues can be applied (1 = no residue applied, 0 = incorporation is current practice)
cc.spam <- readRDS('data/spam_country_id.rds')
d1 <- fread('data/210322DB001_ccID.csv')
d1 <- d1[,.(ccID,area = area_till_cr,cat_crb)]
d1 <- dcast(d1,ccID~cat_crb,value.var = 'area',fun.aggregate = sum)
d1[, RES := fifelse(BURN > 0.25 * (BURN + INC),1,0)]
r.residue <- merge(cc.spam[,.(CELL5M,ccID = ID)],d1[,.(ccID,RES)],by='ccID')
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,r.residue[,.(CELL5M,RES)],by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.residue <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.residue) <- 'epsg:4326'
r.residue <- project(r.residue,r.soil,method='near')
terra::writeRaster(r.residue,'products/r.residue.tif', overwrite = TRUE)

# make raster file for cover_catch cropping (1 = catch crop can be applied, 0 = catch crop is current practice)
cc.spam <- readRDS('data/spam_country_id.rds')
d1 <- fread('data/210322DB001_ccID.csv')
d1 <- d1[,.(ccID,area = area_till_cr,cat_ccr)]
d1 <- dcast(d1,ccID~cat_ccr,value.var = 'area',fun.aggregate = sum)
d1[, CCROP := fifelse(CATCH > 0.25 * (NOCATCH + CATCH),1,0)]
r.ccrop <- merge(cc.spam[,.(CELL5M,ccID = ID)],d1[,.(ccID,CCROP)],by='ccID')
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,r.ccrop[,.(CELL5M,CCROP)],by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.ccrop <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.ccrop) <- 'epsg:4326'
r.ccrop <- project(r.ccrop,r.soil,method='near')
terra::writeRaster(r.ccrop,'products/r.ccrop.tif', overwrite = TRUE)

# make raster for crop rotation improvements (see file 220501 upscaling nue.R)
r1.cropint <- terra::rast('data/cropintensity.tif')
r1.cropint.dt <- as.data.table(as.data.frame(r1.cropint,xy=TRUE))
r1.cropint.dt[, CROPINT := fifelse(cropintensity>1,1,0)]
r.cropint <- terra::rast(r1.cropint.dt,type='xyz')
terra::crs(r.cropint) <- 'epsg:4326'
r.cropint <- project(r.cropint,r.soil,method='near')
terra::writeRaster(r.cropint,'products/r.cropint.tif', overwrite = TRUE)

# read the earlier prepared file with tillage practices
r.till <- terra::rast('data/tillage.tif')

# make one tillage file where no-till can be applied (1) or where no-till is already practice (0)
cc.spam <- readRDS('data/spam_country_id.rds')
d1 <- fread('data/210322DB001_ccID.csv')
d1 <- d1[,.(ccID,area = area_till_cr,tillage)]
d1 <- dcast(d1,ccID~tillage,value.var = 'area',fun.aggregate = sum)
d1[, NOTILL := fifelse(TILL_L < 0.5 * (TILL_M + TILL_H + TILL_L),1,0)]

r.notill <- merge(cc.spam[,.(CELL5M,ccID = ID)],d1[,.(ccID,NOTILL)],by='ccID')
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,r.notill[,.(CELL5M,NOTILL)],by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.notill <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.notill) <- 'epsg:4326'
r.notill <- project(r.notill,r.soil,method='near')
terra::writeRaster(r.notill,'products/r.notill.tif', overwrite = TRUE)


# make crop types raster for wheat
cr.area.all <- readRDS('data/spam_areas_full.rds')
cr.area.wheat <- copy(cr.area.all)
cr.area.wheat[,crop := fifelse(crop %in% c('WHEA','BARL','SMIL','PMIL'),'wheat','other')]
cr.area.wheat <- cr.area.wheat[,list(AREA = sum(ALL)),by=.(CELL5M,crop)]
cr.area.wheat <- dcast(cr.area.wheat,CELL5M~crop, value.var='AREA')
cr.area.wheat[,cwheat := fifelse(wheat > 0.25 * (wheat+other),1,0)]
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,cr.area.wheat[,.(CELL5M,cwheat)],by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.cwheat <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.cwheat) <- 'epsg:4326'
r.cwheat <- project(r.cwheat,r.soil,method='near')
terra::writeRaster(r.cwheat,'products/r.cwheat.tif', overwrite = TRUE)

# make crop types raster for maize
cr.area.all <- readRDS('data/spam_areas_full.rds')
cr.area.sel <- copy(cr.area.all)
cr.area.sel[,crop := fifelse(crop %in% c('MAIZ'),'maize','other')]
cr.area.sel <- cr.area.sel[,list(AREA = sum(ALL)),by=.(CELL5M,crop)]
cr.area.sel <- dcast(cr.area.sel,CELL5M~crop, value.var='AREA')
cr.area.sel[,cmaize := fifelse(maize > 0.25 * (maize+other),1,0)]
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,cr.area.sel[,.(CELL5M,cmaize)],by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.cmaize <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.cmaize) <- 'epsg:4326'
r.cmaize <- project(r.cmaize,r.soil,method='near')
terra::writeRaster(r.cmaize,'products/r.cmaize.tif', overwrite = TRUE)

# make crop types raster for rice
cr.area.all <- readRDS('data/spam_areas_full.rds')
cr.area.sel <- copy(cr.area.all)
cr.area.sel[,crop := fifelse(crop %in% c('RICE'),'rice','other')]
cr.area.sel <- cr.area.sel[,list(AREA = sum(ALL)),by=.(CELL5M,crop)]
cr.area.sel <- dcast(cr.area.sel,CELL5M~crop, value.var='AREA')
cr.area.sel[,crice := fifelse(rice > 0.25 * (rice+other),1,0)]
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,cr.area.sel[,.(CELL5M,crice)],by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.crice <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.crice) <- 'epsg:4326'
r.crice <- project(r.crice,r.soil,method='near')
terra::writeRaster(r.crice,'products/r.crice.tif', overwrite = TRUE)


# make crop types raster for other crops
cr.area.all <- readRDS('data/spam_areas_full.rds')
cr.area.sel <- copy(cr.area.all)
cr.area.sel[,crop := fifelse(crop %in% c('RICE','MAIZE','WHEA','BARL','SMIL','PMIL'),'scrop','other')]
cr.area.sel <- cr.area.sel[,list(AREA = sum(ALL)),by=.(CELL5M,crop)]
cr.area.sel <- dcast(cr.area.sel,CELL5M~crop, value.var='AREA')
cr.area.sel[,cother := fifelse(other > 0.25 * (scrop+other),1,0)]
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,cr.area.sel[,.(CELL5M,cother)],by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.cother <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.cother) <- 'epsg:4326'
r.cother <- project(r.cother,r.soil,method='near')
terra::writeRaster(r.cother,'products/r.cother.tif', overwrite = TRUE)

# make raster where still sufficient manure is available to be applied
# load object with n dose manure and fertilizer (in ton N per ccID)
cc.spam <- readRDS('data/spam_country_id.rds')
cr.area.all <- readRDS('data/spam_areas_full.rds')
cr.area.all <- cr.area.all[,list(area = sum(ALL)),by='CELL5M']
cc.spam <- merge(cc.spam,cr.area.all,by='CELL5M')
cc.spam <- cc.spam[,list(area = sum(area)),by='ID']
cc.man <- readRDS('data/ngift_extracted_full.rds')
d1 <- cc.man[,.(ccID,ndm_loss2)]
d1 <- merge(d1,cc.spam[,.(ccID=ID,area)],by='ccID')
d1[,nav_ha := ndm_loss2 /area ] # ton/ha
d1[,ndm_loss_cat := fifelse(ndm_loss2 > 1 & nav_ha > 0.1, 1,0)]

cc.spam <- readRDS('data/spam_country_id.rds')
r.ndmloss <- merge(cc.spam[,.(CELL5M,ccID = ID)],d1[,.(ccID,ndm_loss_cat)],by='ccID')
r.spam <- terra::rast('D:/DATA/04 crop/spam2010/CELL5M.asc')
r.spam.dt <- as.data.table(as.data.frame(r.spam,xy=TRUE))
r.spam.dt <- merge(r.spam.dt,r.ndmloss,by='CELL5M',all.x=TRUE)
r.spam.dt[,CELL5M := NULL]
r.ndmloss <- terra::rast(r.spam.dt,type='xyz')
terra::crs(r.ndmloss) <- 'epsg:4326'
r.ndmloss <- project(r.ndmloss,r.soil,method='near')
terra::writeRaster(r.ndmloss,'products/r.ndmloss.tif', overwrite = TRUE)

