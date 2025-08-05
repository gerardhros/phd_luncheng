# development of meta-regression model
# this script describes the preparation of the database used for regression modelling
#
# Luncheng You and Gerard Ros (august 2025)

# Load libraries
library(data.table);require(stringr);require(metafor)

# clean environment
rm(list=ls())

# read data
d1 <- readxl::read_xlsx('../06 papers/paper 6/yield_paper/Original_data.xlsx',sheet = "main_sorting")
d1 <- as.data.table(d1)

# --- add covariates ----

  # select covariates (to be updated when source database is expanded)
  if(FALSE){

    # convert to spatial object
    s1 <- st_as_sf(d1,coords = c('lon','lat'),crs = 4326)
    s1 <- vect(s1)

    # read in dbf file for metzger climatic regions
    s2 <- foreign::read.dbf('D:/DATA/03 metzger/GenS_v3.dbf')

    # what rasters are available
    # downloaded via QGIS for ISRIC, 0.5 degrees resolution, https://maps.isric.org/
    # downloaded via CRU, https://catalogue.ceda.ac.uk/uuid/89e1e34ec3554dc98594a5732622bce9
    # downloaded via https://datashare.ed.ac.uk/handle/10283/3089

    # read in the rasters via hard drive
    r1 <- list.files('D:/DATA/01 soil',pattern = 'tif|nc',full.names = T)
    r1 <- r1[!grepl('stack',r1)]
    r2 <- list.files('D:/DATA/02 climate',pattern = 'tif$|nc$',full.names = T)
    r3 <- list.files('D:/DATA/03 metzger',pattern = 'tif$|nc$',full.names = T)

    # read in the raster files and convert to spatrasters
    isric <- sds(r1)
    climate <- sds(r2)
    metzger <- rast(r3)
    isric <- rast(isric)
    climate <- rast(climate)

    # --- extract isric data ----

    # update names of isric raster to avoid duplication in names
    names(isric) <- stringr::str_split_fixed(names(isric),"_isric_",2)[,2]

    # extract data for the spatial objects
    d1.isric <- terra::extract(x = isric, y = s1)
    d2.isric <- terra::extract(x = isric, y = buffer(s1,width = 10000), fun = mean, na.rm=T)
    d3.isric <- terra::extract(x = isric, y = buffer(s1,width = 20000), fun = mean, na.rm=T)
    d4.isric <- terra::extract(x = isric, y = buffer(s1,width = 30000), fun = mean, na.rm=T)
    d5.isric <- terra::extract(x = isric, y = buffer(s1,width = 50000), fun = mean, na.rm=T)

    # convert to data.table to facilitatie re-arranging
    setDT(d1.isric);setDT(d2.isric);setDT(d3.isric);setDT(d4.isric);setDT(d5.isric)

    # function to adapt colnames
    acn <- function(x,var='or'){c('ID',paste0(var,'_',gsub('isric_|_mean_|','',x[-1])))}

    # adapt colnames
    setnames(d1.isric,acn(colnames(d1.isric)))
    setnames(d2.isric,acn(colnames(d2.isric),'e1'))
    setnames(d3.isric,acn(colnames(d3.isric),'e2'))
    setnames(d4.isric,acn(colnames(d4.isric),'e3'))
    setnames(d5.isric,acn(colnames(d5.isric),'e4'))

    c1.isric <- merge(d1.isric,d2.isric,by = "ID")
    c1.isric <- merge(c1.isric,d3.isric,by = "ID")
    c1.isric <- merge(c1.isric,d4.isric,by = "ID")
    c1.isric <- merge(c1.isric,d5.isric,by = "ID")

    c1.isric <- melt(c1.isric,id = 'ID',
                     measure=patterns("or_", "e1_","e2_","e3_","e4_"),
                     variable.factor = FALSE,
                     value.name = c("or", "e1","e2","e3","e4"))
    c1.isric[,variable := sort(names(isric))[as.integer(variable)]]
    c1.isric[,value := as.numeric(or)]
    c1.isric[or < 1 & e1 > 1,value := e1]
    c1.isric[or < 1 & e2 > 1,value := e2]
    c1.isric[or < 1 & e3 > 1,value := e3]
    c1.isric[or < 1 & e4 > 1,value := e4]

    c2.isric <- dcast(c1.isric,ID~variable, value.var = 'value')


    # --- extract climate data ----


    # extract climate data nc files
    d1.climate <- terra::extract(x = climate, y = s1)

    # convert to data.table
    d1.climate <- as.data.table(d1.climate)

    # rearrange data
    d2.climate <- melt(d1.climate,id.vars = 'ID', variable.name = 'variable')
    d2.climate <- as.data.table(d2.climate)
    d2.climate <- d2.climate[!grepl('_stn_',variable)]
    d2.climate <- d2.climate[cvar %in% c('pet','pre','tmp')]
    d2.climate <- d2.climate[!grepl('mae|maea',variable)]
    d2.climate[, cvar :=  stringr::str_extract_all(variable,"(?<=[0-9]{4}\\.[0-9]{4}\\.).+(?=\\.dat_)",simplify = T)]
    d2.climate[, years :=  stringr::str_extract_all(variable,"[0-9]{4}\\.[0-9]{4}",simplify = T)]
    d2.climate[, month :=  stringr::str_extract_all(variable,"(?<=[a-z]{3}_)\\d+",simplify = T)]

    # estimate mean global climate properties over 1991-2019
    # temperature in degrees (mean = tmp, max = tmx, min = tmn)
    # potential evaporation in mm/day
    # precipitation in mm/month
    d3.climate <- dcast(d2.climate,ID+years+month~cvar,value.var = 'value')

    # add year
    d3.climate[,month := as.numeric(month)]
    d3.climate[,year := 1+ floor((as.numeric(month)-1)/12)]

    setorder(d3.climate,years,year,month)

    # estimate annual ET (mm / year) and PRE (mm / year) per year
    d3.climate<- d3.climate[,list(pet = sum(pet * 30),
                                  pre = sum(pre),
                                  tmp = mean(tmp)),by=c('ID','years','year')]

    # derive the mean and SD per gridcel over period 1991-2019
    c1.climate <- d3.climate[,list(pre_mean = mean(pre),
                                   tmp_mean = mean(tmp),
                                   pet_mean = mean(pet)),by='ID']
    c2.climate <- copy(c1.climate)

    # --- extract metzger data

    # extract for measurement points
    d1.metzger <- terra::extract(x = metzger, y = s1)
    d2.metzger <- terra::extract(x = metzger, y = buffer(s1,width = 10000), fun = mean, na.rm=T)
    d2.metzger <- as.data.table(d2.metzger)
    d3.metzger <- merge(as.data.table(d1.metzger),
                        d2.metzger[,.(ID,gens_v3B= round(gens_v3))],by='ID',all.x=TRUE)
    d3.metzger <- d3.metzger[,.(ID,gens_v3 = fifelse(is.na(gens_v3),gens_v3B,gens_v3))]
    d3.metzger[,gens_v3 := nafill(gens_v3,type='locf')]

    # read metzger decription
    s2.dt <- as.data.table(s2)

    # merge description
    d1.metzger <- as.data.table(d3.metzger)
    c2.metzger <- merge(d1.metzger,s2.dt,by.x = 'gens_v3', by.y = 'GEnS_seq')

    # subset
    c2.metzger <- c2.metzger[,.(ID,GEnZname,GEnZ,GEnS)]

    # merge the data files

    dt <- d1[,.(ID = id,lon = lon,lat = lat, study_id)]
    dt <- merge(dt,c2.isric, by = 'ID',all.x=TRUE)
    dt <- merge(dt,c2.climate, by = 'ID',all.x=TRUE)
    dt <- merge(dt,c2.metzger,by='ID',all.x=TRUE)

    # save the file
    fwrite(dt,'products/250804 covariates yield metaanalysis.csv')
  } else {

    # read in the preprocessed covariates for soil, climate and weather
    d1.cov <- fread('products/250804 covariates yield metaanalysis.csv')
  }

  # combine covariates with source data
  d2 <- merge(d1,d1.cov,by.x = c('id','lon','lat','study_id'),by.y = c('ID','lon','lat','study_id'),all.x=TRUE)

# --- cleaning up the database ----

  # estimate missing years
  d2[, ref_year := as.numeric(stringr::str_extract(reference,'[0-9]+'))]
  d2[ref_year < 100, ref_year := 2013]
  d2[is.na(year), year := ref_year -3]

  # remove reference and location (not used for regression)
  d2[,c('reference','location','注') := NULL]

  # adjust annual weather conditions (no need, all data is there)
  d2[,c('pre_mean','tmp_mean') := NULL]

  # adjust soil properties (estimates via linear regression model given this dataset)

    # adjust soil pH (R2 = 0.57) and remove covariate
    d2[is.na(ph), ph := 3.1854527 + 0.0582424 * phw_mean_0_5]
    d2[,c('phw_mean_0_5','phw_mean_15_30','phw_mean_5_15') := NULL]

    # adjust SOC (R2 = 0.61) and remove covariate
    d2[is.na(soc), soc := exp(-0.919741 + 0.634093 * log(soc_mean_0_5))]
    d2[,c('soc_mean_0_5','soc_mean_15_30','soc_mean_5_15') := NULL]

    # adjust total N (R2 = 0.65), set to unit mg/kg and remove covariate
    d2[tn <= 1000, tn := tn * 1000]
    d2[is.na(tn),tn := exp(3.658378 + 0.626721 * log(ntot_mean_0_5))]
    d2[,c('ntot_mean_0_5','ntot_mean_15_30','ntot_mean_5_15') := NULL]

    # adjust clay content (R2 = 0.55)
    d2[is.na(clay),clay := 8.01717 + 0.0719998 * clay_mean_0_5]
    d2[,c('clay_mean_0_5','clay_mean_15_30','clay_mean_5_15') := NULL]

    # set sand content
    d2[,sand := sand_mean_0_5 * 0.1]
    d2[,c('sand_mean_0_5','sand_mean_15_30','sand_mean_5_15') := NULL]
    d2[,c('silt_mean_0_5','silt_mean_15_30','silt_mean_5_15') := NULL]

    # estimate bulk density (R2 = 0.75), set unit kg/m3
    d2[,bd := bd * 1000]
    d2[is.na(bd), bd := 781 + 19.57 * bdod_mean_0_5 - 0.2567*bdod_mean_0_5^2 + 0.001058 * bdod_mean_0_5^3]
    d2[,c('bdod_mean_0_5','bdod_mean_15_30','bdod_mean_5_15') := NULL]

    # set the CEC
    d2[, cec := cec_mean_0_5]
    d2[,c('cec_mean_0_5','cec_mean_15_30','cec_mean_5_15') := NULL]

    # set the P and K dose
    d2[,p_dose_cropmean := median(p_dose,na.rm=T),by=.(crop_type)]
    d2[,k_dose_cropmean := median(p_dose,na.rm=T),by=.(crop_type)]
    d2[,n_dose_cropmean := median(n_dose,na.rm=T),by=.(crop_type)]
    d2[is.na(p_dose), p_dose := p_dose_cropmean]
    d2[is.na(k_dose), k_dose := k_dose_cropmean]
    d2[is.na(n_dose), n_dose := n_dose_cropmean]
    d2[,c('p_dose_cropmean','k_dose_cropmean','n_dose_cropmean') := NULL]

    # reset names for management
    setnames(d2,c('management','managemeno tillage_detial'),c('man','man_detail'))

    # remove fertilizer type detail (no additional value)
    d2[,fertilizer_type_detial := NULL]

    # estimate missing values for yield effects
    d2[,yield_t_cv := yield_t_sd / yield_t_mean]
    d2[,yield_c_cv := yield_c_sd / yield_c_mean]
    d2[,yield_t_cv_mean := mean (yield_t_cv,na.rm=T),by='crop_type']
    d2[,yield_c_cv_mean := mean (yield_c_cv,na.rm=T),by='crop_type']
    d2[is.na(yield_t_sd), yield_t_sd := yield_t_mean * yield_t_cv_mean * 1.25]
    d2[is.na(yield_c_sd), yield_c_sd := yield_c_mean * yield_c_cv_mean * 1.25]
    d2[,yield_t_se := yield_t_sd / sqrt(replication)]
    d2[,yield_c_se := yield_c_sd / sqrt(replication)]
    d2[,c('yield_t_cv','yield_c_cv','yield_t_cv_mean','yield_c_cv_mean') := NULL]

    # rename the current management practices (i.e. NOT the treatment applied, but derived from paper)
    setnames(d2,
             old= c('biochar','fertilizer_type','crop_residue','tillage',
                       'cover_crop','crop_rotation','RFP','RFT','RFR'),
             new = c('cmp_biochar','cmp_ferttype','cmp_cropresidue','cmp_tillage',
                     'cmp_covercrop','cmp_croprotation','cmp_rfp','cmp_rft','cmp_rfr'))

# --- calculate effect size ----

    # estimate Standardized Mean Difference
    d2[, c('smd.yi','smd.vi') := as.list(metafor::escalc(measure = "SMD", data = d2,
                                                      m1i = yield_t_mean, sd1i = yield_t_sd, n1i = replication,
                                                      m2i = yield_c_mean, sd2i = yield_c_sd, n2i = replication,
                                                      append = FALSE))]
    d2[, c('md.yi','md.vi') := as.list(metafor::escalc(measure = "MD", data = d2,
                                                       m1i = yield_t_mean, sd1i = yield_t_sd, n1i = replication,
                                                       m2i = yield_c_mean, sd2i = yield_c_sd, n2i = replication,
                                                       append = FALSE))]
    d2[, c('rom.yi','rom.vi') := as.list(metafor::escalc(measure = "ROM", data = d2,
                                                       m1i = yield_t_mean, sd1i = yield_t_sd, n1i = replication,
                                                       m2i = yield_c_mean, sd2i = yield_c_sd, n2i = replication,
                                                       append = FALSE))]

    # remove cases that are extremes (SMD > 10)
    d2 <- d2[abs(smd.yi)<=10]

    # save the file
    saveRDS(d2,'products/250804 cropyield database.rds')
