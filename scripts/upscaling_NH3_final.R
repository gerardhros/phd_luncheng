# upscaling results for global impacts of management on N losses based on the MD model of Luncheng

#################################################################################
  library(terra)
  library(data.table)
  library(metafor)

  # Luncheng add

  # read the raster with climatic data 读取带有气候数据的栅格
  r.clim <- terra::rast('data/climate.tif')

  # read the raster with cropping data
  r.crop <- rast('data/ma_crops.tif')

  # read the raster with soil data
  r.soil <- terra::rast('data/soil.tif')

  # read the earlier prepared file with tillage practices
  r.till <- terra::rast('data/tillage.tif')

  # read the earlier prepared file with manure N dose
  r.nman <- terra::rast('data/nofert.tif')

  # read the earlier prepared file with fertilizer N dose
  r.nfert <- terra::rast('data/nifert.tif')

  # read the earlier prepared file with cropping intensity
  r.cropint <- terra::rast('data/cropintensity.tif')

#########################################################################
# add all rasters 添加所有的位图

    # clear environment
    rm(list= ls())

    # what rasters are in data
    rfiles <- list.files('data', pattern = 'tif$',full.names = TRUE)
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
    r.dt.melt[,cropname := c('rice','maiz','other','wheat')[as.numeric(croptype)]]

    # set names to tillage practices
    r.dt.melt[, till_name := 'conventional']
    r.dt.melt[tillage %in% c(3,4,7), till_name := 'no-till']

# derive the meta-analytical model

    # read data
    d1 <- readxl::read_xlsx('data/You_paper3_S2.xlsx',sheet = 3)
    d1 <- as.data.table(d1)


    # add CV for N losses treatment and estimate the SD for missing ones
    d2<-d1
    CV_nh3t_bar<-mean(d2$nh3t_sd[is.na(d2$nh3t_sd)==FALSE]/d2$nh3t_mean[is.na(d2$nh3t_sd)==FALSE])
    d2$nh3t_sd[is.na(d2$nh3t_sd)==TRUE]<-d2$nh3t_mean[is.na(d2$nh3t_sd)==TRUE]*1.25*CV_nh3t_bar

    CV_nh3c_bar<-mean(d2$nh3c_sd[is.na(d2$nh3c_sd)==FALSE]/d2$nh3c_mean[is.na(d2$nh3c_sd)==FALSE])
    d2$nh3c_sd[is.na(d2$nh3c_sd)==TRUE]<-d2$nh3c_mean[is.na(d2$nh3c_sd)==TRUE]*1.25*CV_nh3c_bar


    # clean up column names
    setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
    setnames(d2,tolower(colnames(d2)))


    # calculate effect size (N losses)
    es21 <- escalc(measure = "ROM", data = d2,
                   m1i = nh3t_mean, sd1i = nh3t_sd, n1i = replication,
                   m2i = nh3c_mean, sd2i = nh3c_sd, n2i = replication )
    # convert to data.tables
    d02 <- as.data.table(es21)

    # what are the treatments to be assessed
    d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))


    # update the missing values for n_dose and p2o5_dose (as example)
    d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]

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
    d02[,bc := fifelse(management== 'BC'| biochar == 'yes','yes','no')]
    d02[,rfr := fifelse(management=='RFR' | rfr == 'yes','yes','no')]
    d02[,rft := fifelse(management=='RFT' | rft == 'yes','yes','no')]
    d02[,rfp := fifelse(management=='RFP'|rfp =='yes','yes','no')]

    d02[,res := fifelse(management=='RES' | crop_residue == 'yes','yes','no')] #crop management measures
    #d02[,cc := fifelse(management=='CC' | cover_crop == 'yes','yes','no')]
    d02[,rot := fifelse(management=='ROT' | crop_rotation == 'yes','yes','no')]

    d02[,nt := fifelse(management=='NT' | tillage == 'NT','yes','no')] #soil management measures
    d02[,rt := fifelse(management=='RT'| tillage == 'RT','yes','no')]
    d02[,ct := ifelse(!management %in% c('NT','RT') | tillage == 'CT','yes','no')]

    # update the database (it looks like typos)
    d02[crop_type=='marize', crop_type := 'maize']
    d02[crop_type=='vegetables', crop_type := 'vegetable']

    #Combining different factors
    d02[crop_type=='vegetable', crop_type := 'other']
    # d02[tillage=='reduced', tillage := 'no-till']

    m1 <- rma.mv(yi,vi,
                 mods = ~ee + cf + of + rfr + rft + rfp + res + rot + nt + n_dose_scaled + crop_type +
                   ph_scaled + clay_scaled + soc_scaled + map_scaled + mat_scaled + cf : n_dose_scaled + cf : soc_scaled +
                   rot : map_scaled - 1,
                 data = d02,
                 random = list(~ 1|studyid), method="REML",sparse = TRUE)


    # see model structure that need to be filled in to predict N losses as function of the system properties
    p1 <- predict(m1,addx=T)

    # this is the order of input variables needed for model predictions (=newmods in predict function)
    m1.cols <- colnames(p1$X)

    # make prediction dataset for situation that soil is fertilized by both organic and inorganic fertilizers, conventional fertilizer strategy
    dt.new <- copy(r.dt.melt)

    # add the columns required for the ma model, baseline scenario
    # baseline is here defined as "strategy conventional", and mineral fertilizers, no biochar, no crop residue, no cover crops
    dt.new[, eeno := 1]
    dt.new[, eeyes := 0]
    dt.new[, cfyes := 0]
    dt.new[, ofyes := 0]
    dt.new[, rfryes := 0]
    dt.new[, rftyes := 0]
    dt.new[, rfpyes := 0]
    #dt.new[, bcyes := 0]
    dt.new[, resyes := 0]
    dt.new[, rotyes := 0]
    dt.new[, ntyes := 0]
    dt.new[, n_dose_scaled := scale(nh4+no3+nam)]
    dt.new[, crop_typeother := fifelse(cropname=='other',1,0)]
    dt.new[, crop_typerice := fifelse(cropname=='rice',1,0)]
    dt.new[, crop_typewheat := fifelse(cropname=='wheat',1,0)]
    dt.new[, crop_typemaize := fifelse(cropname=='maize',1,0)]
    dt.new[, ph_scaled := (phw * 0.1 - mean(d02$ph)) / sd(d02$ph)]
    dt.new[, clay_scaled := (clay * 0.1 - mean(d02$clay)) / sd(d02$clay)]
    dt.new[, soc_scaled := (soc * 0.1 - mean(d02$soc)) / sd(d02$soc)]
    dt.new[, map_scaled := (pre - mean(d02$map)) / sd(d02$map)]
    dt.new[, mat_scaled := (mat  - mean(d02$mat)) / sd(d02$mat)]
    dt.new[, `cfyes:n_dose_scaled` := cfyes*n_dose_scaled]
    dt.new[, `cfyes:soc_scaled` := cfyes*soc_scaled]
    dt.new[, `rotyes:map_scaled` := rotyes*map_scaled]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.new[,mget(c(m1.cols))])

    # predict the N losses via MD model
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
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1
    dt.s1[, eeyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s1[,mget(c(m1.cols))])

    # predict the N losses via MD model
    dt.pred.s1 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s1[,c(cols) := dt.pred.s1]
    dt.s1[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s1[,.(x,y,s1 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s1 - base]

    # estimate area weighted mean relative improvement
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement

    # convert to spatial raster
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_ee_1.tif', overwrite = TRUE)



    ############################################## scenario S2 (CF) ################################################################
    # make local copy
    dt.s2 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1
    dt.s2[, cfyes := 1]

    # convert to matrix, needed for rma models
    dt.newmod <- as.matrix(dt.s2[,mget(c(m1.cols))])

    # predict the N losses via MD model
    dt.pred.s2 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s2[,c(cols) := dt.pred.s2]
    dt.s2[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s2[,.(x,y,s2 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s2 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_cf_2.tif', overwrite = TRUE)


    ############################################## scenario S3 (OF) ################################################################
    # make local copy 制作当地的副本
    dt.s3 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1 更新场景1的操作
    dt.s3[, ofyes := 1]

    # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    dt.newmod <- as.matrix(dt.s3[,mget(c(m1.cols))])

    # predict the N losses via MD model 利用MD模型预测N losses
    dt.pred.s3 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s3[,c(cols) := dt.pred.s3]
    dt.s3[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s3[,.(x,y,s3 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s3 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_of_3.tif', overwrite = TRUE)


    ############################################## scenario S4 (RFR) ################################################################
    # make local copy 制作当地的副本
    dt.s4 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1 更新场景1的操作
    dt.s4[, rfryes := 1]

    # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    dt.newmod <- as.matrix(dt.s4[,mget(c(m1.cols))])

    # predict the N losses via MD model 利用MD模型预测N losses
    dt.pred.s4 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s4[,c(cols) := dt.pred.s4]
    dt.s4[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s4[,.(x,y,s4 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s4 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rfr_4.tif', overwrite = TRUE)

    ############################################## scenario s5 (RFT) ################################################################
    # make local copy 制作当地的副本
    dt.s5 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1 更新场景1的操作
    dt.s5[, rftyes := 1]

    # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    dt.newmod <- as.matrix(dt.s5[,mget(c(m1.cols))])

    # predict the N losses via MD model 利用MD模型预测N losses
    dt.pred.s5 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s5[,c(cols) := dt.pred.s5]
    dt.s5[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s5[,.(x,y,s5 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s5 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rft_5.tif', overwrite = TRUE)

    ############################################## scenario s6 (RFP) ################################################################
    # make local copy 制作当地的副本
    dt.s6 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1 更新场景1的操作
    dt.s6[, rfpyes := 1]

    # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    dt.newmod <- as.matrix(dt.s6[,mget(c(m1.cols))])

    # predict the N losses via MD model 利用MD模型预测N losses
    dt.pred.s6 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s6[,c(cols) := dt.pred.s6]
    dt.s6[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s6[,.(x,y,s6 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s6 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rfp_6.tif', overwrite = TRUE)

    #
    # ############################################## scenario s7 (BC) ################################################################
    # # make local copy 制作当地的副本
    # dt.s7 <- copy(dt.new)
    #
    # # baseline mean and sd for total N input
    # dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]
    #
    # # update actions taken for scenario 1 更新场景1的操作
    # dt.s7[, bcyes := 1]
    #
    # # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    # dt.newmod <- as.matrix(dt.s7[,mget(c(m1.cols))])
    #
    # # predict the N losses via MD model 利用MD模型预测N losses
    # dt.pred.s7 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    # dt.s7[,c(cols) := dt.pred.s7]
    # dt.s7[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # # compare baseline with scenario 比较"基线"和"场景"
    #
    # # compare baseline with scenario
    #
    # # select relevant columns of the baseline
    # dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]
    #
    # # select relevant columns of scenario 1 and merge
    # dt.fin <- merge(dt.fin,dt.s7[,.(x,y,s7 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))
    #
    # # estimate relative improvement via senario 1
    # dt.fin[, improvement := s7 - base]
    #
    # # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    # dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]
    #
    #
    # # make spatial raster of the estimated improvement 对空间栅格进行估计改进
    #
    # # convert to spatial raster 转换为空间光栅
    # r.fin <- terra::rast(dt.fin,type='xyz')
    # terra::crs(r.fin) <- 'epsg:4326'
    #
    # # write as output
    # terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_bc_7.tif', overwrite = TRUE)

    #
    # ############################################## scenario s8 (RES) ################################################################
    # # make local copy 制作当地的副本
    # dt.s8 <- copy(dt.new)
    #
    # # baseline mean and sd for total N input
    # dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]
    #
    # # update actions taken for scenario 1 更新场景1的操作
    # dt.s8[, resyes := 1]
    #
    # # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    # dt.newmod <- as.matrix(dt.s8[,mget(c(m1.cols))])
    #
    # # predict the N losses via MD model 利用MD模型预测N losses
    # dt.pred.s8 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    # dt.s8[,c(cols) := dt.pred.s8]
    # dt.s8[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # # compare baseline with scenario 比较"基线"和"场景"
    #
    # # compare baseline with scenario
    #
    # # select relevant columns of the baseline
    # dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]
    #
    # # select relevant columns of scenario 1 and merge
    # dt.fin <- merge(dt.fin,dt.s8[,.(x,y,s8 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))
    #
    # # estimate relative improvement via senario 1
    # dt.fin[, improvement := s8 - base]
    #
    # # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    # dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]
    #
    #
    # # make spatial raster of the estimated improvement 对空间栅格进行估计改进
    #
    # # convert to spatial raster 转换为空间光栅
    # r.fin <- terra::rast(dt.fin,type='xyz')
    # terra::crs(r.fin) <- 'epsg:4326'
    #
    # # write as output
    # terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_res_8.tif', overwrite = TRUE)

    ############################################## scenario s10 (ROT) ################################################################
    # make local copy 制作当地的副本
    dt.s10 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1 更新场景1的操作
    dt.s10[, rotyes := 1]

    # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    dt.newmod <- as.matrix(dt.s10[,mget(c(m1.cols))])

    # predict the N losses via MD model 利用MD模型预测N losses
    dt.pred.s10 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s10[,c(cols) := dt.pred.s10]
    dt.s10[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s10[,.(x,y,s10 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s10 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rot_10.tif', overwrite = TRUE)


    ############################################## scenario s11 (NT) ################################################################
    # make local copy 制作当地的副本
    dt.s11 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1 更新场景1的操作
    dt.s11[, ntyes := 1]
    # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    dt.newmod <- as.matrix(dt.s11[,mget(c(m1.cols))])

    # predict the N losses via MD model 利用MD模型预测N losses
    dt.pred.s11 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s11[,c(cols) := dt.pred.s11]
    dt.s11[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s11[,.(x,y,s11 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s11 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_nt_11.tif', overwrite = TRUE)

  ############### scenario run ((combined optimal management practices: EE,CF,RFR,RFT,NT)) ###########################

    # make local copy 制作当地的副本
    dt.s0 <- copy(dt.new)

    # baseline mean and sd for total N input
    dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

    # update actions taken for scenario 1 更新场景1的操作
    dt.s0[, eeno := 1]
    dt.s0[, eeyes := 1]
    dt.s0[, cfyes := 1]
    dt.s0[, ofyes := 1]
    dt.s0[, rfryes := 1]
    dt.s0[, rftyes := 1]
    dt.s0[, rfpyes := 1]
    #dt.s0[, bcyes := 0]
    dt.s0[, resyes := 0]
    dt.s0[, rotyes := 1]
    dt.s0[, ntyes := 0]

    # convert to matrix, needed for rma models 转换为rma模型所需的矩阵
    dt.newmod <- as.matrix(dt.s0[,mget(c(m1.cols))])

    # predict the N losses via MD model 利用MD模型预测N losses
    dt.pred.s0 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s0[,c(cols) := dt.pred.s0]
    dt.s0[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
    # compare baseline with scenario 比较"基线"和"场景"

    # compare baseline with scenario

    # select relevant columns of the baseline
    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs,cropname,area)]

    # select relevant columns of scenario 1 and merge
    dt.fin <- merge(dt.fin,dt.s0[,.(x,y,s0 = pROMmeanAbs,cropname)],by=c('x','y','cropname'))

    # estimate relative improvement via senario 1
    dt.fin[, improvement := s0 - base]

    # estimate area weighted mean relative improvement 估计面积加权平均相对改进
    dt.fin <- dt.fin[,list(improvement = weighted.mean(improvement,w = area)),by = c('x','y')]


    # make spatial raster of the estimated improvement 对空间栅格进行估计改进

    # convert to spatial raster 转换为空间光栅
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'products/scenario_nh3_combination.tif', overwrite = TRUE)

