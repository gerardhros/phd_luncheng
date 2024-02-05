
require(sf);require(terra);require(data.table);require(metafor)

rm(list=ls())

# load all current practices and site properties

r.soil <- terra::rast('.../soil.tif')
r.clim <- terra::rast('.../climate.tif')
r.agtech <- terra::rast('.../r.agtech.tif')
r.nue <- terra::rast('.../r.nue.tif')
r.nman <- terra::rast('.../nofert.tif')
r.nfert <- terra::rast('.../nifert.tif')
r.cwheat <- terra::rast('.../r.cwheat.tif')
r.cmaize <- terra::rast('.../r.cmaize.tif')
r.crice <- terra::rast('.../r.crice.tif')
r.cother <- terra::rast('.../r.cother.tif')
r.notill <- terra::rast('.../r.notill.tif')
r.cropint <- terra::rast('.../r.cropint.tif')
r.ccrop <- terra::rast('.../r.ccrop.tif')
r.residue <- terra::rast('.../r.residue.tif')
r.ndmloss <- terra::rast('.../r.ndmloss.tif')
r.all <- c(r.soil,r.clim,r.agtech,r.nue,r.cwheat,r.cmaize,r.crice,r.cother,r.notill,r.cropint,r.ccrop,r.residue,r.ndmloss, r.nman, r.nfert)
r.dt <- as.data.table(as.data.frame(r.all,xy=TRUE, na.rm = FALSE))

setnames(r.dt,
         old = c('climate_mat', 'climate_pre','isric_phw_mean_0_5','isric_clay_mean_0_5','isric_soc_mean_0_5',
                 'nifert_nfert_nh4','nifert_nfert_no3','nofert_nofert'),
         new = c('mat','pre','phw','clay','soc','nh4','no3','nam'),skip_absent = T)

r.dt <- r.dt[!(is.na(mat)|is.na(pre))]
r.dt <- r.dt[!(is.na(cwheat) & is.na(cmaize) & is.na(crice) & is.na(cother))]
r.dt[,nue_ag := fifelse(is.na(nue_ag),median(nue_ag,na.rm=T),nue_ag)]
r.dt[is.na(NOTILL), NOTILL := 0]
r.dt[is.na(CROPINT), CROPINT := 0]
r.dt[is.na(CCROP), CCROP := 0]
r.dt[is.na(RES), RES := 0]
r.dt[is.na(ndm_loss_cat), ndm_loss_cat := 0]
r.dt[is.na(nfert_nh4), nfert_nh4 := 0]
r.dt[is.na(nfert_no3), nfert_no3 := 0]
r.dt[,c('cropintensity','ccID') := NULL]
r.dt$nue_ag <- ifelse(r.dt$nue_ag < 0.5, 1, 0)

#
    # read data  
    d1 <- readxl::read_xlsx('.../You_paper3_S2.xlsx',sheet = "NH3")
    d1 <- as.data.table(d1)

    d2<-d1
    CV_nh3t_bar<-mean(d2$nh3t_sd[is.na(d2$nh3t_sd)==FALSE]/d2$nh3t_mean[is.na(d2$nh3t_sd)==FALSE])
    d2$nh3t_sd[is.na(d2$nh3t_sd)==TRUE]<-d2$nh3t_mean[is.na(d2$nh3t_sd)==TRUE]*1.25*CV_nh3t_bar
    CV_nh3c_bar<-mean(d2$nh3c_sd[is.na(d2$nh3c_sd)==FALSE]/d2$nh3c_mean[is.na(d2$nh3c_sd)==FALSE])
    d2$nh3c_sd[is.na(d2$nh3c_sd)==TRUE]<-d2$nh3c_mean[is.na(d2$nh3c_sd)==TRUE]*1.25*CV_nh3c_bar
    setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
    setnames(d2,tolower(colnames(d2)))
    
    # calculate effect size
    es21 <- escalc(measure = "ROM", data = d2, 
                   m1i = nh3t_mean, sd1i = nh3t_sd, n1i = replication,
                   m2i = nh3c_mean, sd2i = nh3c_sd, n2i = replication )
    d02 <- as.data.table(es21)
    d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))
    d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]
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
    d02[,res := fifelse(management=='RES' | crop_residue == 'yes','yes','no')]
    d02[,rot := fifelse(management=='ROT' | crop_rotation == 'yes','yes','no')]
    d02[,nt := fifelse(management=='NT' | tillage == 'NT','yes','no')]
    d02[,rt := fifelse(management=='RT'| tillage == 'RT','yes','no')]
    d02[,ct := ifelse(!management %in% c('NT','RT') | tillage == 'CT','yes','no')]
    d02[crop_type=='marize', crop_type := 'maize']
    d02[crop_type=='vegetables', crop_type := 'vegetable']
    d02[crop_type=='vegetable', crop_type := 'other']

    m1 <- rma.mv(yi,vi,
                 mods = ~ee + cf + of + rfr + rft + rfp + res + rot + nt + n_dose_scaled + crop_type +
                   ph_scaled + clay_scaled + soc_scaled + map_scaled + mat_scaled + 
                   cf : n_dose_scaled + res : clay_scaled - 1,
                 data = d02,
                 random = list(~ 1|studyid), method="REML",sparse = TRUE)
    
 
    m1
    p1 <- predict(m1,addx=T)
    m1.cols <- colnames(p1$X)
    dt.new <- copy(r.dt)
    dt.new[, eeno := fifelse(TECH_LOW == 1,1,0)]
    dt.new[, eeyes :=  fifelse(TECH_LOW == 1,0,1)]
    dt.new[, cfyes := fifelse(ndm_loss_cat == 1,0,1)]
    dt.new[, ofyes := fifelse(ndm_loss_cat == 1,0,1)]
    dt.new[, rfryes := fifelse(nue_ag == 1,0,1)]
    dt.new[, rftyes :=  fifelse(TECH_LOW == 1,0,1)] 
    dt.new[, rfpyes := fifelse(TECH_LOW == 1,0,1)]
    dt.new[, resyes := fifelse(RES == 1,0,1)] 
    dt.new[, rotyes := fifelse(CROPINT == 1,0,1)]
    dt.new[, ntyes := fifelse(NOTILL == 1,0,1)] 
    dt.new[, n_dose_scaled := scale(nfert_nh4+nfert_no3+nofert)]
    dt.new[, crop_typeother := cother]
    dt.new[, crop_typerice := crice]
    dt.new[, crop_typewheat := cwheat]
    dt.new[, crop_typemaize := cmaize]
    dt.new[, ph_scaled := (phw * 0.1 - mean(d02$ph)) / sd(d02$ph)]
    dt.new[, clay_scaled := (clay * 0.1 - mean(d02$clay)) / sd(d02$clay)]
    dt.new[, soc_scaled := (soc * 0.1 - mean(d02$soc)) / sd(d02$soc)]
    dt.new[, map_scaled := (pre - mean(d02$map)) / sd(d02$map)]
    dt.new[, mat_scaled := (mat  - mean(d02$mat)) / sd(d02$mat)]
    dt.new[, `cfyes:n_dose_scaled` := cfyes*n_dose_scaled]
    dt.new[, `resyes:clay_scaled` := resyes*clay_scaled]
    dt.newmod <- as.matrix(dt.new[,mget(c(m1.cols))])

    dt.pred <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))

     cols <- c('pROMmean','pROMse','pROMcil','pROMciu','pROMpil','pROMpiu')
     
     dt.new[,c(cols) := dt.pred]
     dt.new[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    #EE

    dt.s1 <- copy(dt.new)

     dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    dt.s1[, eeyes := 1]

    dt.newmod <- as.matrix(dt.s1[,mget(c(m1.cols))])

    dt.pred.s1 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s1[,c(cols) := dt.pred.s1]

    dt.s1[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

    dt.fin <- merge(dt.fin,dt.s1[,.(x,y,s1 = pROMmeanAbs)],by=c('x','y'))

    dt.fin[, improvement := s1 - base]

    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_ee_1.tif', overwrite = TRUE)
    
     #CF
    dt.s2 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    dt.s2[, cfyes := 1]

    dt.newmod <- as.matrix(dt.s2[,mget(c(m1.cols))])

    dt.pred.s2 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s2[,c(cols) := dt.pred.s2]
    dt.s2[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

    dt.fin <- merge(dt.fin,dt.s2[,.(x,y,s2 = pROMmeanAbs)],by=c('x','y'))

    dt.fin[, improvement := s2 - base]

    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_cf_2.tif', overwrite = TRUE)
    
    
    #OF
    dt.s3 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]
    
    dt.s3[, ofyes := 1]

    dt.newmod <- as.matrix(dt.s3[,mget(c(m1.cols))])
    
    dt.pred.s3 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s3[,c(cols) := dt.pred.s3]
    dt.s3[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]
    
    dt.fin <- merge(dt.fin,dt.s3[,.(x,y,s3 = pROMmeanAbs)],by=c('x','y'))
    
    dt.fin[, improvement := s3 - base]
    
    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]
    
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_of_3.tif', overwrite = TRUE)
    
    
    #RFR
    dt.s4 <- copy(dt.new)
    
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]
    
    dt.s4[, rfryes := 1]
    dt.newmod <- as.matrix(dt.s4[,mget(c(m1.cols))])
    
    dt.pred.s4 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s4[,c(cols) := dt.pred.s4]
    dt.s4[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]
    
    dt.fin <- merge(dt.fin,dt.s4[,.(x,y,s4 = pROMmeanAbs)],by=c('x','y'))
    
    dt.fin[, improvement := s4 - base]

    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]
    
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_rfr_4.tif', overwrite = TRUE)
    
    #RFT
    dt.s5 <- copy(dt.new)
    
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]
    
    dt.s5[, rftyes := 1]

    dt.newmod <- as.matrix(dt.s5[,mget(c(m1.cols))])
    
    dt.pred.s5 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s5[,c(cols) := dt.pred.s5]
    dt.s5[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]
    
    dt.fin <- merge(dt.fin,dt.s5[,.(x,y,s5 = pROMmeanAbs)],by=c('x','y'))
    
    dt.fin[, improvement := s5 - base]
    
    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]
    
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_rft_5.tif', overwrite = TRUE)
    
    #RFP
    dt.s6 <- copy(dt.new)
    
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]
    
    dt.s6[, rfpyes := 1]

    dt.newmod <- as.matrix(dt.s6[,mget(c(m1.cols))])
    
    dt.pred.s6 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s6[,c(cols) := dt.pred.s6]
    dt.s6[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]
    
    dt.fin <- merge(dt.fin,dt.s6[,.(x,y,s6 = pROMmeanAbs)],by=c('x','y'))
    
    dt.fin[, improvement := s6 - base]

    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]
    
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_rfp_6.tif', overwrite = TRUE)
    
    #RES
    dt.s8 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    dt.s8[, resyes := 1]

    dt.newmod <- as.matrix(dt.s8[,mget(c(m1.cols))])

    dt.pred.s8 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s8[,c(cols) := dt.pred.s8]
    dt.s8[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

    dt.fin <- merge(dt.fin,dt.s8[,.(x,y,s8 = pROMmeanAbs)],by=c('x','y'))

    dt.fin[, improvement := s8 - base]

    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]
    
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_res_8.tif', overwrite = TRUE)

    #ROT
    dt.s10 <- copy(dt.new)
    
    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]
    
    dt.s10[, rotyes := 1]
    dt.newmod <- as.matrix(dt.s10[,mget(c(m1.cols))])
    
    dt.pred.s10 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s10[,c(cols) := dt.pred.s10]
    dt.s10[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]
    dt.fin <- merge(dt.fin,dt.s10[,.(x,y,s10 = pROMmeanAbs)],by=c('x','y'))
    
    dt.fin[, improvement := s10 - base]
    
    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]
    
    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_rot_10.tif', overwrite = TRUE)
    
    
    #NT

    dt.s11 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    dt.s11[, ntyes := 1]

    dt.newmod <- as.matrix(dt.s11[,mget(c(m1.cols))])

    dt.pred.s11 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s11[,c(cols) := dt.pred.s11]
    dt.s11[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

    dt.fin <- merge(dt.fin,dt.s11[,.(x,y,s11 = pROMmeanAbs)],by=c('x','y'))

    dt.fin[, improvement := s11 - base]

    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'

    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_nt_11.tif', overwrite = TRUE)

  #combined optimal management practices

    dt.s0 <- copy(dt.new)

    dt.fert.bs <- dt.new[,list(mean = mean(nfert_nh4+nfert_no3+nofert), sd = sd(nfert_nh4+nfert_no3+nofert))]

    dt.s0[, eeyes := 1]
    dt.s0[, cfyes := 1]
    dt.s0[, ofyes := 1]
    dt.s0[, rfryes := 1]
    dt.s0[, rftyes := 1]
    dt.s0[, rfpyes := 1]
    dt.s0[, rotyes := 1]

    dt.newmod <- as.matrix(dt.s0[,mget(c(m1.cols))])

    dt.pred.s0 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
    dt.s0[,c(cols) := dt.pred.s0]
    dt.s0[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

    dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

    dt.fin <- merge(dt.fin,dt.s0[,.(x,y,s0 = pROMmeanAbs)],by=c('x','y'))

    dt.fin[, improvement := s0 - base]

    dt.fin[improvement < -100, improvement := NA]
    dt.fin[improvement > 100, improvement := NA]

    r.fin <- terra::rast(dt.fin,type='xyz')
    terra::crs(r.fin) <- 'epsg:4326'
    
    # write as output
    terra::writeRaster(r.fin,'.../scenario_nh3_combination.tif', overwrite = TRUE)
    
    