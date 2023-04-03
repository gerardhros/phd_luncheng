# Table 1, Table S5-S7

library(data.table)
library(metafor)
library(metagear)

##################################### ROM (metafor) ####################################################
d1 <- readxl::read_xlsx('Database.xlsx',sheet = "Tables")
d1 <- as.data.table(d1)

d2<-d1
CV_nuet_bar<-mean(d2$nuet_sd[is.na(d2$nuet_sd)==FALSE]/d2$nuet_mean[is.na(d2$nuet_sd)==FALSE])
d2$nuet_sd[is.na(d2$nuet_sd)==TRUE]<-d2$nuet_mean[is.na(d2$nuet_sd)==TRUE]*1.25*CV_nuet_bar

CV_nuec_bar<-mean(d2$nuec_sd[is.na(d2$nuec_sd)==FALSE]/d2$nuec_mean[is.na(d2$nuec_sd)==FALSE])
d2$nuec_sd[is.na(d2$nuec_sd)==TRUE]<-d2$nuec_mean[is.na(d2$nuec_sd)==TRUE]*1.25*CV_nuec_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

es21 <- escalc(measure = "ROM", data = d2, 
               m1i = nuet_mean, sd1i = nuet_sd, n1i = replication,
               m2i = nuec_mean, sd2i = nuec_sd, n2i = replication )

d02 <- as.data.table(es21)

d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))

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

out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){

    r_nue <- rma.mv(yi,vi, data=d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {

    r_nue <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  }

  out2[[i]] <- data.table(mean = as.numeric(r_nue$b),
                          se = as.numeric(r_nue$se),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
}

out2 <- rbindlist(out2)

forest(x = out2$mean,
       sei = out2$se, slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in NUE (%)", header="Treatment", col="#CC0000", lwd=2)

ranktest(out2$mean, out2$se)

regtest(out2$mean, out2$se)

d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]

d02[,clay_scaled := scale(clay)]
d02[,soc_scaled := scale(soc)]
d02[,ph_scaled := scale(ph)]
d02[,mat_scaled := scale(mat)]
d02[,map_scaled := scale(map)]
d02[,n_dose_scaled := scale(n_dose)]

var.site <- c('mat_scaled','map_scaled','clay_scaled','soc_scaled','ph_scaled')
var.crop <- c('g_crop_type','n_dose_scaled')

var.trea <- c('biochar', 'fertilizer_type', 'crop_residue', 'tillage', 'cover_crop_and_crop_rotation', 'fertilizer_strategy')

var.sel <- c(var.trea,var.crop,var.site)

r_nue_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)

out1.est = out1.sum = list()

for(i in var.sel){

  vartype = is.character(d02[,get(i)])

  if(vartype == TRUE){

    r_nue_1 <- rma.mv(yi,vi, 
                      mods = ~factor(varsel)-1, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {

    r_nue_1 <- rma.mv(yi,vi, 
                      mods = ~varsel, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
  }

  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)','',rownames(r_nue_1$b)),
                              mean = round(as.numeric(r_nue_1$b),3),
                              se = round(as.numeric(r_nue_1$se),3),
                              ci.lb = round(as.numeric(r_nue_1$ci.lb),3),
                              ci.ub = round(as.numeric(r_nue_1$ci.ub),3),
                              pval = round(as.numeric(r_nue_1$pval),3))

  out1.sum[[i]] <- data.table(var = i,
                              AIC = r_nue_1$fit.stats[4,2],
                              ll = r_nue_1$fit.stats[1,2],
                              ll_impr = round(100 * (1-r_nue_1$fit.stats[1,2]/r_nue_0$fit.stats[1,2]),2),
                              r2_impr = round(100*max(0,(sum(r_nue_0$sigma2)-sum(r_nue_1$sigma2))/sum(r_nue_0$sigma2)),2),
                              pval = round(anova(r_nue_1,r_nue_0)$pval,3)
  )
  
}

out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)
print(out1.sum)
print(out1.est)

estats <- function(model_new,model_base){
  out <- data.table(AIC = model_new$fit.stats[4,2],
                    ll = model_new$fit.stats[1,2],
                    ll_impr = round(100 * (1-model_new$fit.stats[1,2]/model_base$fit.stats[1,2]),2),
                    r2_impr = round(100*max(0,(sum(model_base$sigma2)-sum(model_new$sigma2))/sum(model_base$sigma2)),2),
                    pval = round(anova(r_nue_1,r_nue_0)$pval,3))
  return(out)
}

d02[crop_type=='marize', crop_type := 'maize']
d02[crop_type=='vegetables', crop_type := 'vegetable']

d02[g_crop_type=='vegetable', g_crop_type := 'other']
d02[tillage=='reduced', tillage := 'no-till']

d02[,fertilizer_type := factor(fertilizer_type,
                               levels = c('mineral','organic', 'combined','enhanced'))] 
d02[,fertilizer_strategy := factor(fertilizer_strategy,
                                   levels = c("conventional", "placement","rate","timing"))]
d02[,g_crop_type := factor(g_crop_type,
                           levels = c('other','maize','wheat','rice'))]

d4 <- copy(d02)

d4[,r4pl := fifelse(fertilizer_strategy=='placement','yes','no')]
d4[,r4ti := fifelse(fertilizer_strategy=='timing','yes','no')]
d4[,r4do := fifelse(fertilizer_strategy=='rate','yes','no')]
d4[,ctm := fifelse(g_crop_type=='maize','yes','no')]
d4[,ctw := fifelse(g_crop_type=='wheat','yes','no')]
d4[,ctr := fifelse(g_crop_type=='rice','yes','no')]
d4[,cto := fifelse(g_crop_type=='other','yes','no')]
d4[,ndose2 := scale(n_dose^2)]

r_nue_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)

r_nue_4 <- rma.mv(yi,vi,
                  mods = ~fertilizer_type + r4pl + r4ti + r4do + biochar + crop_residue + tillage +
                    cover_crop_and_crop_rotation + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled +
                    soc_scaled : n_dose_scaled + ctm:r4pl + ctm + ctw + ctr + cto + ctm:mat_scaled  + ndose2 -1,
                  data = d4,
                  random = list(~ 1|studyid/g_crop_type), method="REML",sparse = TRUE)

out = estats(model_new = r_nue_4,model_base = r_nue_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(r_nue_4)

k <- r_nue_4$k
wi <- 1/r_nue_4$vi
vt <- (k-1) / (sum(wi) - sum(wi^2)/sum(wi))
PR2 <- r_nue_0$sigma2 / (sum(r_nue_4$sigma2) + vt)

##################################### MD (metafor) ####################################################
theme_set(theme_bw())

d1 <- readxl::read_xlsx('Database.xlsx',sheet = "Tables")
d1 <- as.data.table(d1)

d2<-d1
CV_nuet_bar<-mean(d2$nuet_sd[is.na(d2$nuet_sd)==FALSE]/d2$nuet_mean[is.na(d2$nuet_sd)==FALSE])
d2$nuet_sd[is.na(d2$nuet_sd)==TRUE]<-d2$nuet_mean[is.na(d2$nuet_sd)==TRUE]*1.25*CV_nuet_bar

CV_nuec_bar<-mean(d2$nuec_sd[is.na(d2$nuec_sd)==FALSE]/d2$nuec_mean[is.na(d2$nuec_sd)==FALSE])
d2$nuec_sd[is.na(d2$nuec_sd)==TRUE]<-d2$nuec_mean[is.na(d2$nuec_sd)==TRUE]*1.25*CV_nuec_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

es21 <- escalc(measure = "MD", data = d2, 
               m1i = nuet_mean, sd1i = nuet_sd, n1i = replication,
               m2i = nuec_mean, sd2i = nuec_sd, n2i = replication )

d02 <- as.data.table(es21)

d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))

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

out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_nue <- rma.mv(yi,vi, data=d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {
    
    r_nue <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(mean = as.numeric(r_nue$b),
                          se = as.numeric(r_nue$se),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
}

out2 <- rbindlist(out2)

forest(x = out2$mean,
       sei = out2$se, slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in NUE (%)", header="Treatment", col="#CC0000", lwd=2)

ranktest(out2$mean, out2$se)

regtest(out2$mean, out2$se)

d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]

d02[,clay_scaled := scale(clay)]
d02[,soc_scaled := scale(soc)]
d02[,ph_scaled := scale(ph)]
d02[,mat_scaled := scale(mat)]
d02[,map_scaled := scale(map)]
d02[,n_dose_scaled := scale(n_dose)]

var.site <- c('mat_scaled','map_scaled','clay_scaled','soc_scaled','ph_scaled')
var.crop <- c('g_crop_type','n_dose_scaled')

var.trea <- c('biochar', 'fertilizer_type', 'crop_residue', 'tillage', 'cover_crop_and_crop_rotation', 'fertilizer_strategy')

var.sel <- c(var.trea,var.crop,var.site)

r_nue_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)

out1.est = out1.sum = list()

for(i in var.sel){
  
  vartype = is.character(d02[,get(i)])
  
  if(vartype == TRUE){
    
    r_nue_1 <- rma.mv(yi,vi, 
                      mods = ~factor(varsel)-1, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {
    
    r_nue_1 <- rma.mv(yi,vi, 
                      mods = ~varsel, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  
  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)','',rownames(r_nue_1$b)),
                              mean = round(as.numeric(r_nue_1$b),3),
                              se = round(as.numeric(r_nue_1$se),3),
                              ci.lb = round(as.numeric(r_nue_1$ci.lb),3),
                              ci.ub = round(as.numeric(r_nue_1$ci.ub),3),
                              pval = round(as.numeric(r_nue_1$pval),3))
  
  out1.sum[[i]] <- data.table(var = i,
                              AIC = r_nue_1$fit.stats[4,2],
                              ll = r_nue_1$fit.stats[1,2],
                              ll_impr = round(100 * (1-r_nue_1$fit.stats[1,2]/r_nue_0$fit.stats[1,2]),2),
                              r2_impr = round(100*max(0,(sum(r_nue_0$sigma2)-sum(r_nue_1$sigma2))/sum(r_nue_0$sigma2)),2),
                              pval = round(anova(r_nue_1,r_nue_0)$pval,3)
  )
  
}

out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)
print(out1.sum)
print(out1.est)

estats <- function(model_new,model_base){
  out <- data.table(AIC = model_new$fit.stats[4,2],
                    ll = model_new$fit.stats[1,2],
                    ll_impr = round(100 * (1-model_new$fit.stats[1,2]/model_base$fit.stats[1,2]),2),
                    r2_impr = round(100*max(0,(sum(model_base$sigma2)-sum(model_new$sigma2))/sum(model_base$sigma2)),2),
                    pval = round(anova(r_nue_1,r_nue_0)$pval,3))
  return(out)
}

d02[crop_type=='marize', crop_type := 'maize']
d02[crop_type=='vegetables', crop_type := 'vegetable']

d02[g_crop_type=='vegetable', g_crop_type := 'other']
d02[tillage=='reduced', tillage := 'no-till']

d02[,fertilizer_type := factor(fertilizer_type,
                               levels = c('mineral','organic', 'combined','enhanced'))] 
d02[,fertilizer_strategy := factor(fertilizer_strategy,
                                   levels = c("conventional", "placement","rate","timing"))]
d02[,g_crop_type := factor(g_crop_type,
                           levels = c('other','maize','wheat','rice'))]

d4 <- copy(d02)

d4[,r4pl := fifelse(fertilizer_strategy=='placement','yes','no')]
d4[,r4ti := fifelse(fertilizer_strategy=='timing','yes','no')]
d4[,r4do := fifelse(fertilizer_strategy=='rate','yes','no')]
d4[,ctm := fifelse(g_crop_type=='maize','yes','no')]
d4[,ctw := fifelse(g_crop_type=='wheat','yes','no')]
d4[,ctr := fifelse(g_crop_type=='rice','yes','no')]
d4[,cto := fifelse(g_crop_type=='other','yes','no')]
d4[,ndose2 := scale(n_dose^2)]

r_nue_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)

r_nue_4 <- rma.mv(yi,vi,
                  mods = ~fertilizer_type + r4pl + r4ti + r4do + biochar + crop_residue + tillage +
                    cover_crop_and_crop_rotation + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled +
                    soc_scaled : n_dose_scaled + ctm:r4pl + ctm + ctw + ctr + cto + ctm:mat_scaled  + ndose2 -1,
                  data = d4,
                  random = list(~ 1|studyid/g_crop_type), method="REML",sparse = TRUE)

out = estats(model_new = r_nue_4,model_base = r_nue_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(r_nue_4)

k <- r_nue_4$k
wi <- 1/r_nue_4$vi
vt <- (k-1) / (sum(wi) - sum(wi^2)/sum(wi))
PR2 <- r_nue_0$sigma2 / (sum(r_nue_4$sigma2) + vt)

##################################### SMD (metafor) ####################################################
theme_set(theme_bw())

d1 <- readxl::read_xlsx('Database.xlsx',sheet = "Tables")
d1 <- as.data.table(d1)

d2<-d1
CV_nuet_bar<-mean(d2$nuet_sd[is.na(d2$nuet_sd)==FALSE]/d2$nuet_mean[is.na(d2$nuet_sd)==FALSE])
d2$nuet_sd[is.na(d2$nuet_sd)==TRUE]<-d2$nuet_mean[is.na(d2$nuet_sd)==TRUE]*1.25*CV_nuet_bar

CV_nuec_bar<-mean(d2$nuec_sd[is.na(d2$nuec_sd)==FALSE]/d2$nuec_mean[is.na(d2$nuec_sd)==FALSE])
d2$nuec_sd[is.na(d2$nuec_sd)==TRUE]<-d2$nuec_mean[is.na(d2$nuec_sd)==TRUE]*1.25*CV_nuec_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

es21 <- escalc(measure = "SMD", data = d2, 
               m1i = nuet_mean, sd1i = nuet_sd, n1i = replication,
               m2i = nuec_mean, sd2i = nuec_sd, n2i = replication )

d02 <- as.data.table(es21)

d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))

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

out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_nue <- rma.mv(yi,vi, data=d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {
    
    r_nue <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(mean = as.numeric(r_nue$b),
                          se = as.numeric(r_nue$se),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_nue$k,')')
  )
}

out2 <- rbindlist(out2)

forest(x = out2$mean,
       sei = out2$se, slab=out2$label, psize=0.9, cex=1, sortvar=out2$label, xlab="Change in NUE (%)", header="Treatment", col="#CC0000", lwd=2)

ranktest(out2$mean, out2$se)

regtest(out2$mean, out2$se)

d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]

d02[,clay_scaled := scale(clay)]
d02[,soc_scaled := scale(soc)]
d02[,ph_scaled := scale(ph)]
d02[,mat_scaled := scale(mat)]
d02[,map_scaled := scale(map)]
d02[,n_dose_scaled := scale(n_dose)]

var.site <- c('mat_scaled','map_scaled','clay_scaled','soc_scaled','ph_scaled')
var.crop <- c('g_crop_type','n_dose_scaled')

var.trea <- c('biochar', 'fertilizer_type', 'crop_residue', 'tillage', 'cover_crop_and_crop_rotation', 'fertilizer_strategy')

var.sel <- c(var.trea,var.crop,var.site)

r_nue_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)

out1.est = out1.sum = list()

for(i in var.sel){
  
  vartype = is.character(d02[,get(i)])
  
  if(vartype == TRUE){
    
    r_nue_1 <- rma.mv(yi,vi, 
                      mods = ~factor(varsel)-1, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {
    
    r_nue_1 <- rma.mv(yi,vi, 
                      mods = ~varsel, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  
  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)','',rownames(r_nue_1$b)),
                              mean = round(as.numeric(r_nue_1$b),3),
                              se = round(as.numeric(r_nue_1$se),3),
                              ci.lb = round(as.numeric(r_nue_1$ci.lb),3),
                              ci.ub = round(as.numeric(r_nue_1$ci.ub),3),
                              pval = round(as.numeric(r_nue_1$pval),3))
  
  out1.sum[[i]] <- data.table(var = i,
                              AIC = r_nue_1$fit.stats[4,2],
                              ll = r_nue_1$fit.stats[1,2],
                              ll_impr = round(100 * (1-r_nue_1$fit.stats[1,2]/r_nue_0$fit.stats[1,2]),2),
                              r2_impr = round(100*max(0,(sum(r_nue_0$sigma2)-sum(r_nue_1$sigma2))/sum(r_nue_0$sigma2)),2),
                              pval = round(anova(r_nue_1,r_nue_0)$pval,3)
  )
  
}

out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)
print(out1.sum)
print(out1.est)

estats <- function(model_new,model_base){
  out <- data.table(AIC = model_new$fit.stats[4,2],
                    ll = model_new$fit.stats[1,2],
                    ll_impr = round(100 * (1-model_new$fit.stats[1,2]/model_base$fit.stats[1,2]),2),
                    r2_impr = round(100*max(0,(sum(model_base$sigma2)-sum(model_new$sigma2))/sum(model_base$sigma2)),2),
                    pval = round(anova(r_nue_1,r_nue_0)$pval,3))
  return(out)
}

d02[crop_type=='marize', crop_type := 'maize']
d02[crop_type=='vegetables', crop_type := 'vegetable']

d02[g_crop_type=='vegetable', g_crop_type := 'other']
d02[tillage=='reduced', tillage := 'no-till']

d02[,fertilizer_type := factor(fertilizer_type,
                               levels = c('mineral','organic', 'combined','enhanced'))] 
d02[,fertilizer_strategy := factor(fertilizer_strategy,
                                   levels = c("conventional", "placement","rate","timing"))]
d02[,g_crop_type := factor(g_crop_type,
                           levels = c('other','maize','wheat','rice'))]

d4 <- copy(d02)

d4[,r4pl := fifelse(fertilizer_strategy=='placement','yes','no')]
d4[,r4ti := fifelse(fertilizer_strategy=='timing','yes','no')]
d4[,r4do := fifelse(fertilizer_strategy=='rate','yes','no')]
d4[,ctm := fifelse(g_crop_type=='maize','yes','no')]
d4[,ctw := fifelse(g_crop_type=='wheat','yes','no')]
d4[,ctr := fifelse(g_crop_type=='rice','yes','no')]
d4[,cto := fifelse(g_crop_type=='other','yes','no')]
d4[,ndose2 := scale(n_dose^2)]

r_nue_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)

r_nue_4 <- rma.mv(yi,vi,
                  mods = ~fertilizer_type + r4pl + r4ti + r4do + biochar + crop_residue + tillage +
                    cover_crop_and_crop_rotation + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled +
                    soc_scaled : n_dose_scaled + ctm:r4pl + ctm + ctw + ctr + cto + ctm:mat_scaled  + ndose2 -1,
                  data = d4,
                  random = list(~ 1|studyid/g_crop_type), method="REML",sparse = TRUE)

out = estats(model_new = r_nue_4,model_base = r_nue_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(r_nue_4)

k <- r_nue_4$k
wi <- 1/r_nue_4$vi
vt <- (k-1) / (sum(wi) - sum(wi^2)/sum(wi))
PR2 <- r_nue_0$sigma2 / (sum(r_nue_4$sigma2) + vt)