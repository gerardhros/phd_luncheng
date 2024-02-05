
library(data.table)
library(metafor)
library(metagear)

d1 <- readxl::read_xlsx('.../You_paper3_S2.xlsx',sheet = 4)
d1 <- as.data.table(d1)

d2<-d1
CV_runofft_bar<-mean(d2$runofft_sd[is.na(d2$runofft_sd)==FALSE]/d2$runofft_mean[is.na(d2$runofft_sd)==FALSE])
d2$runofft_sd[is.na(d2$runofft_sd)==TRUE]<-d2$runofft_mean[is.na(d2$runofft_sd)==TRUE]*1.25*CV_runofft_bar

CV_runoffc_bar<-mean(d2$runoffc_sd[is.na(d2$runoffc_sd)==FALSE]/d2$runoffc_mean[is.na(d2$runoffc_sd)==FALSE])
d2$runoffc_sd[is.na(d2$runoffc_sd)==TRUE]<-d2$runoffc_mean[is.na(d2$runoffc_sd)==TRUE]*1.25*CV_runoffc_bar

d2 <- as.data.table(d2)

setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

# calculate effect size
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = runofft_mean, sd1i = runofft_sd, n1i = replication,
               m2i = runoffc_mean, sd2i = runoffc_sd, n2i = replication )
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='EE',desc := 'Enhanced Efficiency']
d02.treat[treatment=='CF',desc := 'Combined fertilizer']
d02.treat[treatment=='RFR',desc := 'Fertilizer rate']
d02.treat[treatment=='RFT',desc := 'Fertilizer timing']
d02.treat[treatment=='OF',desc := 'Organic fertilizer']
d02.treat[treatment=='NT',desc := 'No tillage']
out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_runoff <- rma.mv(yi,vi, data=d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {
    
    r_runoff <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|studyid), method="REML",sparse = TRUE)
    
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_runoff$b)-1)*100),
                          se = as.numeric((exp(r_runoff$se)-1)*100),
                          pval = round(as.numeric(r_runoff$pval),4),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_runoff$k,')')
  )
}

out2 <- rbindlist(out2)

# Meta-regression for main factors

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
d02[,mf := ifelse(!management %in% c('EE','CF','OF') | fertilizer_type == 'MF','yes','no')]
d02[,rfr := fifelse(management=='RFR' | rfr == 'yes','yes','no')]
d02[,rft := fifelse(management=='RFT' | rft == 'yes','yes','no')]
d02[,nt := fifelse(management=='NT' | tillage == 'NT','yes','no')] 

var.site <- c('mat_scaled','map_scaled','clay_scaled','soc_scaled','ph_scaled')
var.crop <- c('crop_type','n_dose_scaled')
var.trea <- c('ee', 'cf', 'of', 'rfr','rft','nt')

var.sel <- c(var.trea,var.crop,var.site)

r_runoff_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)

out1.est = out1.sum = list()

for(i in var.sel){
  
  vartype = is.character(d02[,get(i)])
  
  if(vartype == TRUE){
    
    r_runoff_1 <- rma.mv(yi,vi, 
                      mods = ~factor(varsel)-1, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
    
  } else {
    
    r_runoff_1 <- rma.mv(yi,vi, 
                      mods = ~varsel, 
                      data = d02[,.(yi,vi,studyid,varsel = get(i))],
                      random = list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  
  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)','',rownames(r_runoff_1$b)),
                              mean = round(as.numeric(r_runoff_1$b),3),
                              se = round(as.numeric(r_runoff_1$se),3),
                              ci.lb = round(as.numeric(r_runoff_1$ci.lb),3),
                              ci.ub = round(as.numeric(r_runoff_1$ci.ub),3),
                              pval = round(as.numeric(r_runoff_1$pval),3))
  
  out1.sum[[i]] <- data.table(var = i,
                              AIC = r_runoff_1$fit.stats[4,2],
                              ll = r_runoff_1$fit.stats[1,2],
                              ll_impr = round(100 * (1-r_runoff_1$fit.stats[1,2]/r_runoff_0$fit.stats[1,2]),2),
                              r2_impr = round(100*max(0,(sum(r_runoff_0$sigma2)-sum(r_runoff_1$sigma2))/sum(r_runoff_0$sigma2)),2),
                              pval = round(anova(r_runoff_1,r_runoff_0)$pval,3)
  )
  
}

# merge output into a data.table
out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)
print(out1.sum)
print(out1.est)

# Meta-regression for main factors with interactions

estats <- function(model_new,model_base){
  out <- data.table(AIC = model_new$fit.stats[4,2],
                    ll = model_new$fit.stats[1,2],
                    ll_impr = round(100 * (1-model_new$fit.stats[1,2]/model_base$fit.stats[1,2]),2),
                    r2_impr = round(100*max(0,(sum(model_base$sigma2)-sum(model_new$sigma2))/sum(model_base$sigma2)),2),
                    pval = round(anova(r_runoff_1,r_runoff_0)$pval,3))
  return(out)
}

r_runoff_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|studyid), method="REML",sparse = TRUE)
d02[,n_dose_sqrt := scale(sqrt(n_dose))]

r_runoff_final <- rma.mv(yi,vi, 
                         mods = ~ee+ cf + of + rfr + rft + nt + n_dose_sqrt + crop_type +
                           ph_scaled + clay_scaled + soc_scaled + map_scaled + mat_scaled + 
                           ee : crop_type - 1, 
                         data = d02,
                         random = list(~ 1|studyid), method="REML",sparse = TRUE)

out = estats(model_new = r_runoff_final,model_base = r_runoff_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(r_runoff_final)