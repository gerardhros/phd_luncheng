# Meta-analysis

library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(openxlsx)
library(gg.gap)
library(ggforce)
library(data.table)

malit <- readxl::read_xlsx('.../You_paper3_S1.xlsx',sheet = 1)
malit <- as.data.table(malit)

# create subset of columns we need 
df <- data.frame(reference=malit$reference, ind_code=malit$ind_code, man_code=malit$man_code, 
                 man=malit$man, ind=malit$ind, man_type=malit$man_type, type.m=as.character(malit$type.m), n.S=1, 
                 dyr.1=malit$dyr.1, SEyr.1=as.numeric(malit$Seyr.1),unit=malit$unit, n.O=malit$n)

df1 <- df %>% group_by(ind_code,man_code,ind,man) %>% 
  summarise(type.m= "Weighted mean", dyr.1 = weighted.mean(dyr.1, 1/SEyr.1),
            reference = paste0(reference, collapse = ", "), n.S = n(), n.O = sum(n.O), man_type=first(man_type))
df1 <-data.frame(df1)

df2 <- df %>% group_by(ind_code,man_code,ind,man) %>% 
  summarise(type.m= "Weighted mean SE", dyr.1 = (weighted.mean(dyr.1, 1/SEyr.1)-(1/sqrt(sum(1/SEyr.1)))),
            reference = paste0(reference, collapse = ", "), n.S = n(), n.O = sum(n.O), man_type=first(man_type))
df2 <-data.frame(df2)

df3 <- df %>% group_by(ind_code,man_code,ind,man) %>% 
  summarise(type.m= "Weighted mean SE", dyr.1 = weighted.mean(dyr.1, 1/SEyr.1)+(1/sqrt(sum(1/SEyr.1))),
            reference = paste0(reference, collapse = ", "), n.S = n(), n.O = sum(n.O), man_type=first(man_type))
df3 <-data.frame(df3)

df4 <- df %>% group_by(ind_code,man_code,ind,man) %>% 
  summarise(type.m= "Min and max SE", dyr.1 = min(dyr.1-SEyr.1),
            reference = paste0(reference, collapse = ", "), n.S = n(), n.O = sum(n.O), man_type=first(man_type))
df4 <-data.frame(df4)

df5 <- df %>% group_by(ind_code,man_code,ind,man) %>% 
  summarise(type.m= "Min and max SE", dyr.1 = max(dyr.1+SEyr.1),
            reference = paste0(reference, collapse = ", "), n.S = n(), n.O = sum(n.O), man_type=first(man_type))
df5 <-data.frame(df5)

df_range <- df %>% group_by(ind_code,man_code,ind,man) %>% 
  summarise(min_im = min(dyr.1), max_im = max(dyr.1))

df_im <- data.frame(reference=df$reference, ind_code=df$ind_code, man_code=df$man_code, ind=df$ind, man=df$man,
                    man_type=malit$man_type, type.m=df$type.m, dyr.1=df$dyr.1, n.S=df$n.S, n.O=df$n.O)

df_wm <- rbind(df_im, df1, df2, df3, df4, df5)
df_means <- subset(df_wm, type.m=="Weighted mean")

df_means <- data.frame(indicator=df_means$ind, management=df_means$man, man_type=df_means$man_type, 
                       wm = signif(df_means$dyr.1, digits=2))
df_means <- cbind(df_means, min_im = df_range$min_im, max_im = df_range$max_im)

df_wm$man <- factor(as.factor(df_wm$man), levels=c("NT",
                                                   "RT",
                                                   "RES",
                                                   "CC",
                                                   "ROT",
                                                   "BC",
                                                   "EE",
                                                   "RFT",
                                                   "RFP",
                                                   "RFR",
                                                   "CF",
                                                   "OF"))

write.xlsx(df_means, file=".../meta_of_meta_results.xlsx", sheetName = "Weighted mean results", colNames = TRUE, rowNames = TRUE, append = FALSE)

#N20
library(data.table)
library(metafor)
library(metagear)

# read data
d1 <- readxl::read_xlsx('.../You_paper3_S2.xlsx',sheet = 2)
d1 <- as.data.table(d1)

d2<-d1
CV_n2ot_bar<-mean(d2$n2ot_sd[is.na(d2$n2ot_sd)==FALSE]/d2$n2ot_mean[is.na(d2$n2ot_sd)==FALSE])
d2$n2ot_sd[is.na(d2$n2ot_sd)==TRUE]<-d2$n2ot_mean[is.na(d2$n2ot_sd)==TRUE]*1.25*CV_n2ot_bar

CV_n2oc_bar<-mean(d2$n2oc_sd[is.na(d2$n2oc_sd)==FALSE]/d2$n2oc_mean[is.na(d2$n2oc_sd)==FALSE])
d2$n2oc_sd[is.na(d2$n2oc_sd)==TRUE]<-d2$n2oc_mean[is.na(d2$n2oc_sd)==TRUE]*1.25*CV_n2oc_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

# calculate effect size
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = n2ot_mean, sd1i = n2ot_sd, n1i = replication,
               m2i = n2oc_mean, sd2i = n2oc_sd, n2i = replication )

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


# a list to store the coefficients
out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){

    r_n2O <- rma.mv(yi,vi, data=d02,random= list(~ 1|id), method="REML",sparse = TRUE)
    
  } else {
    
    r_n2O <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|id), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(mean = as.numeric((exp(r_n2O$b)-1)*100),
                          se = as.numeric((exp(r_n2O$se)-1)*100), 
                          pval = round(as.numeric(r_n2O$pval),4),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_n2O$k,')')
  )
}

# convert lists to vector
out2 <- rbindlist(out2)

write.csv(out2, file=".../N2O_meta_underling_data.csv")

#NH3
library(data.table)
library(metafor)
library(metagear)

# read data
d1 <- readxl::read_xlsx('.../You_paper3_S2.xlsx',sheet = 3)
d1 <- as.data.table(d1)

d2<-d1
CV_nh3t_bar<-mean(d2$nh3t_sd[is.na(d2$nh3t_sd)==FALSE]/d2$nh3t_mean[is.na(d2$nh3t_sd)==FALSE])
d2$nh3t_sd[is.na(d2$nh3t_sd)==TRUE]<-d2$nh3t_mean[is.na(d2$nh3t_sd)==TRUE]*1.25*CV_nh3t_bar

CV_nh3c_bar<-mean(d2$nh3c_sd[is.na(d2$nh3c_sd)==FALSE]/d2$nh3c_mean[is.na(d2$nh3c_sd)==FALSE])
d2$nh3c_sd[is.na(d2$nh3c_sd)==TRUE]<-d2$nh3c_mean[is.na(d2$nh3c_sd)==TRUE]*1.25*CV_nh3c_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

# calculate effect size
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = nh3t_mean, sd1i = nh3t_sd, n1i = replication,
               m2i = nh3c_mean, sd2i = nh3c_sd, n2i = replication )

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
d02.treat[treatment=='BC',desc := 'Biochar']
out2 = out3 = list()

# make a for loop to do a main analysis per treatment
for(i in d02.treat$treatment){
  
  if(i=='ALL'){

    r_nh3 <- rma.mv(yi,vi, data=d02,random= list(~ 1|id), method="REML",sparse = TRUE)
    
  } else {

    r_nh3 <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|id), method="REML",sparse = TRUE)
    
  }

  out2[[i]] <- data.table(mean = as.numeric((exp(r_nh3$b)-1)*100),
                          se = as.numeric((exp(r_nh3$se)-1)*100),
                          pval = round(as.numeric(r_nh3$pval),4),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_nh3$k,')')
  )
}

out2 <- rbindlist(out2)

write.csv(out2, file=".../NH3_meta_underling_data.csv")

#N runoff
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
d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

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

# a list to store the coefficients
out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){

    r_runoff <- rma.mv(yi,vi, data=d02,random= list(~ 1|id), method="REML",sparse = TRUE)
    
  } else {

    r_runoff <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|id), method="REML",sparse = TRUE)
    
  }

  out2[[i]] <- data.table(mean = as.numeric(((exp(r_runoff$b)-1)*100)),
                          se = as.numeric(((exp(r_runoff$se)-1)*100)),
                          pval = round(as.numeric(r_runoff$pval),4),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_runoff$k,')')
  )
}

out2 <- rbindlist(out2)

write.csv(out2, file=".../runoff_meta_underling_data.csv")

#N leaching
library(data.table)
library(metafor)
library(metagear)

d1 <- readxl::read_xlsx('.../You_paper3_S2.xlsx',sheet = 5)
d1 <- as.data.table(d1)

d2<-d1
CV_leachingt_bar<-mean(d2$leachingt_sd[is.na(d2$leachingt_sd)==FALSE]/d2$leachingt_mean[is.na(d2$leachingt_sd)==FALSE])
d2$leachingt_sd[is.na(d2$leachingt_sd)==TRUE]<-d2$leachingt_mean[is.na(d2$leachingt_sd)==TRUE]*1.25*CV_leachingt_bar
CV_leachingc_bar<-mean(d2$leachingc_sd[is.na(d2$leachingc_sd)==FALSE]/d2$leachingc_mean[is.na(d2$leachingc_sd)==FALSE])
d2$leachingc_sd[is.na(d2$leachingc_sd)==TRUE]<-d2$leachingc_mean[is.na(d2$leachingc_sd)==TRUE]*1.25*CV_leachingc_bar
d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))
d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

# calculate effect size
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = leachingt_mean, sd1i = leachingt_sd, n1i = replication,
               m2i = leachingc_mean, sd2i = leachingc_sd, n2i = replication )
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='EE',desc := 'Enhanced Efficiency']
d02.treat[treatment=='CF',desc := 'Combined fertilizer']
d02.treat[treatment=='RES',desc := 'Residue retention']
d02.treat[treatment=='RFR',desc := 'Fertilizer rate']
d02.treat[treatment=='ROT',desc := 'Crop rotation']
d02.treat[treatment=='RFT',desc := 'Fertilizer timing']
d02.treat[treatment=='OF',desc := 'Organic fertilizer']
d02.treat[treatment=='NT',desc := 'No tillage']
d02.treat[treatment=='CC',desc := 'Crop cover']
d02.treat[treatment=='BC',desc := 'Biochar']


# a list to store the coefficients
out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){

    r_leaching <- rma.mv(yi,vi, data=d02,random= list(~ 1|id), method="REML",sparse = TRUE)
    
  } else {

    r_leaching <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|id), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(mean = as.numeric(((exp(r_leaching$b)-1)*100)),
                          se = as.numeric(((exp(r_leaching$se)-1)*100)),
                          pval = round(as.numeric(r_leaching$pval),4),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_leaching$k,')')
  )
}

out2 <- rbindlist(out2)

write.csv(out2, file=".../leaching_meta_underling_data.csv")
