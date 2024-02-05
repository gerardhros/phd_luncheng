# analysis N2O
# update code of rmd for developing N2O meta-regression

# clean environment
rm(list=ls())

# Load libraries
library(data.table); library(metafor); library(metagear)

# read data
d1 <- as.data.table(readxl::read_xlsx('articles/paper3/You_paper3_S2.xlsx',sheet = 2))

# clean up column names and remove title from database
setnames(d1,tolower(gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d1)))))
d1[,title := NULL]

# Supplement the SD when missing
d1[, n2ot_cv := mean(n2ot_sd/n2ot_mean,na.rm=T) * 1.25]
d1[, n2oc_cv := mean(n2oc_sd/n2oc_mean,na.rm=T) * 1.25]
d1[is.na(n2ot_sd), n2ot_sd := n2ot_mean * n2ot_cv]
d1[is.na(n2oc_sd), n2oc_sd := n2oc_mean * n2oc_cv]

# add quadratic effect of n_dose
d1[is.na(n_dose), n_dose := median(d1$n_dose,na.rm=TRUE)]
d1[,n_dose2 := n_dose * n_dose]

# Supplement and update the missing values for n_dose
d1[,clay_scaled := scale(clay)]
d1[,soc_scaled := scale(soc)]
d1[,ph_scaled := scale(ph)]
d1[,mat_scaled := scale(mat)]
d1[,map_scaled := scale(map)]
d1[,n_dose_scaled := scale(n_dose)]
d1[,n_dose2_scaled := scale(n_dose2)]

# calculate effect size (n2o)
d2 <- escalc(measure = "ROM", data = d1,
             m1i = n2ot_mean, sd1i = n2ot_sd, n1i = replication,
             m2i = n2oc_mean, sd2i = n2oc_sd, n2i = replication )

# remove a few observations (n = 8) with extremely high variance
d2 <- as.data.table(d2)
d2 <- d2[vi < 20]

# update all management practices with the measure applied

# what are labels
d2[management =='EE',desc := 'Enhanced Efficiency']
d2[management =='CF',desc := 'Combined fertilizer']
d2[management =='RES',desc := 'Residue retention']
d2[management =='RFP',desc := 'Fertilizer placement']
d2[management =='RFR',desc := 'Fertilizer rate']
d2[management =='ROT',desc := 'Crop rotation']
d2[management =='RFT',desc := 'Fertilizer timing']
d2[management =='OF',desc := 'Organic fertilizer']
d2[management =='RT',desc := 'Reduced tillage']
d2[management =='NT',desc := 'No tillage']
d2[management =='CC',desc := 'Crop cover']
d2[management =='BC',desc := 'Biochar']



# make a function to extract relevant model statistics
estats <- function(model_new,model_base){
  out <- data.table(AIC = model_new$fit.stats[4,2],
                    ll = model_new$fit.stats[1,2],
                    ll_impr = round(100 * (1-model_new$fit.stats[1,2]/model_base$fit.stats[1,2]),2),
                    r2_impr = round(100*max(0,(sum(model_base$sigma2)-sum(model_new$sigma2))/sum(model_base$sigma2)),2),
                    pval = round(anova(r_n2o_1,r_n2o_0)$pval,3))
  return(out)
}

# run without a main factor selection to estimate overall mean
r_n2o_0 <- rma.mv(yi,vi, data = d2,random= list(~ 1|studyid), method="REML",sparse = TRUE)

# run a model with all factors included (without interactions)
r_n2o_1 <- rma.mv(yi,vi,
                  data = d2,
                  mods = ~ management + n_dose_scaled +n_dose2_scaled + crop_type +
                          fertilizer_type + rfp + rfr + rft +
                          crop_rotation +
                          ph_scaled + clay_scaled + soc_scaled + map_scaled + mat_scaled - 1,
                  random= list(~ 1|studyid), method="REML",sparse = TRUE)

# see the summary stats of the model
summary(r_n2o_1)

# knowing the empty and full model you can play around with the model parameters
# start with adding the significant variables (see the summary stats ) and add then other properties
# add also interactions (only significant of course) and not more than 2-way interactions.
# below an example

d3 <- copy(d2)
m1 <- rma.mv(yi,vi,
                  mods = ~ management + n_dose_scaled + crop_type +
                           fertilizer_type + rfp + rfr + rft +
                           crop_rotation +
                           ph_scaled + clay_scaled + soc_scaled + map_scaled - 1,
                  data = d3,
                  random = list(~ 1|studyid), method="REML",sparse = TRUE)
# make a prediciton with model m1
p1 <- predict(m1)

# combine in one data.table
d3[,c('pred','se','ci.lb','ci.ub','pi.lb','pi.ub') := as.data.table(p1)]

# make a one-to-one plot
require(ggplot2)
p1 <- ggplot(data = d3,aes(x=yi,y=pred)) + geom_point() + theme_bw() +
      ylim(-5,5) + xlim(-5,5) + geom_abline(slope=1,intercept=0) +
      ggtitle('Observed versus predicted change in N2O') +
      ylab('predicted') + xlab('observed')
ggsave(plot=p1, filename = 'products/testfigure1.png',width = 10,height = 10,units='cm')

# make a prediction to see impact of a measure

  # see model structure that need to be filled in to predict NUE as function of the system properties
  p1 <- predict(m1,addx=T)

  # this is the order of input variables needed for model predictions (=newmods in predict function)
  m1.cols <- colnames(p1$X)

  # make prediction dataset for situation that soil is fertilized by both organic and inorganic fertilizers, conventional fertilizer strategy
  dt.new <- copy(d2)

  # what is the target to evaluate
  target = 'managementEE'
  measures = m1.cols[grepl('mana',m1.cols) & m1.cols != target]

  # set values to default for the measure to evaluate
  dt.new[,c(measures) := 0]
  dt.new[, c(target) := 1]

  # here the default management options to be applied globally
  dt.new[, fertilizer_typeEE := 0]
  dt.new[, fertilizer_typeMF := 1]
  dt.new[, fertilizer_typeOF := 0]

  # what are default management practices
  man.default <- c('rfpyes','rftyes','rfryes','crop_residueyes','cover_cropyes','crop_rotationyes',
                   'tillageNT','tillageRT')
  dt.new[,c(man.default) := 0]

  # set crop type to 1 in relevant case
  dt.new[, crop_typeother := fifelse(crop_type=='other',1,0)]
  dt.new[, crop_typerice := fifelse(crop_type=='rice',1,0)]
  dt.new[, crop_typewheat := fifelse(crop_type=='wheat',1,0)]
  dt.new[, crop_typemaize := fifelse(crop_type=='maize',1,0)]

  # convert to matrix, needed for rma models
  dt.newmod <- as.matrix(dt.new[,mget(c(m1.cols))])

  # predict the N2O via MD model
  dt.pred1 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))



# plot the model coefficients when calibrated

  dt.plot <- data.table(parm = rownames(m1$beta),
                        value = round((exp(as.numeric(m1$beta))-1)*100,1),
                        pval = round(m1$pval,3))
  dt.plot[,pvaluep := fifelse(pval<=0.001,'***',fifelse(pval<=0.01,'**',ifelse(pval<=0.05,'*','')))]
  dt.plot[grepl('manag',parm),ptype := 'measure']
  dt.plot[grepl('^ferti|^rf|^cr|^ti|^co|n_do',parm),ptype := 'man']
  dt.plot[grepl('ph|clay|soc|map|mat',parm),ptype := 'site']

  dt.plot[,parm2 := gsub('management|fertilizer_type|yes|tillage|crop_type|_scaled','', parm)]
  dt.plot[parm2=='crop_residue',parm2 := "RES"]
  dt.plot[parm2=='cover_crop',parm2 := "CC"]
  dt.plot[parm2=='crop_rotation',parm2 := "ROT"]
  dt.plot[,parm2 := toupper(parm2)]
  dt.plot[ptype=='man',parm2:= paste0('m',parm2)]
  dt.plot[,parm2f := factor(parm2,levels = c('EE','OF','MF', 'CF', 'RFP','RFR','RFT', 'BC','CC','RES','ROT','NT','RT',
                                            'mEE','mOF','mMF', 'mRFP','mRFR','mRFT','mCC','mRES','mROT','mNT','mRT',
                                            'mN_DOSE','mN_DOSE2','mOTHER','mRICE','mWHEAT',
                                            'PH','CLAY','SOC','MAP','MAT'))]

  require(ggplot2)
  ggplot(dt.plot,aes(parm2f,value)) + geom_col() + theme_bw() +
    geom_text(aes(label = pvaluep, y = value + fifelse(value>0,5,-5)),size=4, vjust = 0.5,hjust = 0.5) +
    ylab('Change in N2O (%)')+xlab('')+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
    ggtitle('Effect of measures, management and site properties on N2O (%)')

