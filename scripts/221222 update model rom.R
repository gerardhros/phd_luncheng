require(data.table)
require(metafor)

# read data
d1 <- readxl::read_xlsx('data/20220329_1_Database impacts measures on NUE_add 2.xlsx',sheet = 1)
d1 <- as.data.table(d1)

d2<-d1
CV_nuet_bar<-mean(d2$nuet_sd[is.na(d2$nuet_sd)==FALSE]/d2$nuet_mean[is.na(d2$nuet_sd)==FALSE])
d2$nuet_sd[is.na(d2$nuet_sd)==TRUE]<-d2$nuet_mean[is.na(d2$nuet_sd)==TRUE]*1.25*CV_nuet_bar

CV_nuec_bar<-mean(d2$nuec_sd[is.na(d2$nuec_sd)==FALSE]/d2$nuec_mean[is.na(d2$nuec_sd)==FALSE])
d2$nuec_sd[is.na(d2$nuec_sd)==TRUE]<-d2$nuec_mean[is.na(d2$nuec_sd)==TRUE]*1.25*CV_nuec_bar

# clean up column names
d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

d3 <- copy(d1)
d3[is.na(nuet_sd), nuet_sd := mean(d3$nuet_sd/d3$nuet_mean,na.rm=T) * 1.25 * nuet_mean]
d3[is.na(nuec_sd), nuec_sd := mean(d3$nuec_sd/d3$nuec_mean,na.rm=T) * 1.25 * nuec_mean]

d3 <- escalc(measure = "ROM", data = d3,
               m1i = nuet_mean, sd1i = nuet_sd, n1i = replication,
               m2i = nuec_mean, sd2i = nuec_sd, n2i = replication )
d3 <- as.data.table(d3)
# update the missing values for n_dose and p2o5_dose (as example)
d3[is.na(n_dose), n_dose := median(d3$n_dose,na.rm=TRUE)]
d3[is.na(p_dose), p_dose := median(d3$p_dose,na.rm=TRUE)]
d3[is.na(k_dose), k_dose := median(d3$k_dose,na.rm=TRUE)]


# # scale the variables to unit variance
d3[,clay_scaled := scale(clay)]
d3[,soc_scaled := scale(soc)]
d3[,ph_scaled := scale(ph)]
d3[,mat_scaled := scale(mat)]
d3[,map_scaled := scale(map)]
d3[,n_dose_scaled := scale(n_dose)]

d3[g_crop_type=='vegetable', g_crop_type := 'other']
d3[tillage=='reduced', tillage := 'no-till']
d3[fertilizer_type=='organic', fertilizer_type := 'organic_and_combined']
d3[fertilizer_type=='combined', fertilizer_type := 'organic_and_combined']

d3[,fertilizer_type := factor(fertilizer_type,
                              levels = c('mineral','organic_and_combined','enhanced'),
                              labels = c('mineral','mix','enhanced'))]
d3[,fertilizer_strategy := factor(fertilizer_strategy,
                                  levels = c("conventional", "placement","rate","timing"))]
d3[,g_crop_type := factor(g_crop_type,
                          levels = c('other','maize','wheat','rice'))]

d4 <- copy(d3)

d4[,r4pl := fifelse(fertilizer_strategy=='placement','yes','no')]
d4[,r4ti := fifelse(fertilizer_strategy=='timing','yes','no')]
d4[,r4do := fifelse(fertilizer_strategy=='rate','yes','no')]
d4[,ctm := fifelse(g_crop_type=='maize','yes','no')]
d4[,ctw := fifelse(g_crop_type=='wheat','yes','no')]
d4[,ctr := fifelse(g_crop_type=='rice','yes','no')]
d4[,cto := fifelse(g_crop_type=='other','yes','no')]
d4[,ndose2 := scale(n_dose^2)]

# this is a final model with only studyid to the error structure
r_nue_5 <- rma.mv(yi,vi,
                  mods = ~fertilizer_type + r4pl + r4ti + r4do + biochar + crop_residue + tillage +
                    cover_crop_and_crop_rotation + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled +
                    soc_scaled : n_dose_scaled  + ctm:r4pl+ ctm + ctw + ctr + cto + ctm:mat_scaled + ndose2 -1,
                  data = d4,
                  random = list(~ 1|studyid), method="REML",sparse = TRUE)

# this might be a better option, by adding crop type to the random error structure
r_nue_5n <- rma.mv(yi,vi,
                  mods = ~fertilizer_type + r4pl + r4ti + r4do + biochar + crop_residue + tillage +
                    cover_crop_and_crop_rotation + n_dose_scaled + clay_scaled + ph_scaled + map_scaled + mat_scaled +
                    soc_scaled : n_dose_scaled  + ctm:r4pl+ ctm + ctw + ctr + cto + ctm:mat_scaled + ndose2 -1,
                  data = d4,
                  random = list(~ 1|studyid/g_crop_type), method="REML",sparse = TRUE)
