#Figure S2

# require packages
require(readxl);require(data.table);library(metafor); library(GGally)

# read data from Excel
d1 <- readxl::read_xlsx('data/20220329_1_Database impacts measures on NUE_add 2.xlsx',sheet = 1)
d1 <- as.data.table(d1)

# adjust missing NA values
d2 <- copy(d1)
d2[is.na(nuet_sd), nuet_sd := mean(d2$nuet_sd/d2$nuet_mean,na.rm=T) * 1.25 * nuet_mean]
d2[is.na(nuec_sd), nuec_sd := mean(d2$nuec_sd/d2$nuec_mean,na.rm=T) * 1.25 * nuec_mean]

# clean up column names
d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

#Supplement and update the missing values for n_dose and p_dose
d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]
d2[is.na(p_dose), p_dose := median(d2$p_dose,na.rm=TRUE)]
d2[is.na(k_dose), k_dose := median(d2$k_dose,na.rm=TRUE)]

# update the database (g_crop_type)
d2[g_crop_type=='maize', g_crop_type := 1]
d2[g_crop_type=='wheat', g_crop_type := 2]
d2[g_crop_type=='rice', g_crop_type := 3]
d2[g_crop_type=='vegetable', g_crop_type := 4]
d2[g_crop_type=='barley', g_crop_type := 5]
d2[g_crop_type=='other', g_crop_type := 6]

# update the database (tillage)
d2[tillage=='ROT', tillage := 7]
d2[tillage=='CC', tillage := 8]
d2[tillage=='RES', tillage := 9]
d2[tillage=='RFR', tillage := 10]
d2[tillage=='RFP', tillage := 11]
d2[tillage=='RFT', tillage := 12]
d2[tillage=='CF', tillage := 13]
d2[tillage=='OF', tillage := 14]
d2[tillage=='BC', tillage := 15]
d2[tillage=='RT', tillage := 16]
d2[tillage=='NT', tillage := 17]
d2[tillage=='EE', tillage := 18]

# adjust to number
d2[,g_crop_type := as.numeric(g_crop_type)]
d2[,tillage := as.numeric(tillage)]

# Estimate meta-analytical response measure (ROM Method)

  # calculate effect size (NUE)
  es21 <- escalc(measure = "ROM", data = d2,
                 m1i = nuet_mean, sd1i = nuet_sd, n1i = replication,
                 m2i = nuec_mean, sd2i = nuec_sd, n2i = replication )

# make forest plots per group treatments

  # convert to data.tables
  d02 <- as.data.table(es21)

  # subset the data.table
  d3 <- d02[,list(MAT = mat,MAP = map, Clay = clay, SOC = soc, pH = ph, `N rate` = n_dose, `Crop type` = g_crop_type)]

  # scale values to z-score
  cols <- colnames(d3)[!grepl('Crop',colnames(d3))]
  d3[,(cols) := lapply(.SD,scale),.SDcols = cols]

# plot all pairs
p1 <- ggpairs(d3, lower = list(continuous = wrap("cor", size = 8,color="#E64B35B2")),
              upper = list(continuous = wrap("smooth", size =1.2,color="#4DBBD5B2")))+
              theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
              theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
              theme(axis.text=element_text(colour='black',size=18),
                    strip.text = element_text(color="black", size = 18, face="bold"))

# save the plot in directory 'products'
ggsave(plot = p1,file = "products/Data fitting and correlation coefficients.png",width = 410,height = 297, units = "mm")
