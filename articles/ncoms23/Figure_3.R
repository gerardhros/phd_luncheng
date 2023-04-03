# Figure 3

# require packages
library(ggpubr)
library(data.table)

# --- ROM Method -----

# load data from Excel file
d1 <- readxl::read_xlsx('articles/ncoms23/Database.xlsx',sheet = "Figure3a")
d1 <- as.data.table(d1)

# set moderater levels
mlevels <- c("EE","CF","OF","MF","RFP","RFR", "RFT","BC", "RES","CC/ROT","ZT/RT","Cr_w",
             "Cr_m","Cr_r","N_sc","Clay_sc","pH_sc","MAP_sc","MAT_sc","N_sq","RFP˟Cr_m",
             "MAT_sc˟Cr_m","N_sc˟SOC_sc")
d1[,Moderator1 := factor(Moderator1, levels = mlevels)]

# add labels for p value in the figure
d1[,p_value2 := as.numeric(gsub("<( )?","",p_value))]
d1[p_value2 >= 0.05,plab := '']
d1[p_value2 >= 0.01 & p_value2 < 0.05, plab:='*']
d1[p_value2 >= 0.001 & p_value2 < 0.01, plab:='**']
d1[p_value2 >= 0.0001 & p_value2 < 0.001, plab:='***']

# add label position for p value
d1[, plabpos := Parameter_estimate + fifelse(Parameter_estimate < 0, -1,1)*0.05]

p1 <- ggplot(d1,aes(Moderator1,Parameter_estimate))+
      geom_col()+ theme_bw()+
      theme(panel.grid=element_blank())+
      geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 11.5), fill = "#FBE7DD")+
      geom_rect(aes(ymin = -Inf, xmin = 11.5, ymax = Inf, xmax = Inf), fill = "#E6E5E3")+
      geom_col(fill = c(rep("#66c2a5", 8),rep("#e78ac3",2),"#fb8072", rep("#fdbf6f",4),
                        rep("#bebada",5),rep("#a6d854",3)))+
      xlab("")+ ylab("Parameter estimate")+
      theme(legend.position ="none",
            axis.title = element_text(size=12, colour="black", face = "bold"),
            axis.text.x = element_blank(),
            axis.ticks.x= element_blank(),
            axis.text.y = element_text(size=12, colour="black"))+
      annotate("text",x=1:nrow(d1),y= d1$plabpos, label = d1$plab,size=5)+
      annotate("text",x=11.5,y=1.1,label="ROM",size=5, fontface = "bold", colour="black")+
      annotate("text",x=6,y=1.02,label="Management practices",size=4, fontface = "bold", colour="black")+
      annotate("text",x=16,y=1.02,label="Site factors",size=4, fontface = "bold", colour="black")+
      annotate("text",x=21,y=-0.9,label= expression(paste(Pseudo~R^2,"= 0.44")), size=4,
               fontface = "bold", colour="deeppink4")



# ---  MD Method ----

# load in the data
d2 <- readxl::read_xlsx('articles/ncoms23/Database.xlsx',sheet = "Figure3b")
d2 <- as.data.table(d2)


# set moderater levels
mlevels <- c("EE","CF","OF","MF","RFP","RFR", "RFT","BC", "RES","CC/ROT","ZT/RT","Cr_w",
             "Cr_m","Cr_r","N_sc","Clay_sc","pH_sc","MAP_sc","MAT_sc","N_sq","RFP˟Cr_m",
             "MAT_sc˟Cr_m","N_sc˟SOC_sc")
d2[,Moderator1 := factor(Moderator1, levels = mlevels)]

# add labels for p value in the figure
d2[,p_value2 := as.numeric(gsub("<( )?","",p_value))]
d2[p_value2 >= 0.05,plab := '']
d2[p_value2 >= 0.01 & p_value2 < 0.05, plab:='*']
d2[p_value2 >= 0.001 & p_value2 < 0.01, plab:='**']
d2[p_value2 >= 0.0001 & p_value2 < 0.001, plab:='***']

# add label position for p value
d2[, plabpos := Parameter_estimate + fifelse(Parameter_estimate < 0, -1,1)*0.5]

p2 <- ggplot(d2,aes(Moderator1, Parameter_estimate))+
      geom_col()+
      theme_bw()+
      theme(panel.grid=element_blank())+
      geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 11.5), fill = "#FBE7DD")+
      geom_rect(aes(ymin = -Inf, xmin = 11.5, ymax = Inf, xmax = Inf), fill = "#E6E5E3")+
      geom_col(fill =  c(rep("#66c2a5", 8),rep("#e78ac3",2),"#fb8072", rep("#fdbf6f",4),
                         rep("#bebada",5),rep("#a6d854",3))) +
      xlab("")+
      ylab("Parameter estimate")+
      theme(legend.position ="none",
            axis.title = element_text(size=12, colour="black", face = "bold"),
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y = element_text(size=12, colour="black"))+
      annotate("text",x=1:nrow(d2),y= d2$plabpos, label = d2$plab,size=5)+
      annotate("text",x=11.5,y=11,label="MD",size=5, fontface = "bold", colour="black")+
      annotate("text",x=6,y=10,label="Management practices",size=4, fontface = "bold", colour="black")+
      annotate("text",x=16,y=10,label="Site factors",size=4, fontface = "bold", colour="black")+
      annotate("text",x=21,y=-10,label= expression(paste(Pseudo~R^2,"= 0.82")), size=4, fontface = "bold", colour="deeppink4")

    p2

# --- SMD Method -----

# load in the data
d3 <- readxl::read_xlsx('articles/ncoms23/Database.xlsx',sheet = "Figure3c")
d3 <- as.data.table(d3)

# set moderater levels
mlevels <- c("EE","CF","OF","MF","RFP","RFR", "RFT","BC", "RES","CC/ROT","ZT/RT","Cr_w",
             "Cr_m","Cr_r","N_sc","Clay_sc","pH_sc","MAP_sc","MAT_sc","N_sq","RFP˟Cr_m",
             "MAT_sc˟Cr_m","N_sc˟SOC_sc")
d3[,Moderator1 := factor(Moderator1, levels = mlevels)]

# add labels for p value in the figure
setnames(d3,'z_value','p_value')
d3[,p_value2 := as.numeric(gsub("<( )?","",p_value))]
d3[p_value2 >= 0.05,plab := '']
d3[p_value2 >= 0.01 & p_value2 < 0.05, plab:='*']
d3[p_value2 >= 0.001 & p_value2 < 0.01, plab:='**']
d3[p_value2 >= 0.0001 & p_value2 < 0.001, plab:='***']

# add label position for p value
d3[, plabpos := Parameter_estimate + fifelse(Parameter_estimate < 0, -1,1)*0.5]

p3 <- ggplot(d3,aes(Moderator1,Parameter_estimate))+
      geom_col()+
      theme_bw()+
      theme(panel.grid=element_blank())+
      geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 11.5), fill = "#FBE7DD")+
      geom_rect(aes(ymin = -Inf, xmin = 11.5, ymax = Inf, xmax = Inf), fill = "#E6E5E3")+
      geom_col(fill = c(rep("#66c2a5", 8),rep("#e78ac3",2),"#fb8072", rep("#fdbf6f",4),
                        rep("#bebada",5),rep("#a6d854",3)))+
      xlab("Management practices & Site factors")+
      ylab("Parameter estimate")+
      theme(legend.position ="none",
            axis.title = element_text(size=12, face = "bold", colour="black"),
            axis.text.x = element_text(size=12,angle = 45, hjust = 1, vjust = 1, colour="black"),
            axis.text.y = element_text(size=12, colour="black"))+
      annotate("text",x=1:nrow(d3),y= d3$plabpos, label = d3$plab,size=5) +
      annotate("text",x=11.5,y=1.8,label="SMD",size=5, fontface = "bold", colour="black")+
      annotate("text",x=6,y=1.7,label="Management practices",size=4, fontface = "bold", colour="black")+
      annotate("text",x=16,y=1.7,label="Site factors",size=4, fontface = "bold", colour="black")+
      annotate("text",x=21,y=1.3,label= expression(paste(Pseudo~R^2,"= 0.65")), size=4, fontface = "bold", colour="deeppink4")

# arrage plots into one figure
p<- ggarrange(p1, p2, p3, ncol = 1, nrow = 3,align = "v",heights = c(2,2,2.9),
             labels = c("a", "b","c"), font.label=list(size=14, color = "black", face = "bold"), label.x = 0, label.y = 1,hjust = -0.2, vjust = 1)

ggsave(plot = p, file = "products/meta_regression.png", width = 180, height = 210, units = "mm")

