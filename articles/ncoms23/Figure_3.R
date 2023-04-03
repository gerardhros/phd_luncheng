# Figure 3


library(tidyverse)
library(ggpubr)
library(data.table)

######################################## ROM Method #############################################

d1 <- readxl::read_xlsx('Database.xlsx',sheet = "Figure3a")
d1 <- as.data.table(d1)

d1$Moderator1 <- factor(d1$Moderator1, levels = c("EE",
                                                  "CF",
                                                  "OF",
                                                  "MF",
                                                  "RFP",
                                                  "RFR",
                                                  "RFT",
                                                  "BC",
                                                  "RES",
                                                  "CC/ROT",
                                                  "ZT/RT",
                                                  "Cr_w",
                                                  "Cr_m",
                                                  "Cr_r",
                                                  "N_sc",
                                                  "Clay_sc",
                                                  "pH_sc",
                                                  "MAP_sc",
                                                  "MAT_sc",
                                                  "N_sq",
                                                  "RFP˟Cr_m",
                                                  "MAT_sc˟Cr_m",
                                                  "N_sc˟SOC_sc"))

p1 <- ggplot(d1,aes(Moderator1,Parameter_estimate))+
  geom_col()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 11.5), fill = "#FBE7DD")+
  geom_rect(aes(ymin = -Inf, xmin = 11.5, ymax = Inf, xmax = Inf), fill = "#E6E5E3")+
  geom_col(fill = c("#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#66c2a5","#e78ac3",
                    "#e78ac3","#fb8072", "#fdbf6f",
                    "#fdbf6f", "#fdbf6f", "#fdbf6f",
                    "#bebada","#bebada","#bebada","#bebada","#bebada","#a6d854",
                    "#a6d854", "#a6d854"))+

  xlab("")+
  ylab("Parameter estimate")+
  theme(legend.position ="none", 
        axis.title = element_text(size=12, colour="black", face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=12, colour="black"))+ 
  annotate("text",x=1,y=-0.2086,label="",size=5)+
  annotate("text",x=2,y=-0.3357,label="***",size=5)+
  annotate("text",x=3,y=-0.3422,label="*",size=5)+
  annotate("text",x=4,y=-0.4518,label="*",size=5)+
  annotate("text",x=5,y=0.1685,label="***",size=5)+
  annotate("text",x=6,y=0.1589,label="***",size=5)+
  annotate("text",x=7,y=0.2085,label="***",size=5)+
  annotate("text",x=8,y=0.2213,label="***",size=5)+
  annotate("text",x=9,y=0.0962,label="***",size=5)+
  annotate("text",x=10,y=0.1646,label="***",size=5)+
  annotate("text",x=11,y=-0.1574,label="***",size=5)+
  annotate("text",x=12,y=0.4597,label="***",size=5)+
  annotate("text",x=13,y=0.5617,label="***",size=5)+
  annotate("text",x=14,y=0.4241,label="***",size=5)+
  annotate("text",x=15,y=-1.042,label="***",size=5)+
  annotate("text",x=16,y=-0.2342,label="***",size=5)+
  annotate("text",x=17,y=-0.1055,label="",size=5)+
  annotate("text",x=18,y=0.1834,label="***",size=5)+
  annotate("text",x=19,y=-0.1528,label="**",size=5)+
  annotate("text",x=20,y=1.0397,label="***",size=5)+
  annotate("text",x=21,y=-0.4741,label="***",size=5)+
  annotate("text",x=22,y=0.1106,label="**",size=5)+
  annotate("text",x=23,y=0.0876,label="***",size=5)+
  annotate("text",x=11.5,y=1.1,label="ROM",size=5, face = "bold", colour="black")+
  annotate("text",x=6,y=1.02,label="Management practices",size=4, face = "bold", colour="black")+
  annotate("text",x=16,y=1.02,label="Site factors",size=4, face = "bold", colour="black")+
  annotate("text",x=21,y=-0.9,label= expression(paste(Pseudo~R^2,"= 0.44")), size=4, face = "bold", colour="deeppink4")

p1  

######################################## MD Method #############################################

d2 <- readxl::read_xlsx('Database.xlsx',sheet = "Figure3b")
d2 <- as.data.table(d2)

d2$Moderator1 <- factor(d2$Moderator1, levels = c("EE",
                                                  "CF",
                                                  "OF",
                                                  "MF",
                                                  "RFP",
                                                  "RFR",
                                                  "RFT",
                                                  "BC",
                                                  "RES",
                                                  "CC/ROT",
                                                  "ZT/RT",
                                                  "Cr_w",
                                                  "Cr_m",
                                                  "Cr_r",
                                                  "N_sc",
                                                  "Clay_sc",
                                                  "pH_sc",
                                                  "MAP_sc",
                                                  "MAT_sc",
                                                  "N_sq",
                                                  "RFP˟Cr_m",
                                                  "MAT_sc˟Cr_m",
                                                  "N_sc˟SOC_sc"))

p2 <- ggplot(d2,aes(Moderator1, Parameter_estimate))+
  geom_col()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 11.5), fill = "#FBE7DD")+
  geom_rect(aes(ymin = -Inf, xmin = 11.5, ymax = Inf, xmax = Inf), fill = "#E6E5E3")+
  geom_col(fill = c("#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#66c2a5","#e78ac3",
                    "#e78ac3","#fb8072", "#fdbf6f",
                    "#fdbf6f", "#fdbf6f", "#fdbf6f",
                    "#bebada","#bebada","#bebada","#bebada","#bebada","#a6d854",
                    "#a6d854", "#a6d854"))+
  xlab("")+
  ylab("Parameter estimate")+
  theme(legend.position ="none",
        axis.title = element_text(size=12, colour="black", face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=12, colour="black"))+
  annotate("text",x=1,y=6.3291,label="***",size=5)+
  annotate("text",x=2,y=5.8507,label="***",size=5)+
  annotate("text",x=3,y=4.103,label="*",size=5)+
  annotate("text",x=4,y=3.0492,label="",size=5)+
  annotate("text",x=5,y=3.3594,label="***",size=5)+
  annotate("text",x=6,y=3.3998,label="***",size=5)+
  annotate("text",x=7,y=2.9327,label="***",size=5)+
  annotate("text",x=8,y=5.283,label="***",size=5)+
  annotate("text",x=9,y=1.7721,label="***",size=5)+
  annotate("text",x=10,y=2.4669,label="***",size=5)+
  annotate("text",x=11,y=-3.3213,label="***",size=5)+
  annotate("text",x=12,y=0.3846,label="",size=5)+
  annotate("text",x=13,y=2.148,label="",size=5)+
  annotate("text",x=14,y=2.2146,label="***",size=5)+
  annotate("text",x=15,y=-11.9624,label="***",size=5)+
  annotate("text",x=16,y=-3.4173,label="***",size=5)+
  annotate("text",x=17,y=0.4489,label="",size=5)+
  annotate("text",x=18,y=1.8763,label="***",size=5)+
  annotate("text",x=19,y=-1.685,label="",size=5)+
  annotate("text",x=20,y=8.6054,label="***",size=5)+
  annotate("text",x=21,y=-5.907,label="***",size=5)+
  annotate("text",x=22,y=-1.2572,label="",size=5)+
  annotate("text",x=23,y=0.9079,label="***",size=5)+
  annotate("text",x=11.5,y=11,label="MD",size=5, face = "bold", colour="black")+
  annotate("text",x=6,y=10,label="Management practices",size=4, face = "bold", colour="black")+
  annotate("text",x=16,y=10,label="Site factors",size=4, face = "bold", colour="black")+
  annotate("text",x=21,y=-10,label= expression(paste(Pseudo~R^2,"= 0.82")), size=4, face = "bold", colour="deeppink4")

p2

######################################## SMD Method ############################################

d3 <- readxl::read_xlsx('Database.xlsx',sheet = "Figure3c")
d3 <- as.data.table(d3)


d3$Moderator1 <- factor(d2$Moderator1, levels = c("EE",
                                                  "CF",
                                                  "OF",
                                                  "MF",
                                                  "RFP",
                                                  "RFR",
                                                  "RFT",
                                                  "BC",
                                                  "RES",
                                                  "CC/ROT",
                                                  "ZT/RT",
                                                  "Cr_w",
                                                  "Cr_m",
                                                  "Cr_r",
                                                  "N_sc",
                                                  "Clay_sc",
                                                  "pH_sc",
                                                  "MAP_sc",
                                                  "MAT_sc",
                                                  "N_sq",
                                                  "RFP˟Cr_m",
                                                  "MAT_sc˟Cr_m",
                                                  "N_sc˟SOC_sc"))

p3 <- ggplot(d3,aes(Moderator1,Parameter_estimate))+
  geom_col()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 11.5), fill = "#FBE7DD")+
  geom_rect(aes(ymin = -Inf, xmin = 11.5, ymax = Inf, xmax = Inf), fill = "#E6E5E3")+
  geom_col(fill = c("#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#66c2a5","#e78ac3",
                    "#e78ac3","#fb8072", "#fdbf6f",
                    "#fdbf6f", "#fdbf6f", "#fdbf6f",
                    "#bebada","#bebada","#bebada","#bebada","#bebada","#a6d854",
                    "#a6d854", "#a6d854"))+
  xlab("Management practices & Site factors")+
  ylab("Parameter estimate")+
  theme(legend.position ="none",
        axis.title = element_text(size=12, face = "bold", colour="black"),
        axis.text.x = element_text(size=12,angle = 45, hjust = 1, vjust = 1, colour="black"),
        axis.text.y = element_text(size=12, colour="black"))+
  annotate("text",x=1,y=1.4266,label="***",size=5)+
  annotate("text",x=2,y=1.0966,label="***",size=5)+
  annotate("text",x=3,y=0.3962,label="*",size=5)+
  annotate("text",x=4,y=0.6368,label="*",size=5)+
  annotate("text",x=5,y=0.9431,label="***",size=5)+
  annotate("text",x=6,y=0.9412,label="***",size=5)+
  annotate("text",x=7,y=0.6262,label="***",size=5)+
  annotate("text",x=8,y=1.0312,label="***",size=5)+
  annotate("text",x=9,y=0.298,label="***",size=5)+
  annotate("text",x=10,y=0.4423,label="***",size=5)+
  annotate("text",x=11,y=-0.5463,label="***",size=5)+
  annotate("text",x=12,y=-0.2845,label="***",size=5)+
  annotate("text",x=13,y=-0.233,label="***",size=5)+
  annotate("text",x=14,y=-0.3442,label="***",size=5)+
  annotate("text",x=15,y=-0.1854,label="***",size=5)+
  annotate("text",x=16,y=-0.2187,label="***",size=5)+
  annotate("text",x=17,y=0.1378,label="***",size=5)+
  annotate("text",x=18,y=0.2431,label="***",size=5)+
  annotate("text",x=19,y=-0.2128,label="***",size=5)+
  annotate("text",x=20,y=0.143,label="***",size=5)+
  annotate("text",x=21,y=-0.7713,label="***",size=5)+
  annotate("text",x=22,y=0.1502,label="***",size=5)+
  annotate("text",x=23,y=-0.1248,label="***",size=5)+
  annotate("text",x=11.5,y=1.48,label="SMD",size=5, face = "bold", colour="black")+
  annotate("text",x=6,y=1.38,label="Management practices",size=4, face = "bold", colour="black")+
  annotate("text",x=16,y=1.38,label="Site factors",size=4, face = "bold", colour="black")+
  annotate("text",x=21,y=1.3,label= expression(paste(Pseudo~R^2,"= 0.65")), size=4, face = "bold", colour="deeppink4")
    
p3

p<-ggarrange(p1, p2, p3, ncol = 1, nrow = 3,align = "v",heights = c(2,2,2.9),
             labels = c("a", "b","c"), font.label=list(size=14, color = "black", face = "bold"), label.x = 0, label.y = 1,hjust = -0.2, vjust = 1)
p

ggsave(file = "meta_regression.png", width = 180, height = 210, units = "mm")

