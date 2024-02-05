#Figure1 map for paper3

library(tidyverse)
library(data.table)
site <- readxl::read_xlsx('.../You_paper3_S2.xlsx',sheet = 1)
site <- as.data.table(site)
site[management=='OF', management := 'Nutrient management']
site[management=='CF', management := 'Nutrient management']
site[management=='RFR', management := 'Nutrient management']
site[management=='RFT', management := 'Nutrient management']
site[management=='RFP', management := 'Nutrient management']
site[management=='EE', management := 'Nutrient management']
site[management=='BC', management := 'Nutrient management']
site[management=='ROT', management := 'Crop management']
site[management=='CC', management := 'Crop management']
site[management=='RES', management := 'Crop management']
site[management=='NT', management := 'Soil management']
site[management=='RT', management := 'Soil management']

names(site)[names(site)=="management"] <- "Management practice"
site$`Management practice` <- factor(site$`Management practice`, levels = c('Nutrient management','Crop management','Soil management'))

world <- map_data("world")
world <- subset(world, region != "Antarctica") # delete Antarctica

p1 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "#838B8B", fill = "#E0EEEE", size = 0.1) +
  geom_point(data = site,aes(lon, lat,color = `Management practice`), alpha = 1, size = 1) +
  scale_color_manual(values = c("Nutrient management" = "indianred3","Crop management"= "seagreen3","Soil management"="royalblue3"))+
  theme_bw()+
  theme(panel.grid =element_blank()) +  
  theme(axis.text = element_blank()) +  
  theme(axis.ticks = element_blank()) +  
  theme(panel.border = element_blank())+ 
  theme(axis.title = element_blank()) + 
  theme(legend.position = c(0.1,0.3))+theme(legend.text = element_text(size=12, color="black"))+
  theme(legend.background = element_blank())+
  theme(legend.title = element_text(face="bold", size=12, color="black"))


#
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(readxl)
library(metafor)
library(glmulti)
library(reshape2)
library(Hmisc)
library(psych)

metaresult_group<-read.csv(".../metaresult_paper3.csv",header=TRUE,na.strings = "")

mydata<- subset(metaresult_group[which(metaresult_group$Group.type=="N2O"),], Vari=="N2O emission")

p2<- ggplot(data = mydata,aes(x=Management,y=mean,shape=Group,fill=Group)) +
  geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 7.5), fill = "#E7DAD2")+
  geom_rect(aes(ymin = -Inf, xmin = 7.5, ymax = Inf, xmax = 10.5), fill = "#E0EEEE")+
  geom_rect(aes(ymin = -Inf, xmin = 10.5, ymax = Inf, xmax = Inf), fill = "#DCDCDC")+
  geom_hline(yintercept=0,linetype = "dashed",size=0.1)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.1,size=0.4)+
  geom_point(position=position_dodge(0.7), size=2, stroke = 0) +  
  scale_shape_manual(values=c("Meta-analytical data"=21,"Primary data"=23))+
  geom_text(aes(x = Management, y = ci.ub +6, label = n),position = position_dodge(width = 0.7),vjust = 0, hjust=0.5, size = 3, check_overlap = FALSE)+
  scale_x_discrete(limits=rev(c("Reduced tillage","No tillage","Crop rotation","Cover cropping","Residue retention","Biochar","Fertilizer placement","Fertilizer timing","Fertilizer rate","Organic fertilizer","Combined fertilizer","Enhanced efficiency")),labels = rev(c("RT","ZT","ROT","CC","RES","BC","RFP","RFT","RFR","OF","CF","EE")))+
  scale_y_continuous(limits=c(-100,100), breaks = c(-100,-50,0,50,100))+
  labs(x = "Management practice", y = expression("N"[2]*"O emission (%)"),colour = 'black')+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position="none",
        legend.key = element_rect(fill = "white",size = 1.5),
        legend.key.width = unit(0.4,"lines"),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_blank(),
        legend.text=element_text(colour = 'black', size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y =element_text(size=12, colour = 'black', face='bold'),
        axis.title.x=element_blank(),
        axis.text.y = element_text(colour = 'black', size = 12),
        
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank())

mydata<- subset(metaresult_group[which(metaresult_group$Group.type=="NH3"),], Vari=="NH3 volatilization")

p3<- ggplot(data = mydata,aes(x=Management,y=mean,shape=Group,fill=Group)) +
  geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 7.5), fill = "#E7DAD2")+
  geom_rect(aes(ymin = -Inf, xmin = 7.5, ymax = Inf, xmax = 10.5), fill = "#E0EEEE")+
  geom_rect(aes(ymin = -Inf, xmin = 10.5, ymax = Inf, xmax = Inf), fill = "#DCDCDC")+
  geom_hline(yintercept=0,linetype = "dashed",size=0.1)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.1,size=0.4)+
  geom_point(position=position_dodge(0.7), size=2, stroke = 0) +  
  scale_shape_manual(values=c("Meta-analytical data"=21,"Primary data"=23))+
  geom_text(aes(x = Management, y = ci.ub +6, label = n),position = position_dodge(width = 0.7),vjust = 0, hjust=0.5, size = 3, check_overlap = FALSE)+
  scale_x_discrete(limits=rev(c("Reduced tillage","No tillage","Crop rotation","Cover cropping","Residue retention","Biochar","Fertilizer placement","Fertilizer timing","Fertilizer rate","Organic fertilizer","Combined fertilizer","Enhanced efficiency")),labels = rev(c("RT","ZT","ROT","CC","RES","BC","RFP","RFT","RFR","OF","CF","EE")))+
  scale_y_continuous(limits=c(-100,100), breaks = c(-100,-50,0,50,100))+
  labs(x = "Management practice", y = expression("NH"[3]*" emission (%)"),colour = 'black')+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position="none",
        legend.key = element_rect(fill = "white",size = 1.5),
        legend.key.width = unit(0.4,"lines"),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_blank(),
        legend.text=element_text(colour = 'black', size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, colour = 'black', face='bold'),
        axis.title.x=element_blank(),
        axis.text.y = element_text(colour = 'black', size = 12),
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank())

mydata<- subset(metaresult_group[which(metaresult_group$Group.type=="runoff"),], Vari=="N runoff")

p4<- ggplot(data = mydata,aes(x=Management,y=mean,shape=Group,fill=Group)) +
  geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 7.5), fill = "#E7DAD2")+
  geom_rect(aes(ymin = -Inf, xmin = 7.5, ymax = Inf, xmax = 10.5), fill = "#E0EEEE")+
  geom_rect(aes(ymin = -Inf, xmin = 10.5, ymax = Inf, xmax = Inf), fill = "#DCDCDC")+
  geom_hline(yintercept=0,linetype = "dashed",size=0.1)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.1,size=0.4)+
  geom_point(position=position_dodge(0.7), size=2, stroke = 0) +  
  scale_shape_manual(values=c("Meta-analytical data"=21,"Primary data"=23))+
  geom_text(aes(x = Management, y = ci.ub +6, label = n),position = position_dodge(width = 0.7),vjust = 0, hjust=0.5, size = 3, check_overlap = FALSE)+
  scale_x_discrete(limits=rev(c("Reduced tillage","No tillage","Crop rotation","Cover cropping","Residue retention","Biochar","Fertilizer placement","Fertilizer timing","Fertilizer rate","Organic fertilizer","Combined fertilizer","Enhanced efficiency")),labels = rev(c("RT","ZT","ROT","CC","RES","BC","RFP","RFT","RFR","OF","CF","EE")))+
  scale_y_continuous(limits=c(-100,100), breaks = c(-100,-50,0,50,100))+
  labs(x = "Management practice", y = expression("N runoff (%)"),colour = 'black')+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position="none",
        legend.key = element_rect(fill = "white",size = 1.5),
        legend.key.width = unit(0.4,"lines"),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_blank(),
        legend.text=element_text(colour = 'black', size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, colour = 'black', face='bold'),
        axis.title.x=element_blank(),
        axis.text.y = element_text(colour = 'black', size = 12),
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank())


mydata<- subset(metaresult_group[which(metaresult_group$Group.type=="leaching"),], Vari=="N leaching")

p5<- ggplot(data = mydata,aes(x=Management,y=mean,shape=Group,fill=Group)) +
  geom_rect(aes(ymin = -Inf, xmin = -Inf, ymax = Inf, xmax = 7.5), fill = "#E7DAD2")+
  geom_rect(aes(ymin = -Inf, xmin = 7.5, ymax = Inf, xmax = 10.5), fill = "#E0EEEE")+
  geom_rect(aes(ymin = -Inf, xmin = 10.5, ymax = Inf, xmax = Inf), fill = "#DCDCDC")+
  geom_hline(yintercept=0,linetype = "dashed",size=0.1)+
  geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.1,size=0.4)+
  geom_point(position=position_dodge(0.7), size=2, stroke = 0) +  
  scale_shape_manual(values=c("Meta-analytical data"=21,"Primary data"=23))+
  geom_text(aes(x = Management, y = ci.ub +6, label = n),position = position_dodge(width = 0.7),vjust = 0, hjust=0.5, size = 3, check_overlap = FALSE)+
  scale_x_discrete(limits=rev(c("Reduced tillage","No tillage","Crop rotation","Cover cropping","Residue retention","Biochar","Fertilizer placement","Fertilizer timing","Fertilizer rate","Organic fertilizer","Combined fertilizer","Enhanced efficiency")),labels = rev(c("RT","ZT","ROT","CC","RES","BC","RFP","RFT","RFR","OF","CF","EE")))+
  scale_y_continuous(limits=c(-100,100), breaks = c(-100,-50,0,50,100))+
  labs(x = "Management practice", y = expression("N leaching (%)"),colour = 'black')+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position=c(0.5,0.07),
        legend.key = element_blank(),
        legend.key.width = unit(0.4,"lines"),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_blank(),
        legend.text=element_text(colour = 'black', size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, colour = 'black', face='bold'),
        axis.title.x = element_text(size=12, colour = 'black', face='bold'),
        #axis.title.x=element_blank(),
        axis.text.y = element_text(colour = 'black', size = 12),
        axis.text.x = element_text(colour = 'black', size = 12, hjust = 0.5, vjust = 0.5))

p<-ggarrange(p1, p2, p3, p4, p5, ncol = 1, nrow = 5, align = "v",heights = c(3,2,2,2,2.5),
             labels = c("a", "b","c","d","e"), label.x = 0.96,label.y = 0.9,
             font.label=list(size=12,color = "black", face = "bold"),hjust = -0.2, vjust = 1)

ggsave(file = ".../Figure1_paper3.tiff",width = 179,height = 220, units = "mm")