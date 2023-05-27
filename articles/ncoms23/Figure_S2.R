#Figure S2

# load packages
library(data.table); library(gridExtra); library(cowplot)

# load the data file
d1 <- readxl::read_xlsx('C:/Users/86188/Desktop/Source Data.xlsx',sheet = "FigureS2")
d1 <- as.data.table(d1)

# make plots
p1 <- ggplot(data = d1, aes(x=mat))+
  geom_histogram(binwidth=1,fill="#F39B7FB2", 
                 color="darksalmon", alpha=0.5)+
  theme_bw()+labs(x= "MAT (°C)", y= "Count")+ theme(axis.title= element_text(size=22, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))+ theme(axis.text = element_text(size=20, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))

p2 <- ggplot(data = d1, aes(x=map))+
  geom_histogram(binwidth=70,fill="#F39B7FB2", 
                 color="darksalmon", alpha=0.5)+
  theme_bw()+ labs(x= "MAP (mm)", y= "Count")+ theme(axis.title= element_text(size=22, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))+ theme(axis.text = element_text(size=20, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5)) 

p3 <- ggplot(data = d1, aes(x=clay))+
  geom_histogram(binwidth=1.5,fill="#00A087B2", 
                 color="firebrick", alpha=0.5)+
  theme_bw()+ labs(x= "Clay (%)", y= "Count")+ theme(axis.title= element_text(size=22, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))+ theme(axis.text = element_text(size=20, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5)) 

p4 <- ggplot(data = d1, aes(x=soc))+
  geom_histogram(binwidth=2.5,fill="#00A087B2", 
                 color="firebrick", alpha=0.5)+
  theme_bw()+ labs(x= "SOC (g/kg)", y= "Count")+ theme(axis.title= element_text(size=22, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))+ theme(axis.text = element_text(size=20, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5)) 

p5 <- ggplot(data = d1, aes(x=ph))+
  geom_histogram(binwidth=0.13,fill="#00A087B2", 
                 color="firebrick", alpha=0.5)+
  theme_bw()+ labs(x= "pH", y= "Count")+ theme(axis.title= element_text(size=22, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))+ theme(axis.text = element_text(size=20, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5)) 

p6 <- ggplot(data = d1, aes(x=n_dose))+
  geom_histogram(binwidth=25,fill="#3C5488B2", 
                 color="firebrick", alpha=0.5)+
  xlim(0, 900)+
  theme_bw()+ labs(x= "N rate (kg/ha)", y= "Count")+ theme(axis.title= element_text(size=22, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))+ theme(axis.text = element_text(size=20, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5)) 

#3*4
plot_grid(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3, labels= c("a", "b", "c", "d", "e", "f"), label_size=28, label_fontfamily = "Arial", label_fontface = "bold", label_colour = "black", label_x = -0.01, label_y = 1.02)

ggsave(file = "C:/Users/86188/Desktop/Figures/Figure_S2.png",width = 410,height = 297, units = "mm")
