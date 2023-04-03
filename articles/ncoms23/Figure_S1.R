#Figure S1

# load packages
library(data.table); library(gridExtra); library(cowplot)

# load the data file
d1 <- readxl::read_xlsx('data/20220329_1_Database impacts measures on NUE_add 2.xlsx',sheet = 1)
d1 <- as.data.table(d1)

# default format
plot_dff <-  theme(axis.title= element_text(size=14, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5),
                   axis.text = element_text(size=12, family="myFont", color="black", face= "bold", vjust=0.5, hjust=0.5))

# make plots
p1 <- ggplot(data = d1, aes(x=evaporation))+
      geom_histogram(binwidth=10,fill="darkseagreen", color="darksalmon", alpha=0.5)+
      theme_bw()+labs(x= "Evaporation (mm)", y= "Count")+ plot_dff
p2 <- ggplot(data = d1, aes(x=mat))+
      geom_histogram(binwidth=1,fill="#F39B7FB2", color="darksalmon", alpha=0.5)+
      theme_bw()+labs(x= "MAT (°C)", y= "Count") + plot_dff
p3 <- ggplot(data = d1, aes(x=map))+
      geom_histogram(binwidth=70,fill="#F39B7FB2", color="darksalmon", alpha=0.5)+
      theme_bw()+ labs(x= "MAP (mm)", y= "Count") + plot_dff
p4 <- ggplot(data = d1, aes(x=elevation))+
      geom_histogram(binwidth=70,fill="darkseagreen", color="darksalmon", alpha=0.5)+
      theme_bw()+ labs(x= "Elevation (m)", y= "Count")+ plot_dff
p5 <- ggplot(data = d1, aes(x=bulk_density))+
      geom_histogram(binwidth=0.03,fill="blue", color="red", alpha=0.5)+ xlim(0.75, 1.8)+
      theme_bw()+ labs(x= "Bulk density (kg/dm3)", y= "Count")+ plot_dff
p6 <- ggplot(data = d1, aes(x=clay))+
      geom_histogram(binwidth=1.5,fill="#00A087B2", color="firebrick", alpha=0.5)+
      theme_bw()+ labs(x= "Clay (%)", y= "Count")+ plot_dff
p7 <- ggplot(data = d1, aes(x=cec))+
      geom_histogram(binwidth=1,fill="blue", color="red", alpha=0.5)+
      theme_bw()+ labs(x= "CEC (cmol/kg)", y= "Count")+ plot_dff
p8 <- ggplot(data = d1, aes(x=total_nitrogen))+
      geom_histogram(binwidth=1.5,fill="blue", color="red", alpha=0.5)+
      theme_bw()+labs(x= "Total N (mg/kg)", y= "Count")+ plot_dff
p9 <- ggplot(data = d1, aes(x=soc))+
      geom_histogram(binwidth=2.5,fill="#00A087B2", color="firebrick", alpha=0.5)+
      theme_bw()+ labs(x= "SOC (g/kg)", y= "Count")+ plot_dff
p10 <- ggplot(data = d1, aes(x=ph))+
       geom_histogram(binwidth=0.13,fill="#00A087B2", color="firebrick", alpha=0.5)+
       theme_bw()+ labs(x= "pH", y= "Count")+ plot_dff
p11 <- ggplot(data = d1, aes(x=n_dose))+
       geom_histogram(binwidth=25,fill="#3C5488B2",color="firebrick", alpha=0.5)+ xlim(0, 900)+
       theme_bw()+ labs(x= "N rate (kg/ha)", y= "Count")+ plot_dff
p12 <- ggplot(data = d1, aes(x=p_dose))+
       geom_histogram(binwidth=25,fill="forestgreen", color="firebrick", alpha=0.5)+
       theme_bw()+ labs(x= "P dose (kg/ha)", y= "Count")+ plot_dff
p13 <- ggplot(data = d1, aes(x=p_dose))+
       geom_histogram(binwidth=25,fill="forestgreen", color="firebrick", alpha=0.5)+
       theme_bw()+ labs(x= "K dose (kg/ha)", y= "Count")+plot_dff

# combine relevant figures in 3*4
p <- plot_grid(p2, p3, p6, p9, p10, p11, ncol=2, nrow=3,
          labels= c(" a", "b", "c", "d", "e", " f"), label_size=18,
          label_fontfamily = "Arial", label_fontface = "bold", label_colour = "black")

# save the figure
#ggsave(file = "frequency_distribution_histogram.png")
ggsave(plot = p, file = "products/frequency_distribution_histogram.png",width = 410,height = 297, units = "mm")
