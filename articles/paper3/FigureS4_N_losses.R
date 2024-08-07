# plotting

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(cowplot)
library(vcd)
library(RColorBrewer)
# Create a custom color palette
color_palette <- colorRampPalette(c("blue","lightgreen", "red"))(21)

#EE
theme_set(theme_bw())

r1 <- terra::rast('.../scenario_runoff_ee_1.tif')

r1.p <- as.data.frame(r1,xy=TRUE)

r1.p$improvement <- ifelse(r1.p$improvement < -100 | r1.p$improvement > 100, NA, r1.p$improvement)
r1.p <- r1.p[!is.na(r1.p$improvement),]
r1.p.mean1 <- paste0(round(mean(r1.p$improvement, digits = 1), 1),'%')

world <- ne_countries(scale = "medium", returnclass = "sf")

p1 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r1.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of N runoff (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("EE") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r1.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326) 

#CF
theme_set(theme_bw())

r2 <- terra::rast('.../scenario_runoff_cf_2.tif')

r2.p <- as.data.frame(r2,xy=TRUE)

r2.p$improvement <- ifelse(r2.p$improvement < -100 | r2.p$improvement > 100, NA, r2.p$improvement)
r2.p <- r2.p[!is.na(r2.p$improvement),]
r2.p.mean1 <- paste0(round(mean(r2.p$improvement, digits = 1), 1),'%')

world <- ne_countries(scale = "medium", returnclass = "sf")

p2 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r2.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of N runoff (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("CF") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r2.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#OF
theme_set(theme_bw())

r3 <- terra::rast('.../scenario_runoff_of_3.tif')

r3.p <- as.data.frame(r3,xy=TRUE)

r3.p$improvement <- ifelse(r3.p$improvement < -100 | r3.p$improvement > 100, NA, r3.p$improvement)
r3.p <- r3.p[!is.na(r3.p$improvement),]
r3.p.mean1 <- paste0(round(mean(r3.p$improvement, digits = 1), 1),'%')

world <- ne_countries(scale = "medium", returnclass = "sf")

p3 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r3.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of N runoff (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("OF") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r3.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#RFR
theme_set(theme_bw())

r4 <- terra::rast('.../scenario_runoff_rfr_4.tif')

r4.p <- as.data.frame(r4,xy=TRUE)

r4.p$improvement <- ifelse(r4.p$improvement < -100 | r4.p$improvement > 100, NA, r4.p$improvement)
r4.p <- r4.p[!is.na(r4.p$improvement),]
r4.p.mean1 <- paste0(round(mean(r4.p$improvement, digits = 1), 1),'%')

world <- ne_countries(scale = "medium", returnclass = "sf")

p4 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r4.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of N runoff (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFR") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r4.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#RFT
theme_set(theme_bw())

r5 <- terra::rast('.../scenario_runoff_rft_5.tif')

r5.p <- as.data.frame(r5,xy=TRUE)

r5.p$improvement <- ifelse(r5.p$improvement < -100 | r5.p$improvement > 100, NA, r5.p$improvement)
r5.p <- r5.p[!is.na(r5.p$improvement),]
r5.p.mean1 <- paste0(round(mean(r5.p$improvement, digits = 1), 1),'%')

world <- ne_countries(scale = "medium", returnclass = "sf")

p5 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r5.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of N runoff (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFT") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r5.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#NT
theme_set(theme_bw())

r6 <- terra::rast('.../scenario_runoff_nt_6.tif')

r6.p <- as.data.frame(r6,xy=TRUE)

r6.p$improvement <- ifelse(r6.p$improvement < -100 | r6.p$improvement > 100, NA, r6.p$improvement)
r6.p <- r6.p[!is.na(r6.p$improvement),]
r6.p.mean1 <- paste0(round(mean(r6.p$improvement, digits = 1), 1),'%')

world <- ne_countries(scale = "medium", returnclass = "sf")

p6 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r6.p, aes(x = x, y = y, name = 'none',
                             fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of N runoff (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("ZT") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r6.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#combine figures

library(ggpubr)
p_run<-ggarrange(p1,p2,p3,p4,p5,p6, heights = c(4, 4, 4, 4), ncol = 3, nrow = 2,
                   labels = c("a", "b","c","d","e","f"),font.label=list(size=12,color = "black", face = "bold"),
                   common.legend = TRUE, legend = "bottom", 
                   hjust = 0, vjust = 1.5)

ggsave(p_run, file = ".../Figure_S_run.png",width = 179,height = 80, units = "mm")

