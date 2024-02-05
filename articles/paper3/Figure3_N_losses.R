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
color_palette <- colorRampPalette(c("blue","lightgreen","red"))(21)

## scenario_N2O (combined optimal management practices) 
theme_set(theme_bw())

# get the raster to plot
r1 <- terra::rast('.../scenario_n2o_combination.tif')

r1.p <- as.data.frame(r1, xy=TRUE)
r1.p$mean_improvement <- ifelse(r1.p$mean_improvement < -100 | r1.p$mean_improvement > 100, NA, r1.p$mean_improvement)
r1.p <- r1.p[!is.na(r1.p$mean_improvement),]
r1.p.mean <- paste0(round(mean(r1.p$mean_improvement, digits = 1), 1),'%')

r1.p$mean_improvement <- ifelse(r1.p$mean_improvement < -40, -40, 
                            ifelse(r1.p$mean_improvement > 0, 0, r1.p$mean_improvement))

r1.p <- r1.p[!is.na(r1.p$mean_improvement),]

sd_n2o <- sd(r1.p$mean_improvement)

world <- ne_countries(scale = "medium", returnclass = "sf")

p1 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r1.p, aes(x = x, y = y, name = 'none',
                             fill = mean_improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-40,-30,-20,-10,0),
                       labels = c("-40","-30","-20","-10","0"),
                       limits = c(-40,0))+
  labs(fill = expression("Relative change (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(expression("N"[2]*"O emission")) +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r1.p.mean),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#scenario_NH3 (combined optimal management practices) 

theme_set(theme_bw())

r2 <- terra::rast('.../scenario_nh3_combination.tif')

r2.p <- as.data.frame(r2, xy=TRUE)
r2.p$mean_improvement <- ifelse(r2.p$mean_improvement < -100 | r2.p$mean_improvement > 100, NA, r2.p$mean_improvement)
r2.p <- r2.p[!is.na(r2.p$mean_improvement),]
r2.p.mean <- paste0(round(mean(r2.p$mean_improvement, digits = 1), 1),'%')
r2.p$mean_improvement <- ifelse(r2.p$mean_improvement < -40, -40, 
                           ifelse(r2.p$mean_improvement > 0, 0, r2.p$mean_improvement))

r2.p <- r2.p[!is.na(r2.p$mean_improvement),]

sd_nh3 <- sd(r2.p$mean_improvement)

world <- ne_countries(scale = "medium", returnclass = "sf")

p2 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r2.p, aes(x = x, y = y, name = 'none',
                             fill = mean_improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-40,-30,-20,-10,0),
                       labels = c("-40","-30","-20","-10","0"),
                       limits = c(-40,0))+
  labs(fill = expression("Relative change (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(expression("NH"[3]*" emission")) +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r2.p.mean),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#scenario_runoff (combined optimal management practices)
theme_set(theme_bw())

r3 <- terra::rast('.../scenario_runoff_combination.tif')

r3.p <- as.data.frame(r3, xy=TRUE)
r3.p$mean_improvement <- ifelse(r3.p$mean_improvement < -100 | r3.p$mean_improvement > 100, NA, r3.p$mean_improvement)
r3.p <- r3.p[!is.na(r3.p$mean_improvement),]
r3.p.mean <- paste0(round(mean(r3.p$mean_improvement, digits = 1), 1),'%')

r3.p$mean_improvement <- ifelse(r3.p$mean_improvement < -40, -40, 
                           ifelse(r3.p$mean_improvement > 0, 0, r3.p$mean_improvement))

r3.p <- r3.p[!is.na(r3.p$mean_improvement),]

sd_runoff <- sd(r3.p$mean_improvement)

world <- ne_countries(scale = "medium", returnclass = "sf")

p3 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r3.p, aes(x = x, y = y, name = 'none',
                                fill = mean_improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-40,-30,-20,-10,0),
                       labels = c("-40","-30","-20","-10","0"),
                       limits = c(-40,0))+
  labs(fill = expression("Relative change (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("N runoff") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r3.p.mean),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#cenario_leaching (combined optimal management practices) 
theme_set(theme_bw())

r4 <- terra::rast('.../scenario_leaching_combination.tif')

r4.p <- as.data.frame(r4, xy=TRUE)
r4.p$mean_improvement <- ifelse(r4.p$mean_improvement < -100 | r4.p$mean_improvement > 100, NA, r4.p$mean_improvement)
r4.p <- r4.p[!is.na(r4.p$mean_improvement),]
r4.p.mean <- paste0(round(mean(r4.p$mean_improvement, digits = 1), 1),'%')

r4.p$mean_improvement <- ifelse(r4.p$mean_improvement < -40, -40, 
                           ifelse(r4.p$mean_improvement > 0, 0, r4.p$mean_improvement))

r4.p <- r4.p[!is.na(r4.p$mean_improvement),]

sd_leaching <- sd(r4.p$mean_improvement)

world <- ne_countries(scale = "medium", returnclass = "sf")

p4 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r4.p, aes(x = x, y = y, name = 'none',
                             fill = mean_improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-40,-30,-20,-10,0),
                       labels = c("-40","-30","-20","-10","0"),
                       limits = c(-40,0))+
  labs(fill = expression("Relative change (%)")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("N leaching") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r4.p.mean),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#combine figures
library(ggpubr)
p<-ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2,
                labels = c("a","b","c","d"),font.label=list(size=12,color = "black", face = "bold"),
                common.legend = TRUE, legend = "bottom", 
                hjust = 0, vjust = 1.5)

ggsave(p, file = ".../Figure_3.png",width = 179,height = 120, units = "mm")
