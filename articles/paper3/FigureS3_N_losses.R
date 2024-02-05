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

r10 <- terra::rast('.../scenario_nh3_ee_1.tif')

r10.p <- as.data.frame(r10, xy=TRUE)
r10.p$improvement <- ifelse(r10.p$improvement < -100 | r10.p$improvement > 100, NA, r10.p$improvement)
r10.p <- r10.p[!is.na(r10.p$improvement),]
r10.p.mean1 <- paste0(round(mean(r10.p$improvement, digits = 1), 1),'%')

r10.p$improvement <- ifelse(r10.p$improvement < -20, -20, 
                            ifelse(r10.p$improvement > 20, 20, r10.p$improvement))

r10.p <- r10.p[!is.na(r10.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p10 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r10.p, aes(x = x, y = y, name = 'none',fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("EE") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r10.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#CF
theme_set(theme_bw())

r11 <- terra::rast('.../scenario_nh3_cf_2.tif')

r11.p <- as.data.frame(r11, xy=TRUE)
r11.p$improvement <- ifelse(r11.p$improvement < -100 | r11.p$improvement > 100, NA, r11.p$improvement)
r11.p <- r11.p[!is.na(r11.p$improvement),]
r11.p.mean1 <- paste0(round(mean(r11.p$improvement, digits = 1), 1),'%')
r11.p$improvement <- ifelse(r11.p$improvement < -20, -20, 
                            ifelse(r11.p$improvement > 20, 20, r11.p$improvement))

r11.p <- r11.p[!is.na(r11.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p11 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r11.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("CF") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r11.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#OF
theme_set(theme_bw())

r12 <- terra::rast('.../scenario_nh3_of_3.tif')

r12.p <- as.data.frame(r12, xy=TRUE)
r12.p$improvement <- ifelse(r12.p$improvement < -100 | r12.p$improvement > 100, NA, r12.p$improvement)
r12.p <- r12.p[!is.na(r12.p$improvement),]
r12.p.mean1 <- paste0(round(mean(r12.p$improvement, digits = 1), 1),'%')

r12.p$improvement <- ifelse(r12.p$improvement < -20, -20, 
                            ifelse(r12.p$improvement > 20, 20, r12.p$improvement))

r12.p <- r12.p[!is.na(r12.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p12 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r12.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("OF") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r12.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#RFR
theme_set(theme_bw())

r13 <- terra::rast('.../scenario_nh3_rfr_4.tif')

r13.p <- as.data.frame(r13, xy=TRUE)
r13.p$improvement <- ifelse(r13.p$improvement < -100 | r13.p$improvement > 100, NA, r13.p$improvement)
r13.p <- r13.p[!is.na(r13.p$improvement),]
r13.p.mean1 <- paste0(round(mean(r13.p$improvement, digits = 1), 1),'%')

r13.p$improvement <- ifelse(r13.p$improvement < -20, -20, 
                            ifelse(r13.p$improvement > 20, 20, r13.p$improvement))

r13.p <- r13.p[!is.na(r13.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p13 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r13.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFR") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r13.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#RFT
theme_set(theme_bw())

r14 <- terra::rast('.../scenario_nh3_rft_5.tif')

r14.p <- as.data.frame(r14, xy=TRUE)
r14.p$improvement <- ifelse(r14.p$improvement < -100 | r14.p$improvement > 100, NA, r14.p$improvement)
r14.p <- r14.p[!is.na(r14.p$improvement),]
r14.p.mean1 <- paste0(round(mean(r14.p$improvement, digits = 1), 1),'%')

r14.p$improvement <- ifelse(r14.p$improvement < -20, -20, 
                            ifelse(r14.p$improvement > 20, 20, r14.p$improvement))

r14.p <- r14.p[!is.na(r14.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p14 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r14.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFT") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r14.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#RFP
theme_set(theme_bw())

r15 <- terra::rast('.../scenario_nh3_rfp_6.tif')

r15.p <- as.data.frame(r15, xy=TRUE)
r15.p$improvement <- ifelse(r15.p$improvement < -100 | r15.p$improvement > 100, NA, r15.p$improvement)
r15.p <- r15.p[!is.na(r15.p$improvement),]
r15.p.mean1 <- paste0(round(mean(r15.p$improvement, digits = 1), 1),'%')
r15.p$improvement <- ifelse(r15.p$improvement < -20, -20, 
                            ifelse(r15.p$improvement > 20, 20, r15.p$improvement))

r15.p <- r15.p[!is.na(r15.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p15 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r15.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFP") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r15.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

# RES
theme_set(theme_bw())

r17 <- terra::rast('.../scenario_nh3_res_8.tif')

r17.p <- as.data.frame(r17, xy=TRUE)
r17.p$improvement <- ifelse(r17.p$improvement < -100 | r17.p$improvement > 100, NA, r17.p$improvement)
r17.p <- r17.p[!is.na(r17.p$improvement),]
r17.p.mean1 <- paste0(round(mean(r17.p$improvement, digits = 1), 1),'%')

r17.p$improvement <- ifelse(r17.p$improvement < -20, -20, 
                            ifelse(r17.p$improvement > 20, 20, r17.p$improvement))

r17.p <- r17.p[!is.na(r17.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p17 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r17.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RES") +
  theme(plot.title = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r17.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#ROT
theme_set(theme_bw())

r19 <- terra::rast('.../scenario_nh3_rot_10.tif')

r19.p <- as.data.frame(r19, xy=TRUE)
r19.p$improvement <- ifelse(r19.p$improvement < -100 | r19.p$improvement > 100, NA, r19.p$improvement)
r19.p <- r19.p[!is.na(r19.p$improvement),]
r19.p.mean1 <- paste0(round(mean(r19.p$improvement, digits = 1), 1),'%')

r19.p$improvement <- ifelse(r19.p$improvement < -20, -20, 
                            ifelse(r19.p$improvement > 20, 20, r19.p$improvement))

r19.p <- r19.p[!is.na(r19.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p19 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r19.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("ROT") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r19.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#NT/RT
theme_set(theme_bw())

r20 <- terra::rast('.../scenario_nh3_nt_11.tif')

r20.p <- as.data.frame(r20, xy=TRUE)
r20.p$improvement <- ifelse(r20.p$improvement < -100 | r20.p$improvement > 100, NA, r20.p$improvement)
r20.p <- r20.p[!is.na(r20.p$improvement),]
r20.p.mean1 <- paste0(round(mean(r20.p$improvement, digits = 1), 1),'%')

r20.p$improvement <- ifelse(r20.p$improvement < -20, -20, 
                            ifelse(r20.p$improvement > 20, 20, r20.p$improvement))

r20.p <- r20.p[!is.na(r20.p$improvement),]

world <- ne_countries(scale = "medium", returnclass = "sf")

p20 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r20.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours =  color_palette,
                       breaks = c(-20,-10,0,10,20),
                       labels = c("-20","-10","0","10","20"),
                       limits = c(-20,20))+
  labs(fill = expression("Relative change of NH"[3]*" emission")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("ZT/RT") +
  theme(plot.title = element_text(size = 12))+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r20.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#
library(ggpubr)
p_fig.5<-ggarrange(p10,p11,p12,p13,p14,p15,p17,p19,p20, heights = c(4, 4, 4, 4), ncol = 3, nrow = 3,
                   labels = c("a","b","c","d","e","f","g","h","i"),font.label=list(size=12,color = "black", face = "bold"),
                   common.legend = TRUE, legend = "bottom", 
                   hjust = 0, vjust = 1.5)

ggsave(p_fig.5, file = ".../Figure_S_nh3.png",width = 179,height = 120, units = "mm")

