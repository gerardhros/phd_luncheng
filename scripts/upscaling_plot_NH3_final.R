# plotting

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(cowplot)
library(vcd)
library(RColorBrewer)

color_palette <- colorRampPalette(brewer.pal(6, "Paired"))(21)
color_palette <- rev(color_palette)

######################################## scenario_S10 (EE) ##########################################################
# set theme
theme_set(theme_bw())

# get the raster to plot
r10 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_ee_1.tif')

# convert to data.frame
r10.p <- as.data.frame(r10,xy=TRUE)
# Exclude outliers
r10.p <- r10.p[r10.p$improvement >-70,]
r10.p <- r10.p[r10.p$improvement < 70,]
r10.p.mean1 <- paste0(round(mean(r10.p$improvement)),'%')

r10.p.mean <- mean(r10.p$improvement)
r10.p.sd <- sd(r10.p$improvement)
r10.p <- r10.p[r10.p$improvement > r10.p.mean-3*r10.p.sd,]
r10.p <- r10.p[r10.p$improvement < r10.p.mean+3*r10.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

#plot a basic world map plot

p10 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r10.p, aes(x = x, y = y, name = 'none',fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("EE") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r10.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

########################################### scenario_S11 (CF) ########################################################

# set theme
theme_set(theme_bw())

# get the raster to plot
r11 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_cf_2.tif')

# convert to data.frame
r11.p <- as.data.frame(r11,xy=TRUE)
# Exclude outliers
r11.p <- r11.p[r11.p$improvement >-70,]
r11.p <- r11.p[r11.p$improvement < 70,]
r11.p.mean1 <- paste0(round(mean(r11.p$improvement)),'%')


r11.p.mean <- mean(r11.p$improvement)
r11.p.sd <- sd(r11.p$improvement)
r11.p <- r11.p[r11.p$improvement > r11.p.mean-3*r11.p.sd,]
r11.p <- r11.p[r11.p$improvement < r11.p.mean+3*r11.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p11 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r11.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("CF") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r11.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

########################################### scenario_S12 (OF) ########################################################

# set theme
theme_set(theme_bw())

# get the raster to plot
r12 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_of_3.tif')

# convert to data.frame
r12.p <- as.data.frame(r12,xy=TRUE)
# Exclude outliers
r12.p <- r12.p[r12.p$improvement >-70,]
r12.p <- r12.p[r12.p$improvement < 70,]
r12.p.mean1 <- paste0(round(mean(r12.p$improvement)),'%')

r12.p.mean <- mean(r12.p$improvement)
r12.p.sd <- sd(r12.p$improvement)
r12.p <- r12.p[r12.p$improvement > r12.p.mean-3*r12.p.sd,]
r12.p <- r12.p[r12.p$improvement < r12.p.mean+3*r12.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p12 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r12.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("OF") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r12.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

########################################### scenario_S13 (RFR) ########################################################

# set theme
theme_set(theme_bw())

# get the raster to plot
r13 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rfr_4.tif')

# convert to data.frame
r13.p <- as.data.frame(r13,xy=TRUE)
# Exclude outliers
r13.p <- r13.p[r13.p$improvement >-70,]
r13.p <- r13.p[r13.p$improvement < 70,]
r13.p.mean1 <- paste0(round(mean(r13.p$improvement)),'%')

r13.p.mean <- mean(r13.p$improvement)
r13.p.sd <- sd(r13.p$improvement)
r13.p <- r13.p[r13.p$improvement > r13.p.mean-3*r13.p.sd,]
r13.p <- r13.p[r13.p$improvement < r13.p.mean+3*r13.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p13 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r13.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFR") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r13.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

########################################### scenario_S14 (RFT) ########################################################

# set theme
theme_set(theme_bw())

# get the raster to plot
r14 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rft_5.tif')

# convert to data.frame
r14.p <- as.data.frame(r14,xy=TRUE)
# Exclude outliers
r14.p <- r14.p[r14.p$improvement >-70,]
r14.p <- r14.p[r14.p$improvement < 70,]
r14.p.mean1 <- paste0(round(mean(r14.p$improvement)),'%')

r14.p.mean <- mean(r14.p$improvement)
r14.p.sd <- sd(r14.p$improvement)
r14.p <- r14.p[r14.p$improvement > r14.p.mean-3*r14.p.sd,]
r14.p <- r14.p[r14.p$improvement < r14.p.mean+3*r14.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p14 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r14.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFT") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r14.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

########################################### scenario_S15 (RFP) ########################################################

# set theme
theme_set(theme_bw())

# get the raster to plot
r15 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rfp_6.tif')

# convert to data.frame
r15.p <- as.data.frame(r15,xy=TRUE)
# Exclude outliers
r15.p <- r15.p[r15.p$improvement >-70,]
r15.p <- r15.p[r15.p$improvement < 70,]
r15.p.mean1 <- paste0(round(mean(r15.p$improvement)),'%')

r15.p.mean <- mean(r15.p$improvement)
r15.p.sd <- sd(r15.p$improvement)
r15.p <- r15.p[r15.p$improvement > r15.p.mean-3*r15.p.sd,]
r15.p <- r15.p[r15.p$improvement < r15.p.mean+3*r15.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p15 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r15.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("RFP") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r15.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

# ########################################### scenario_S16 (BC) ########################################################
# 
# # set theme
# theme_set(theme_bw())
# 
# # get the raster to plot
# r16 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_bc_7.tif')
# 
# # convert to data.frame
# r16.p <- as.data.frame(r16,xy=TRUE)
# # Exclude outliers
# r16.p <- r16.p[r16.p$improvement >-70,]
# r16.p <- r16.p[r16.p$improvement < 70,]
# r16.p.mean1 <- paste0(round(mean(r16.p$improvement)),'%')
# 
# r16.p.mean <- mean(r16.p$improvement)
# r16.p.sd <- sd(r16.p$improvement)
# r16.p <- r16.p[r16.p$improvement > r16.p.mean-3*r16.p.sd,]
# r16.p <- r16.p[r16.p$improvement < r16.p.mean+3*r16.p.sd,]
# 
# # get base world map
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# # plot a basic world map plot
# p16 <- ggplot(data = world) +
#   geom_sf(color = "black", fill = "gray92") +
#   geom_tile(data = r16.p, aes(x = x, y = y, name = 'none',
#                               fill = improvement)) +
#   theme_void() +
#   theme(legend.position = 'bottom', text = element_text(size = 12),
#         legend.background = element_rect(fill = NA,color = NA),
#         panel.border = element_blank()) +
#   scale_fill_gradientn(colours = color_palette,
#                        breaks = c(-50,-25,0,25,50),
#                        labels = c("-50","-25","0","25","50"),
#                        limits = c(-70,70))+
#   labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("BC") +
#   theme(plot.title = element_text(size = 12))+ 
#   theme(plot.title = element_text(hjust = 0.5))+
#   annotate("text",x=25,y=-50,label= paste("Mean:",r16.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
#   coord_sf(crs = 4326)


# ########################################### scenario_S17 (RES) ########################################################
# 
# # set theme
# theme_set(theme_bw())
# 
# # get the raster to plot
# r17 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_res_8.tif')
# 
# # convert to data.frame
# r17.p <- as.data.frame(r17,xy=TRUE)
# # Exclude outliers
# r17.p <- r17.p[r17.p$improvement >-70,]
# r17.p <- r17.p[r17.p$improvement < 70,]
# r17.p.mean1 <- paste0(round(mean(r17.p$improvement)),'%')
# 
# r17.p.mean <- mean(r17.p$improvement)
# r17.p.sd <- sd(r17.p$improvement)
# r17.p <- r17.p[r17.p$improvement > r17.p.mean-3*r17.p.sd,]
# r17.p <- r17.p[r17.p$improvement < r17.p.mean+3*r17.p.sd,]
# 
# # get base world map
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# # plot a basic world map plot
# p17 <- ggplot(data = world) +
#   geom_sf(color = "black", fill = "gray92") +
#   geom_tile(data = r17.p, aes(x = x, y = y, name = 'none',
#                               fill = improvement)) +
#   theme_void() +
#   theme(legend.position = 'bottom', text = element_text(size = 12),
#         legend.background = element_rect(fill = NA,color = NA),
#         panel.border = element_blank()) +
#   scale_fill_gradientn(colours = color_palette,
#                        breaks = c(-50,-25,0,25,50),
#                        labels = c("-50","-25","0","25","50"),
#                        limits = c(-70,70))+
#   labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("RES") +
#   theme(plot.title = element_text(size = 12))+
#   theme(plot.title = element_text(hjust = 0.5))+
#   annotate("text",x=25,y=-50,label= paste("Mean:",r17.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
#   coord_sf(crs = 4326)

########################################### scenario_S19 (ROT) ########################################################

# set theme
theme_set(theme_bw())

# get the raster to plot
r19 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_rot_10.tif')

# convert to data.frame
r19.p <- as.data.frame(r19,xy=TRUE)
# Exclude outliers  
r19.p <- r19.p[r19.p$improvement >-70,]
r19.p <- r19.p[r19.p$improvement <70,]
r19.p.mean1 <- paste0(round(mean(r19.p$improvement)),'%')

r19.p.mean <- mean(r19.p$improvement)
r19.p.sd <- sd(r19.p$improvement)
r19.p <- r19.p[r19.p$improvement > r19.p.mean-3*r19.p.sd,]
r19.p <- r19.p[r19.p$improvement < r19.p.mean+3*r19.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p19 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r19.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("ROT") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r19.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

########################################### scenario_SI20 (NT/RT) ########################################################

# set theme
theme_set(theme_bw())

# get the raster to plot
r20 <- terra::rast('E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/scenario_nh3_nt_11.tif')

# convert to data.frame
r20.p <- as.data.frame(r20,xy=TRUE)
# Exclude outliers less than -100%
r20.p <- r20.p[r20.p$improvement >-70,]
r20.p <- r20.p[r20.p$improvement < 70,]
r20.p.mean1 <- paste0(round(mean(r20.p$improvement)),'%')

r20.p.mean <- mean(r20.p$improvement)
r20.p.sd <- sd(r20.p$improvement)
r20.p <- r20.p[r20.p$improvement > r20.p.mean-3*r20.p.sd,]
r20.p <- r20.p[r20.p$improvement < r20.p.mean+3*r20.p.sd,]

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p20 <- ggplot(data = world) +
  geom_sf(color = "black", fill = "gray92") +
  geom_tile(data = r20.p, aes(x = x, y = y, name = 'none',
                              fill = improvement)) +
  theme_void() +
  theme(legend.position = 'bottom', text = element_text(size = 12),
        legend.background = element_rect(fill = NA,color = NA),
        panel.border = element_blank()) +
  scale_fill_gradientn(colours = color_palette,
                       breaks = c(-50,-25,0,25,50),
                       labels = c("-50","-25","0","25","50"),
                       limits = c(-70,70))+
  labs(fill = expression("Relative change of NH"[3]*" volatilization")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("ZT/RT") +
  theme(plot.title = element_text(size = 12))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=25,y=-50,label= paste("Mean:",r20.p.mean1),size=3, colour="#0070C0",fontface = "bold")+
  coord_sf(crs = 4326)

#2*2
library(ggpubr)
p_fig.5<-ggarrange(p10,p11,p12,p13,p14,p15,p19,p20, heights = c(4, 4, 4, 4), ncol = 3, nrow = 3,
                   labels = c("a","b","c","d","e","f","g","h"),font.label=list(size=12,color = "black", face = "bold"),
                   common.legend = TRUE, legend = "bottom", 
                   hjust = 0, vjust = 1.5)

ggsave(p_fig.5, file = "E:/phD/Papers/paper3/you_et_al_2023/upscaling_paper3/Figure_S_nh3.png",width = 179,height = 120, units = "mm")
