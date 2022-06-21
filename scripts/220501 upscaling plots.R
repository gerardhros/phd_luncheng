# plotting

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)

# set theme
theme_set(theme_bw())

# get the raster to plot
r1 <- terra::rast('products/scenario_1.tif')

# convert to data.frame
r1.p <- as.data.frame(r1,xy=TRUE)

# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p1 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_tile(data = r1.p,aes(x=x,y=y,fill= improvement)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean change for scenario 1") +
    coord_sf(crs = 4326)
ggsave(plot = p1, filename = 'products/nue_effect_s1.jpg')


# plot source data to understand spatial patterns

  # get the raster to plot crops
  r1 <- terra::rast('data/ma_crops.tif')
  r1.p <- as.data.frame(r1,xy=TRUE)
  r1.p$total = (r1.p$RICE + r1.p$MAIZ + r1.p$other + r1.p$wheat)/1000
  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
        geom_raster(data = r1.p,aes(x=x,y=y,fill= total)) +
        scale_fill_viridis_c()+ theme_void() +
        theme(legend.position = 'bottom')

  # get the raster to plot MAT
  r1 <- terra::rast('data/climate.tif')
  r1.p <- as.data.frame(r1,xy=TRUE)
  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
        geom_raster(data = r1.p,aes(x=x,y=y,fill= mat)) +
        scale_fill_viridis_c()+ theme_void() +
        theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean Anntual Temperature") +
    coord_sf(crs = 4326)
  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_raster(data = r1.p,aes(x=x,y=y,fill= pre*0.1)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean Annual Precipitation") +
    coord_sf(crs = 4326)

  # get the raster to plot soil properties
  r1 <- terra::rast('data/soil.tif')
  r1.p <- as.data.frame(r1,xy=TRUE)
  colnames(r1.p) <- c('x','y','ph','clay','soc')
  r1.p <- r1.p[r1.p$ph>0,]

  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_raster(data = r1.p,aes(x=x,y=y,fill= ph)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean pH isric") +
    coord_sf(crs = 4326)
  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_raster(data = r1.p,aes(x=x,y=y,fill= soc)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean SOC isric") +
    coord_sf(crs = 4326)

  # get the raster to plot tillage properties TEST
  # SOETHING GOES WRONG HERE
  # SEE DIFFERENCE BETWEEN DEFAULT PLOT AND THE GGPLOT

  r1 <- terra::rast('data/tillage.tif')
  r1.p <- as.data.frame(r1,xy=TRUE,na.rm=F)
  r1.p <- as.data.table(r1.p)
  r1.p <- r1.p[!(is.na(RICE) & is.na(MAIZ) & is.na(other) & is.na(wheat))]
  r1.p[,total := pmax(0,RICE,na.rm=T) + pmax(MAIZ,0,na.rm=T) +
         pmax(other,0,na.rm=T) + pmax(wheat,0,na.rm=T)]
  ggplot(data = world) + geom_sf(color = "black", fill = "gray98") +
    geom_tile(data = r1.p,aes(x=x,y=y,fill= total)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "All tillage practices") +
    coord_sf(crs = 4326)

