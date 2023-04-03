#Figure 1

library(tidyverse)
library(data.table)

site <- readxl::read_xlsx('articles/ncoms23/Database.xlsx',sheet = "Figure1")
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

names(site)[names(site)=="management"] <- "Managements"
site$Managements <- factor(site$Managements, levels = c('Nutrient management','Crop management','Soil management'))

world <- map_data("world")
world

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "#999999", fill = "#CCCCCC", size = 0.1) +
  geom_point(data = site,aes(lon, lat,color = Managements), alpha = 1, size = 3) +
  scale_color_manual(values = c("Nutrient management" = "indianred3","Crop management"= "seagreen3","Soil management"="royalblue3"))+
  theme_bw()+
  scale_x_continuous(breaks = c(-120, -60, 0, 60, 120), expand = c(0, 0),
                     labels = c('120°W', '60°W', '0', '60°E', '120°E')) +
  scale_y_continuous(breaks = c(-60, -30, 0, 30, 60), expand = c(0, 0),
                     labels = c('60°S', '30°S', '0', '30°N', '60°N')) +
  labs(x = 'Longitude', y = 'Latitude', color = 'Managements')+
  theme(axis.text=element_text(size=22, color = "black"),
        axis.title = element_text(size = 22, face="bold", color = "black"))+
  theme(legend.position = c(0.15,0.3))+theme(legend.text = element_text(size=22, color="black"))+theme(legend.title = element_text(face="bold", size=18, color="black"))


ggsave(file = "products/location_map.png",width = 410,height = 197, units = "mm")
