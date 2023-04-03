# Script to plot Figure 1

# load in the packages
library(readxl); library(ggplot2); library(data.table)

# read in the data needed for figure 1
site <- readxl::read_xlsx('articles/ncoms23/Database.xlsx',sheet = "Figure1")
site <- as.data.table(site)

# update management categories
site[grepl('OF|CF|RFR|RFT|RFP|EE|BC',management), management := 'Nutrient management']
site[grepl('ROT|CC|RES',management), management := 'Crop management']
site[grepl('RT|NT',management),management := 'Soil Management']

# rename
setnames(site,'management','Managements')

# convert Management to a factor in a specific order
site[,Managements := factor(Managements,
                            levels = c('Nutrient management','Crop management','Soil management'))]

# load in the base map
world <- map_data("world")

# make the plot
p1 <- ggplot() +
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

# save the plot in directory 'products'
ggsave(plot = p1, file = "products/location_map.png",width = 410,height = 197, units = "mm")
