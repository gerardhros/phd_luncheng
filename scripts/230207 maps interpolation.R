# require packages
require(terra)
require(data.table)
require(gstat)
require(sf)

# read in Qiyang data
d1 <- as.data.table(readxl::read_xlsx('../01 data/quzhou_data.xlsx',skip = 1))

# select relevant columns
d1 <- d1[,.(x=lons,y=lats,nin = `total N input (kg/ha)`,nue = `NUE %`,id = Site_ID)]

# convert data.table to spatial point object
s1 <- st_as_sf(d1[,.(x,y,nin,nue)],coords = c('x','y'),crs = 4326)

# select a raster to predict on
p <- terra::rast('../01 data/Quzhou_cropland.tif')

# convert the raster file to CRS 4326 (= WGS84). the original tiff had a strange crs.
p <- terra::project(p,'epsg:4326')

# aggregate a factor 3.33 to speed up the proces (and 100x100m seems sufficient)
# this is optional of course, if you do not, then following process is slower)
p <- terra::aggregate(p, fact = 100/30)

# interpolation: option 1, using IDW

  # make an interpolation function
  mg <- gstat::gstat(id = 'nin', formula = nin~1, locations = ~x+y, data = d1, nmax = 7, set=list(idp=.5))

  # interpolate the data unto raster p
  z <- terra::interpolate(p, mg, debug.level=0, index=1)

  # mask the predicitons to p
  z <- mask(z,p)

# interpolation: option 2, using sf object

  # make an interpolation function for an sf object
  mgsf <- gstat::gstat(id = "nin", formula = nin~1, data=s1,  nmax=7, set=list(idp = .5))

  # extra function is needed to support interpolation
  interpolate_gstat <- function(model, x, crs, ...) {
    v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
    p <- predict(model, v, ...)
    as.data.frame(p)[,1:2]
  }

  # interpolate the data unto raster p and mask
  zsf <- interpolate(p, mgsf, debug.level=0, fun=interpolate_gstat, crs=crs(p), index=1)
  zsf <- mask(zsf, p)

# interpolation: option 3, using ordinairy kriging

  # make a variogram
  v <- gstat::variogram(log(nin)~1, ~x+y, data = d1)
  mv <- gstat::fit.variogram(v, gstat::vgm(1,"Sph",range = 0.2,nugget = 0.3))

  # check variogram
  plot(v,model=mv)

  # make kriging model
  gOK <- gstat::gstat(NULL, "log.nin", log(nin)~1, d1, locations=~x+y, model=mv)

  # interpolate the data unto raster p and mask
  OK <- interpolate(p, gOK, debug.level=0)
  OK <- mask(OK, p)

# make a list of variables and add all interpolated maps to a raster stack

  # what are the columns to make maps for
  cols <- c('nin','nue')

  # output stack
  out <- p

  # make a for loop for each of the variables
  for(i in 1:length(cols)){

    # add and update a new predictor column
    d2 <- d1[, varpred := get(cols[i])]

    # make an interpolation function
    mg <- gstat::gstat(id = 'varpred', formula = varpred~1, locations = ~x+y, data = d2, nmax = 7, set=list(idp=.5))

    # interpolate the data unto raster p
    z <- terra::interpolate(p, mg, debug.level=0, index=1)

    # mask the predicitons to p
    z <- mask(z,p)

    # update the name
    names(z) <- cols[i]

    # add predicted map to the output stack
    out <- c(out,z)

    # print number
    print(paste0('the variable ',cols[i],' is succesfully added'))
  }

  # write all output maps to disk as raster
  terra::writeRaster(out,'products/maps_interpolated.tif', overwrite = TRUE)

# a simple example to make a nice plot JPG

  # load spatial polygon
  s2 <- st_read('../01 data/Quzhou_cropland.shp')
  s2 <- st_transform(s2,4326)

  # convert to plotted raster to data.frame (in this case for nin)
  r1.p <- as.data.frame(out$nin,xy=TRUE)

  # make a plot for Quzhou with continuous color legend
  plot1 <- ggplot(data = s2) + geom_sf(color = "black", fill = "gray92") +
            geom_tile(data = r1.p,aes(x=x,y=y,fill= nin)) +
            scale_fill_viridis_c()+ theme_bw()+
            theme(legend.position = 'bottom') +
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("Quzhou", subtitle = "total N input") +
            coord_sf(crs = 4326)

  # save the plot (note you have to adjust widht and height manually up to the figure is nice)
  ggsave(plot = plot1, filename = 'products/testfigure1.jpg',
         width = 25, height = 25, units = c("cm"), dpi = 1200)


  # here an helper function to plot the figure with predefined classes
  visualize <- function(raster, layer, name, breaks, labels, ftitle){

    # select the correct layer
    raster.int <- raster[layer]

    # define crs
    plotcrs <- coord_sf(crs = 4326, lims_method = "box")

    #raster to xy
    df <- as.data.frame(raster.int, xy = TRUE)

    #colnames
    colnames(df) <- c("x", "y", "variable")

    #plot
    ggplot() +
      geom_tile(data = df, aes(x = x, y = y,fill = cut(variable, breaks,labels = labels))) +
      plotcrs +
      scale_fill_viridis_d(direction=-1) +
      xlab("") + ylab("")+ theme_bw() +
      #xlab("Longitude") + ylab("Latitude") +
      labs(fill = name) +
      theme(text = element_text(size = 10),
            legend.text=element_text(size=8),
            legend.position = c(0.85,0.75),
            legend.background = element_rect(fill = "white",color='white'),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(ftitle)
  }

  # you can run this one as follows for figure nin
  plot1 <- visualize(raster = out,
                     layer = 'nin',
                     name = "total N input\n(kg N/ha)",
                     breaks = c(-100,100,200,400,800,2000),
                     labels = c('<100','100-200','200-400','400-800','>800'),
                     ftitle = 'total N input in Quzhou')
  ggsave(plot = plot1, filename = 'products/testfigure1.jpg')
