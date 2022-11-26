getwd()
setwd("/Users/keshavkhanna/Desktop/CNNpred")


install.packages("raster")
install.packages("sp")
library(raster)
str_name = "direct_human_2013_impact (1).tif"
imported_raster = raster(str_name)
imported_raster

install.packages('gplot')
library('gplot')

gplot(imported_raster, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'white', high = 'black') +
  coord_equal()

image(imported_raster)

##When we are reading this file, it is giving us just a yellow map. Just to see if it 
##spitting out different points 

ras <- raster(str_name)
pts <- rasterToPoints(ras, spatial = TRUE, bylayer=TRUE)
pts

readTIFF(str_name)

