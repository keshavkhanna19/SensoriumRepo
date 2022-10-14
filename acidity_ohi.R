install.packages("installr")
library(installr)
updateR()

# Install the required packages
install.packages("ggplot2") 
install.packages("tidyverse")
install.packages("cli")

#To resolve the errors with the tidyverse library, we will install its 
#dependencies as well
#1
if (!require("broom")){
  install.packages("broom",dependencies = TRUE)
  library(broom)
}

if (!require("tidyverse")){
  install.packages("tidyverse",dependencies = TRUE)
  library(tidyverse)
}

install.packages("broom", type="binary")


#Install the required libraries that will help us to create maps in R
library(ggplot2)

#To install a package not available for this version of R
install.packages("ggbiplot")
install.packages("devtools")
library(devtools)



library(tidyverse)

#First we will change directory to the OHI Database folder 
setwd("/Users/keshavkhanna/Desktop/OHI_Database")


#To read both the acid and general scores csv files 
acid_data = read.csv("Acidity_OHI.csv")
general_scores = read.csv("scores.csv")
class(acid_data)
class(general_scores)

#First let's pick a year for which we can test our mapping for. Then we can combine all the given years
#Let's start by analyzing for year 2017 
acid_data_2017 = acid_data[acid_data$year==2017,]

#Now we are going to merge both the columns on id to match the ides to their respect regions and then we select all the columns from the regions 
acid_data_w_reg_names = merge(acid_data_2017,general_scores, by.x = "rgn_id", , by.y = "region_id",all.x = TRUE)[,1:4]


#Now create map-data 
mapdata = map_data("world")
#We change the the column name region to region to assure uniformity
colnames(mapdata)[5] = 'region_name'
View(mapdata)

#Join the longitudes and latitudes with their data 
mapdata = left_join(mapdata,acid_data_w_reg_names,by="region_name")
View(mapdata)

#To remove all the NA rows, we can do something like this
mapdata1 = mapdata %>%
  drop_na(c("region_name"))
View(mapdata1)

#I can't remove because they are greyed out, Is there a way to remove them alternatively 

#Now we create a way with longitude and latitude 
map1 = ggplot(mapdata1,aes(x = long,y=lat,group=group))+geom_polygon(aes(fill=region_name),color="black")
map1

#Instead of making a plot using ggplot, we will utilize the vgpm function

#an Extent object of a Raster* or Spatial* 
#object (or an Extent object), or creates an Extent object from a 2x2 matrix (first row: xmin, xmax; second row: ymin, ymax),


#create empty raster

e <- extent(c(-180,180,-90,90))
r <- raster(e,ncol=2160,nrow=1080)


#The options supplied to vgpm.raster() are as follows:
# file = file name (or substitute file.choose() to pick file interactively)
# w.lon = western longitude limit for region of interest (-180 to +180)
# e.lon = eastern longitude limit for region of interest (-180 to +180)
# n.lat = northern latitude limit for region of interest (+90 to -90)
# s.lat = southern latitude limit for region of interest (+90 to -90)

w.lon = -180
e.lon = 180
n.lat = 90
s.lat = -90

#First test the portion of this function 

vgpm.raster = function(w.lon, e.lon, n.lat, s.lat, log = TRUE, 
                       color = tim.colors(30)){
  
  #Extract date from file title
  
  print(w.lon)

}

#Also we need not to include all the parameters in our vgpm raster function 
#because we already have our file converted as a dataframe. We can utilize the 
#documentation in later stages 

vgpm.raster(w.lon, e.lon, n.lat, s.lat, log = TRUE, 
            color = tim.colors(30))




vgpm.raster = function(df, w.lon, e.lon, n.lat, s.lat, log = TRUE, 
                       color = tim.colors(30)){
  
  #Extract year from the file 
  yr    = df$year #extract year
  x = df
  
  if (nrow(x) <= 2332800) { f.size = '1080'        #I don't know what this 
  # equal file is doing, I do something like less than or equal to some specific value
  } else if (2332800 < nrow(x) && nrow(x) <= 9331200) { f.size = '2160'
  } else {
    warning('Unknown file type\n', immediate. = TRUE)
  }
  
  if (f.size == '1080') {
    lons = x$long[1:2160] #get set of longitude values
    lats = x$lat[seq(1,2332800,by = 2160)] #get latitude values
    values = matrix(x$values, nrow = 1080, ncol = 2160, byrow = TRUE)
  } else if (f.size == '2160') {
    lons = x$long[1:4320] #get set of longitude values
    lats = x$lat[seq(1,9331200,by = 4320)] #get latitude values
    values = matrix(x$pressure_score, nrow = 2160, ncol = 4320, byrow = TRUE)
  }

  
#Insert the lat/lon values as the 'values' matrix dimension names
  dimnames(values) = list(Latitude = lats, Longitude = lons)

  
  # Specify the boundaries of your lat/lon of interest. Recall that
  # longitude values run from -180E (international date line in the Pacific)
  # to +180E, where Greenwich,England is at 0E. Latitude values range from
  # +90N (north pole) to -90 (south pole). The first value for longitude must be
  # the western-most edge of your region of interest, and the first value for the
  # latitude must be the northern-most edge of the region of interest.
  lonlim = c(w.lon,e.lon) # c(western edge, eastern edge)
  latlim = c(n.lat,s.lat)	# c(northern edge, southern edge)
  
  # Create vectors of lat/lon indices
  lonindx = 1:length(lons) #make vector of longitude cell indices
  latindx = 1:length(lats) #make vector of latitude cell indices
  
  # Pull out 2 vectors that contain the indices of the lat/lon coordinates
  # of interest. We search for longitudes that are greater than the 1st value
  # in lonlim, and longitudes that are less than the 2nd value in lonlim, and
  # then grab the corresponding indices from lonindx to store in goodlons. The
  # same is done for the latitudes
  goodlons = lonindx[lons >= lonlim[1] & lons <= lonlim[2]]
  goodlats = latindx[lats >= latlim[2] & lats <= latlim[1]]
  
  # Extract a subset of the matrix 'values', call it the Region of Interest (ROI) 
  ROI = values[goodlats[1]:goodlats[length(goodlats)],
               goodlons[1]:goodlons[length(goodlons)]]
  # Add the latitudes and longitudes to the ROI matrix as dimension names
  dimnames(ROI) = list(Latitude = lats[goodlats], Longitude = lons[goodlons])
  n.lats = as.numeric(rownames(ROI))
  n.lons = as.numeric(colnames(ROI))
  
  # Generate a new set of lats and longs on a regular grid spacing for plot.
  if (f.size == '1080') {
    lats2 = seq(n.lats[1],(n.lats[length(n.lats)]-0.1666667),by=-0.1666667)
    lons2 = seq(n.lons[1],(n.lons[length(n.lons)]+0.1666667),by=0.1666667)
  } else if (f.size == '2160') {
    lats2 = seq(n.lats[1],(n.lats[length(n.lats)]-0.0833333),by=-0.0833333)
    lons2 = seq(n.lons[1],(n.lons[length(n.lons)]+0.0833333),by=0.0833333)
  }
  if(length(lats2) > length(n.lats)) lats2 = lats2[1:length(n.lats)]
  if(length(lons2) > length(n.lons)) lons2 = lons2[1:length(n.lons)]
  ROI.plot = t(ROI) # swap longs and lats in 'ROI', so lats are in columns
  ROI.plot = ROI.plot[,rev(1:length(lats2))] # reverse latitudes so that 
  # southern lats are listed first
  if (log) {
    image.plot(lons2, rev(lats2), log10(ROI.plot), useRaster = TRUE, 
               col = color,
               xlab = 'Longitude', ylab = 'Latitude', 
               main = paste('Net Primary Production', strftime(day1,'%B %Y')), 
               legend.lab = expression(paste(log[10],'(mg C /', m^2,'/ day)')),
               legend.mar = 4.1)
  } else if (!log){
    image.plot(lons2, rev(lats2), ROI.plot, useRaster = TRUE, 
               col = color,
               xlab = 'Longitude', ylab = 'Latitude', 
               main = paste('Net Primary Production', strftime(day1,'%B %Y')), 
               legend.lab = expression(paste('mg C /', m^2,'/ day')),
               legend.mar = 4.3)
  }
  
  ROI # return region of interest data to workspace
  
  # From here down, added code to the function to rasterize the matrix
  
  log = log10(ROI)
  r = raster(log)
  extent(r)<-e
  projection(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  plot(r,col=color,
       xlab='Longitude',
       ylab='Latitude',
       main=paste('Net Primary Production',strftime(day1,'%B %Y')),
       legend.lab=expression(paste('mg C /', m^2,'/ day')),
       legend.mar=4.3)
  
  writeRaster(r,filename=paste0('working/rasterized_rawdata/npp',sep='_',strftime(day1,'%B %Y')),format='GTiff',overwrite=T)
  
}  # end of vgpm.raster() function

  
  

  
vgpm.raster(mapdata, w.lon, e.lon, n.lat, s.lat, log = TRUE, 
            color = tim.colors(30))
  
nrow(mapdata)