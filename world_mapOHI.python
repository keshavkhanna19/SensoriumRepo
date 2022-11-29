# Import the os module
import os


cwd = os.chdir('/Users/keshavkhanna/Desktop')
print(cwd)


# Print the current working directory
print("Current working directory: {0}".format(cwd))


###All this code needs to be done if we have rasters with 3 bands 
#Import the modules and open the files
from osgeo import gdal
import matplotlib.pyplot as plt
import numpy as np
  
  
dataset = gdal.Open(r'direct_human_2013_impact 2.tif')
print(dataset.RasterCount)

# since there are 3 bands
# we store in 3 different variables
band1 = dataset.GetRasterBand(1) # Red channel
band2 = dataset.GetRasterBand(2) # Green channel
band3 = dataset.GetRasterBand(3) # Blue channel

b1 = band1.ReadAsArray()
b2 = band2.ReadAsArray()
b3 = band3.ReadAsArray()



img = np.dstack((b1, b2, b3))
f = plt.figure()
plt.imshow(img)
plt.savefig('Tiff.png')
plt.show()




#For raster with a single band we do the following
import rasterio 
import matplotlib.pyplot as plt
src = rasterio.open('direct_human_2013_impact 2.tif')
plt.imshow(src.read(1), cmap='pink')
plt.show()
plt.savefig('map.png')



# Without vmin and vmax, matplotlib uses nodata value as minimum value, and stretches the color map.
import rasterio as rs
from matplotlib import pyplot

library(raster)
r = raster('direct_human_2013_impact 2.tif')

file = 'direct_human_2013_impact 2.tif'
raster = rs.open(file)
array = raster.read()
plt.imshow(array[0], vmin=0, vmax=array[0].max())
plt.show()

#But it still gives low quality images 




#Another strategy that we can apply is studying the raster data completely 
import rasterio

dataset = rasterio.open('direct_human_2013_impact 2.tif')
print(dataset.name)
print(dataset.mode)
print(dataset.count)
print(dataset.width)
print(dataset.height)
print(dataset.bounds)
print(dataset.profile)
print(dataset.transform)


#Now we handle the bands
for i in range(len(dataset.indexes) ):
    print("{}: {}".format(i, dataset.dtypes[i]))

band1 = dataset.read(1)
print(band1)

#The code at the bottom doesnt work because we just have 1 band 
band2 = dataset.read(2)
print(band2)

#This is the default plotting scheme that we used earlier which we can used to comapre 
#higher resolution plots 
plt.imshow(band1)
plt.tight_layout()



#2nd method that we have is Raster show() method
from rasterio.plot import show
show(dataset)
plt.tight_layout()
