# modscag data formatting script for ally
# feb 22, 2023

######### helpful links
### general modis snow info
# https://modis-snow-ice.gsfc.nasa.gov/?c=MOD10A1F

### modis snow products user guide
# https://modis-snow-ice.gsfc.nasa.gov/uploads/snow_user_guide_C6.1_final_revised_april.pdf

### cloud and gapped filled mosdis NDSI data
# https://nsidc.org/data/mod10a1f/versions/61

### info on coordinate reference systems in r
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf


# load in packages, all we need is terra
# package info here - https://rspatial.org/pkg/index.html
library(terra)

# set working directory to where all rasters are stored
setwd("/Users/jacktarricone/ch3_fusion/rasters/")

# list the subfolders in the wd
list.dirs(recursive = FALSE)

# list raw modscag files
list <-list.files("./modscag/raw", # "." means use relative path from working directory
                  full.names = TRUE) # full names grabs full path not just file name

# print first few rows of list
# all files are .tifs
head(list)

### test read in one random raster from the list
## define path
path_to_raster <-list[662]
path_to_raster # inspect

# read in raster
modscag_raw <-rast(path_to_raster)
modscag_raw # inspect
plot(modscag_raw)

# there is no coordinate reference system (CRS)
# but we know the data is same project as tradtional modis data


# bring in modis fsca data using rast() function
# if you want to learn more about a fucntion do this
?terra::rast

modis_raw <-rast("./MOD10A1F_wy2020/raw/MOD10A1F.A2019274.h08v05.061.2020312180452.hdf")
modis_raw
plot(modis_raw[[1]])

# modis data comes in 2400 x 2400 tiles
# check dimensions of both modis and modscag
dim(modscag_raw)
dim(modis_raw)

# modscag is the whole WUS, while the modis data we have is just one tile
# set cropping extent down to lower left tile
# using dimensions of modis raw which is just one tile
# using the ext() or extent function, set cropping extent
?terra::ext

modis_lower_left_tile <-ext(0, ncol(modis_raw), 0, nrow(modis_raw))

# crop modscag data down to the modis extent
?terra::crop

modscag_crop <-crop(modscag_raw, modis_lower_left_tile)
plot(modscag_crop)
plot(modis_raw[[5]])

# not the data is the correct number of rows and columns
# but still doesn't have a crs
modscag_crop

# use the crs information from the modis data to geolocate modscag
# set crs
?terra::crs

crs(modscag_crop) <-crs(modis_raw)
modscag_crop # check

# now set extent
ext(modscag_crop) <-ext(modis_raw)
modscag_crop # see how the units changed to meters and crs is in UTM

# test plots
plot(modscag_crop)    
plot(modis_raw[[1]])

# modis/modscag natively data come in a sinusoidal projection
# this distorts the things for CA and the western US
# the data need to be reprojected

# check current projection
modscag_crop
s
# the modis meta data uses notation called a PROJ sting
# this is outdated, and you should always use and EPSG code when you can
# info on EPSG codes: https://en.wikipedia.org/wiki/EPSG_Geodetic_Parameter_Dataset

# a common one to use is EPSG:4326
# this uses WGS84 ellipsoid
modscag_reproj <-project(modscag_crop, 'EPSG:4326')

modscag_reproj # inspect
modscag_crop # compare to old one
##### what changed?

# not plot sinusoidal and wgs84 to see difference
plot(modscag_reproj)
plot(modscag_crop)

# crop down to flm extent
# modscag_final <-crop(ms_reproj, ext(modis_fsca))


# save and export so the tif and be loaded into QGIS
?terra::writeRaster
# writeRaster(modscag_reproj, "./modscag/modscag_reproj_20200304.tif")
