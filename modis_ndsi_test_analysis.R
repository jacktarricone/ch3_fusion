# nsidc-download_MOD10A1.061_2022-11-08.py test

library(terra)

setwd("/Users/jacktarricone/ch3_fusion")
list.files()

# read in modis tile in sinusodal projection
modis_sin <-rast("./MOD10A1_wy2020/MOD10A1.A2019327.h08v05.061.2020318122651.hdf")
modis_sin

# test plot NDSI layer
plot(modis_sin[[4]])

# reproject to geogrpahic coords
ndsi <-project(modis_sin[[4]], 'EPSG:4326')
ndsi
plot(ndsi)

# read in karls fused data for croping

ndsi_crop <-crop()