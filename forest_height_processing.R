## formatting data for qgis
## jack tarricone
## december 5th, 2022

library(terra)

setwd("~/ch3_fusion/")

# bring in sierra shape
sierra <-vect("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
plot(sierra)

# usj
usj_v1 <-vect("./shapefiles/upper_san_joaquin.gpkg")
usj <-project(usj_v1, crs(sierra))
plot(usj)
plot(sierra, add = TRUE)

########## nlcd canopy cover 2016
fh_raw <-rast("./rasters/geo_layers/raw/Forest_height_2019_NAM.tif")
fh_sierra1 <-crop(fh_raw,ext(sierra)) # mask usj
fh_sierra <-mask(fh_sierra1,sierra)
plot(fh_sierra)
writeRaster(fh_sierra, "./rasters/geo_layers/fh_sierra_30m.tif")

# resample to 80m
dat <-rast("./rasters/uavsar/data_80m_0226_0311/cor_80m.tif")
fh_80m <-resample(fh_sierra, dat, method = 'bilinear')
plot(fh_80m)
writeRaster(fh_80m, "./rasters/geo_layers/fh_sierra_80m.tif")


