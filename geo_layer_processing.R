## formatting data for qgis
## jack tarricone
## december 5th, 2022

library(terra)

setwd("/Users/jacktarricone/ch3_fusion/")

# bring in sierra shape
sierra <-vect("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
plot(sierra)

# usj
usj_v1 <-vect("./shapefiles/upper_san_joaquin.gpkg")
usj <-project(usj_v1, crs(sierra))
plot(usj)
plot(sierra, add = TRUE)

########## nlcd canopy cover 2016
cc_raw <-rast("./rasters/geo_layers/raw/NLCD_2016_Tree_Canopy_L48_20190831_8ILcwOA0bCSdi15EeQtJ.tiff")
cc_reproj <-project(cc_raw, crs(sierra)) # reproj
cc_usj <-mask(cc_reproj, usj) # mask usj
cc_sierra <-mask(cc_reproj, sierra) # mask sierra
cc_both <-merge(cc_usj, cc_sierra) # merge together

# make extent from both
both_ext <-ext(-119.735641599587, -118.654386843973, 36.9833382269695, 37.8592799112127)
cc <-crop(cc_both, both_ext) # crop

# test plot, looks good
plot(cc)
plot(usj, add = TRUE)
plot(sierra, add = TRUE)

# save
writeRaster(cc, "./rasters/geo_layers/cc_domain.tif")
