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
# writeRaster(cc, "./rasters/geo_layers/cc_domain.tif")

########## nlcd canopy cover 2016
lc_raw <-rast("./rasters/geo_layers/raw/NLCD_2019_Land_Cover_L48_20210604_8ILcwOA0bCSdi15EeQtJ.tiff")
lc_reproj <-project(lc_raw, crs(sierra), method = "near") # reproj
lc_usj <-mask(lc_reproj, usj) # mask usj
lc_sierra <-mask(lc_reproj, sierra) # mask sierra
lc_both <-merge(lc_usj, lc_sierra) # merge together

# make extent from both
both_ext <-ext(-119.735641599587, -118.654386843973, 36.9833382269695, 37.8592799112127)
lc <-crop(lc_both, both_ext) # crop

# test plot, looks good
plot(lc)
plot(usj, add = TRUE)
plot(sierra, add = TRUE)

# save
# writeRaster(lc, "./rasters/geo_layers/lc_domain.tif")

## dem
dem_raw <-rast("./rasters/geo_layers/raw/output_COP30.tif")
dem_usj <-mask(dem_raw, usj) # mask usj
dem_sierra <-mask(dem_raw, sierra) # mask sierra
dem_both <-merge(dem_usj, dem_sierra) # merge together
plot(dem_both)

# make extent from both
both_ext <-ext(-119.735641599587, -118.654386843973, 36.9833382269695, 37.8592799112127)
dem <-crop(dem_both, both_ext) # crop

# test plot, looks good
plot(dem)
plot(usj, add = TRUE)
plot(sierra, add = TRUE)

# save
# writeRaster(dem, "./rasters/geo_layers/dem_domain.tif")


