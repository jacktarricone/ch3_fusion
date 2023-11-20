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


# make extent from both
both_ext <-ext(-119.735641599587, -118.654386843973, 36.9833382269695, 37.8592799112127)
fh <-crop(fh_both, both_ext) # crop

# test plot, looks good
plot(fh)
plot(usj, add = TRUE)
plot(sierra, add = TRUE)

# save
# writeRaster(fh, "./rasters/geo_layers/fh_domain.tif")

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


