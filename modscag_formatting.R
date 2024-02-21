# modscag data formatting test
# jack tarricone
# november 23, 2022

library(terra)

setwd("~/ch3_fusion/rasters/")

# list modscag files
list_v1 <-list.files("./modscag/raw", pattern = "_snow_fraction_v2023.0.", full.names = TRUE)
list_v2 <-grep(list_v1, pattern='viewable', invert=TRUE, value=TRUE)
list <-grep(list_v2, pattern="20200131|20200212|20200219|20200226|20200311", value=TRUE)

# rast
modscag_raw <-rast(list)
modscag_raw
plot(modscag_raw[[1]])


# bring in modis raw
modis_raw <-rast("./MOD10A1F_wy2020/raw/MOD10A1F.A2019274.h08v05.061.2020312180452.hdf")
modis_raw
plot(modis_raw[[1]])

# set cropping extent down to same tile
# using dimensions of modis raw which is just one tile
modis_lower_left_tile <- ext(0, 2400, 0, 2400)

# crop down
ms_c <- crop(modscag_raw, modis_lower_left_tile)
plot(ms_c[[1]])
plot(modis_raw[[5]])

# geolocate
crs(ms_c) <-crs(modis_raw)
ext(ms_c) <-ext(modis_raw)
ms_c 

# project
ms_reproj <-project(ms_c, 'EPSG:4326')
plot(ms_reproj)

# bring in modis
modis_fsca <-rast("./MOD10A1F_wy2020/fsca/modis_fsca_20200301.tif")


# crop down to flm extent
modscag_final <-crop(ms_reproj, ext(modis_fsca))
plot(modscag_final)

# save
writeRaster(modscag_final[[1]], "./modscag/sierra/modscag_reproj_20200131.tif")
writeRaster(modscag_final[[2]], "./modscag/sierra/modscag_reproj_20200212.tif")
writeRaster(modscag_final[[3]], "./modscag/sierra/modscag_reproj_20200219.tif")
writeRaster(modscag_final[[4]], "./modscag/sierra/modscag_reproj_20200226.tif")
writeRaster(modscag_final[[5]], "./modscag/sierra/modscag_reproj_20200311.tif")
