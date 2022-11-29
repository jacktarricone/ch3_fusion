# modscag data formatting test
# jack tarricone
# november 23, 2022

library(terra)

setwd("/Users/jacktarricone/ch3_fusion/rasters/")

# list modscag files
list <-list.files("./modscag_test", full.names = TRUE)

# rast
modscag_raw <-rast(list[2])
modscag_raw
plot(modscag_raw)

# bring in modis fsca
modis_fsca <-rast("./MOD10A1F_wy2020/fsca/modis_fsca_20200403.tif")

# bring in modis raw
modis_raw <-rast("./MOD10A1F_wy2020/raw/MOD10A1F.A2019274.h08v05.061.2020312180452.hdf")
modis_raw
plot(modis_raw[[1]])

# set cropping extent down to same tile
# using dimension sof modis raw which is just one tile
modis_lower_left_tile <- ext(0, 2400, 0, 2400)

# crop down
ms_c <- crop(modscag_raw, modis_lower_left_tile)
plot(ms_c)

# geolocate
crs(ms_c) <-crs(modis_raw)
ext(ms_c) <-ext(modis_raw)
ms_c

# test plot
plot(ms_c)    
plot(modis_raw[[1]])

# project
ms_reproj <-project(ms_c, 'EPSG:4326')

# crop down to flm extent
modscag_final <-crop(ms_reproj, ext(modis_fsca))
plot(modscag_final)
writeRaster(modscag_final, "./modscag_test/modscag_reproj_test_v2.tif")
