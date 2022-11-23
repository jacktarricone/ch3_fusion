# test resampling for march 5th, 2020
# jack tarricone
# november 23, 2022

library(terra)

setwd("./ch3_fusion/rasters/")
list.files()

# usj
usj <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/upper_san_joaquin.gpkg")
plot(usj)

# nisar sim
cor <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
cor
plot(cor)

unw <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-unwrappedPhase.tif")
unw
plot(unw)
plot(usj, add = TRUE)

# modis
modis <-rast("./MOD10A1F_wy2020/fsca/modis_fsca_20200305.tif")
modis
plot(modis[[1]])
plot(usj, add = TRUE)

# viirs
viirs <-rast("./VNP10A1F_wy2020/fsca/viirs_fsca_20200305.tif")
viirs
plot(viirs[[3]])
plot(usj, add = TRUE)

# landsat
# bring in other tile so it covers full basin
landsat_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200304_20210504_02_SNOW/LC08_CU_003010_20200304_20210504_02_GROUND_SNOW.TIF")
landsat_9 <-rast("./landsat_fsca/h3_v9_2020/LC08_CU_003009_20200304_20210504_02_SNOW/LC08_CU_003009_20200304_20210504_02_GROUND_SNOW.TIF")

# merge and reproject
landsat_r <-merge(landsat_10, landsat_9)
plot(landsat_r)

# reproj
landsat <-project(landsat_r, "EPSG:4326", method = "bilinear")
landsat
plot(landsat)
plot(usj, add = TRUE)

# flm
flm_r <-rast("./flm/SSN.downscaled.20200305.v4.3e+05.tif")
flm <-project(flm_r, "EPSG:4326", method = "bilinear")
flm
plot(flm)
plot(usj, add = TRUE)


#### crop/mask all
# cor
cor_c <-crop(cor, ext(usj))
cor_mc <-mask(cor_c, usj)
plot(cor_mc)

# unw
unw_c <-crop(unw, ext(usj))
unw_mc <-mask(unw_c, usj)
plot(unw_mc)

# flm
flm_c <-crop(flm, ext(usj))
flm_mc <-mask(flm_c, usj)
plot(flm_mc)

# viirs
viirs_c <-crop(viirs, ext(usj))
viirs_mc <-mask(viirs_c, usj)
plot(viirs_mc[[3]])

# modis
modis_c <-crop(modis, ext(usj))
modis_mc <-mask(modis_c, usj)
plot(modis_mc[[1]])

# landsat
landsat_c <-crop(landsat, ext(usj))
landsat_mc <-mask(landsat_c, usj)
plot(landsat_mc[[1]])



