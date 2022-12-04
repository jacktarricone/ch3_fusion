# test resampling for march 5th, 2020
# jack tarricone
# november 23, 2022

library(terra)
library(Metrics)

setwd("./ch3_fusion/rasters/")
list.files()

# bring in usj shape file
usj <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/upper_san_joaquin.gpkg")
plot(usj)

### bring nisar sim data from feb 22 - march 5th
cor <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
cor
plot(cor)
 

unw_nisar <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-unwrappedPhase.tif")
unw_nisar
plot(unw_nisar)
plot(usj, add = TRUE)

# modis
modis <-rast("./MOD10A1F_wy2020/fsca/modis_fsca_20200304.tif")
modis
plot(modis[[1]])
plot(usj, add = TRUE)

# viirs
viirs <-rast("./VNP10A1F_wy2020/sierra_fsca/viirs_fsca_20200304.tif")
viirs
plot(viirs[[3]])
plot(usj, add = TRUE)

# landsat
# bring in other tile so it covers full basin
landsat_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200304_20210504_02_SNOW/LC08_CU_003010_20200304_20210504_02_GROUND_SNOW.TIF")
landsat_9 <-rast("./landsat_fsca/h3_v09_2020/LC08_CU_003009_20200304_20210504_02_SNOW/LC08_CU_003009_20200304_20210504_02_GROUND_SNOW.TIF")

# merge and reproject
landsat_r <-merge(landsat_10, landsat_9)
plot(landsat_r)

# reproj
landsat <-project(landsat_r, "EPSG:4326", method = "bilinear")
landsat
plot(landsat)
plot(usj, add = TRUE)

# flm
flm_r <-rast("./flm/SSN.downscaled.20200304.v4.3e+05.tif")
flm <-project(flm_r, "EPSG:4326", method = "bilinear")
values(flm)[values(flm) < 15] = NA
flm
plot(flm)
plot(usj, add = TRUE)

# uavsar from feb 26 - march 11
unw_6m <-rast("./uavsar/UA_sierra_17305_20014-000_20016-005_0014d_s01_L090_01/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.unw.grd.tif")
values(unw_6m)[values(unw_6m) == 0] = NA
plot(unw_6m)

# test plot with all the data
plot(unw_nisar)
plot(flm, col = "blue", add = TRUE)
plot(unw_6m, col = "red", add = TRUE)
plot(usj, add = TRUE)

# rasample uavsar to NISAR resolution
uavsar_80m <-resample(unw_6m, unw_nisar, method = "bilinear")
uavsar_80m <-crop(uavsar_80m, ext(unw_6m))
plot(uavsar_80m)
# writeRaster(uavsar_80m, "./for_Q/uavsar_unw_80m.tif")

##########
## mask all data for USJ
##########

# landsat
landsat_usj <-mask(landsat, usj)
plot(landsat_usj)
#writeRaster(landsat_usj, "./for_Q/landsat_usj_20200304.tif")

# flm
flm_usj <-mask(flm, usj)
plot(flm_usj)
# writeRaster(flm_usj, "./for_Q/flm_usj_20200304.tif")

# grey background
grey_raw <-landsat_usj
values(grey_raw)[values(grey_raw) >= 0] = 1
# plot(grey_raw)
# writeRaster(grey_raw, "./for_Q/grey_usj.tif")

unw <-uavsar_80m

#### testing cropping to uavsar extent
# flm
flm_u_crop <-crop(flm, ext(unw))
plot(flm_u_crop)
#writeRaster(flm_u_crop, "./for_Q/flm_crop.tif")
plot(unw, add = TRUE)

# landsat
landsat_u_crop <-crop(landsat, ext(unw))
plot(landsat_u_crop)
# writeRaster(landsat_u_crop, "./for_Q/landsat_crop.tif")

# modis
modis_u_crop <-crop(modis, ext(unw))
plot(modis_u_crop[[1]])

# viirs
viirs_u_crop <-crop(viirs, ext(unw))
plot(viirs_u_crop[[3]])

#### resampling test
modis_resamp <-resample(modis_u_crop, unw)
plot(modis_resamp[[1]])
plot(unw,add = TRUE)

## modis masking test
# convert an pixels on 15% to NA
values(modis_resamp[[1]])[values(modis_resamp) < 15] = NA
plot(modis_resamp[[1]])

# mask for NA
unw_mask_modis <-mask(unw, modis_resamp[[1]], maskvalue = NA)
plot(unw_mask_modis)

## flm
# reample
flm_resamp <-resample(flm_u_crop, unw)
plot(flm_resamp)

# mask
unw_mask_flm <-mask(unw, flm_resamp, maskvalue = NA)
plot(unw_mask_flm)

# save
writeRaster(unw_mask_flm, "./for_Q/unw_mask_flm_80m_20200304.tif")
writeRaster(unw_mask_modis, "./for_Q/unw_mask_modis_80m_20200304.tif")

# number of non na pixels
unw_num_pix <-as.numeric(global(unw, fun="notNA"))
flm_mask_pix <-as.numeric(global(unw_mask_flm, fun="notNA"))
modis_mask_pix <-as.numeric(global(unw_mask_modis, fun="notNA"))

# how many pixels were masked?
unw_flm_diff <-unw_num_pix - flm_mask_pix
unw_modis_diff <-unw_num_pix - modis_mask_pix


?rmse
rmse_test <-rmse(unw_flm_diff,unw_modis_diff)
sqrt(mean((data$actual - data$predicted)^2))

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



