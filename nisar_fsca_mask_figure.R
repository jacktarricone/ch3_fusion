# test resampling for march 5th, 2020
# jack tarricone
# november 23, 2022

library(terra)

setwd("./ch3_fusion/rasters/")
list.files()

# bring in usj shape file
usj <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/upper_san_joaquin.gpkg")
plot(usj)

### bring nisar sim data from feb 22 - march 5th
# coherence
cor <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
cor
plot(cor)
 
# unwrapped phase
unw <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-unwrappedPhase.tif")
unw
plot(unw)
plot(usj, add = TRUE)

# amp
# amp <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-amplitude.tif")
# amp_db <- 10.0 * log10(1/amp)
# writeRaster(amp_db, "./sen1_nisar_sim/wy2020/20200305_amp_db.tif")
amp_db <-rast("./sen1_nisar_sim/wy2020/20200305_amp_db.tif")
plot(amp_db)
plot(usj, add = TRUE)

#####
## optical data
#####

# modis
modis <-rast("./modscag_test/raw/westernUS_Terra_20191001_snow_fraction_v03.tif")
values(modis[[1]])[values(modis[[1]]) < 15] = NA
modis
plot(modis[[1]])
plot(usj, add = TRUE)

# viirs
viirs <-rast("./VNP10A1F_wy2020/sierra_fsca/viirs_fsca_20200304.tif")
values(viirs[[3]])[values(viirs[[3]]) < 15] = NA
viirs
plot(viirs[[3]])
plot(usj, add = TRUE)

# landsat march 4th

# format
# # bring in other tile so it covers full basin
# landsat_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200304_20210504_02_SNOW/LC08_CU_003010_20200304_20210504_02_GROUND_SNOW.TIF")
# landsat_9 <-rast("./landsat_fsca/h3_v09_2020/LC08_CU_003009_20200304_20210504_02_SNOW/LC08_CU_003009_20200304_20210504_02_GROUND_SNOW.TIF")
# 
# # merge and reproject
# landsat_r1 <-merge(landsat_10, landsat_9)
# landsat_r <-landsat_r1 / 10 # convert units
# values(landsat_r)[values(landsat_r) < 15] = NA
# plot(landsat_r)
# 
# # reproj
# landsat <-project(landsat_r, "EPSG:4326", method = "bilinear")
# writeRaster(landsat, "./for_Q/landsat_20200304_resamp.tif")

landsat <-rast("./for_Q/landsat_20200304_resamp.tif")
plot(landsat)
plot(usj, add = TRUE)

### format for read in
# fused landsat-modis (flm) march 4th
# flm_raw <-rast("./flm/SSN.downscaled.20200304.v4.3e+05.tif")
# flm <-project(flm_raw, "EPSG:4326", method = "bilinear")
# values(flm)[values(flm) < 15] = NA
# writeRaster(flm, "./for_Q/flm_20200304_resamp.tif")

flm <-rast("./for_Q/flm_20200304_resamp.tif")
flm
plot(flm)
plot(usj, add = TRUE)

# test plot with all the data
plot(unw)
plot(flm, col = "blue", add = TRUE)
plot(modis[[1]], add = TRUE)
plot(usj, add = TRUE)

##########
## mask and crop all data for USJ
##########

##### nisar
# unw
unw_usj_v1 <-mask(unw, usj)
unw_usj <-crop(unw_usj_v1, ext(usj))
plot(unw_usj)
# writeRaster(unw_usj, "./clips/usj/unw_usj_20200305.tif")

# cor
cor_usj_v1 <-mask(cor, usj)
cor_usj <-crop(cor_usj_v1, ext(usj))
plot(cor_usj)
# writeRaster(cor_usj, "./clips/usj/cor_usj_20200305.tif")

# amp_db
amp_db_usj_v1 <-mask(amp_db, usj)
amp_db_usj <-crop(amp_db_usj_v1, ext(usj))
plot(amp_db_usj)
# writeRaster(amp_db_usj, "./clips/usj/amp_db_usj_20200305.tif")


###### optical

# modis
modis_usj_v1 <-mask(modis, usj)
modis_usj <-crop(modis_usj_v1, ext(usj))
plot(modis_usj[[1]])
# writeRaster(modis_usj, "./clips/usj/modis_usj_20200304.tif")

# viirs
viirs_usj_v1 <-mask(viirs, usj)
viirs_usj <-crop(viirs_usj_v1, ext(usj))
plot(viirs_usj[[3]])
# writeRaster(viirs_usj, "./clips/usj/viirs_usj_20200304.tif")

# landsat
landsat_usj_v1 <-mask(landsat, usj)
landsat_usj <-crop(landsat_usj_v1, ext(usj))
plot(landsat_usj)
# writeRaster(landsat_usj, "./clips/usj/landsat_usj_20200304.tif")

# flm
flm_usj_v1 <-mask(flm, usj)
flm_usj <-crop(flm_usj_v1, ext(usj))
plot(flm_usj)
#writeRaster(flm_usj, "./clips/usj/flm_usj_20200304.tif")


#######
## resample to 80m nisar res
#######

# modis
modis_resamp_v1 <-resample(modis_usj, unw)
modis_usj_80m <-crop(modis_resamp_v1, ext(usj))
plot(modis_usj_80m[[1]])
# writeRaster(modis_usj_80m[[1]], "./clips/usj/modis_usj_80m.tif")

# viirs
viirs_resamp_v1 <-resample(viirs_usj, unw)
viirs_usj_80m <-crop(viirs_resamp_v1, ext(usj))
plot(viirs_usj_80m[[3]])
# writeRaster(viirs_usj_80m[[3]], "./clips/usj/viirs_usj_80m.tif")

# landsat
landsat_resamp_v1 <-resample(landsat_usj, unw)
landsat_usj_80m <-crop(landsat_resamp_v1, ext(usj))
plot(landsat_usj_80m)
# writeRaster(landsat_usj_80m, "./clips/usj/landsat_usj_80m.tif")

# flm
flm_resamp_v1 <-resample(flm_usj, unw)
flm_usj_80m <-crop(flm_resamp_v1, ext(usj))
plot(flm_usj_80m)
# writeRaster(flm_usj_80m, "./clips/usj/flm_usj_80m.tif")

############################################################
##### mask phase data with the different fsca products #####
############################################################

# modis
unw_modis <-mask(unw_usj, modis_usj_80m[[1]], maskvalue = NA)
plot(unw_modis)
# writeRaster(unw_modis, "./clips/usj/unw_modis.tif")

# viirs
unw_viirs <-mask(unw_usj, viirs_usj_80m[[3]], maskvalue = NA)
plot(unw_viirs)
# writeRaster(unw_viirs, "./clips/usj/unw_viirs.tif")

# landsat
unw_landsat <-mask(unw_usj, landsat_usj_80m, maskvalue = NA)
plot(unw_landsat)
# writeRaster(unw_landsat, "./clips/usj/unw_landsat.tif")

# flm
unw_flm <-mask(unw_usj, flm_usj_80m, maskvalue = NA)
plot(unw_flm)
# writeRaster(unw_flm, "./clips/usj/unw_flm.tif")


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



