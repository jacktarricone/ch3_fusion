# sierra swe calc with different fsca masks
# jack tarricone
# decemeber 5, 2022

library(terra)

setwd("./ch3_fusion/rasters/")
list.files()

# bring in sierra shape
sierra <-vect("/Users/jacktarricone/ch3_fusion/uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
plot(sierra)

### bring nisar sim data from feb 22 - march 5th
# coherence
nisar_cor <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
nisar_cor
plot(nisar_cor)
plot(sierra, add = TRUE)
 
###### feb 26 - march 11 UAVSAR data
## cor
cor <-rast("./uavsar/sierra_17305_20014-000_20016-005_0014d_s01_L090_01_int_grd/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd.tiff")
cor
plot(cor)

## unw
unw <-rast("./uavsar/sierra_17305_20014-000_20016-005_0014d_s01_L090_01_int_grd/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.unw.grd.tiff")
unw
plot(unw)

## inc
inc <-rast("./uavsar/inc/sierra_17305_20016_005_200311_L090_CX_01.inc.tiff")
inc
plot(inc)

#####
## optical data
#####

# modscag march 4th
modscag <-rast("./modscag/modscag_reproj_20200304.tif")
values(modscag)[values(modscag) < 15] = NA
modscag
plot(modscag)
plot(sierra, add = TRUE)

# modis march 4th
modis <-rast("./MOD10A1F_wy2020/fsca/modis_fsca_20200304.tif")
values(modis[[1]])[values(modis[[1]]) < 15] = NA
modis
plot(modis[[1]])
plot(sierra, add = TRUE)

# viirs march 4th
viirs <-rast("./VNP10A1F_wy2020/sierra_fsca/viirs_fsca_20200304.tif")
values(viirs[[3]])[values(viirs[[3]]) < 15] = NA
viirs
plot(viirs[[3]])
plot(sierra, add = TRUE)

# landsat march 4th
landsat <-rast("./for_Q/landsat_20200304_resamp.tif")
plot(landsat)
plot(sierra, add = TRUE)

# flm march 4th
flm <-rast("./for_Q/flm_20200304_resamp.tif")
flm
plot(flm)
plot(sierra, add = TRUE)

##########
## mask and crop all data for USJ
##########

# nisar sim
nisar_cor_sierra_v1 <-mask(nisar_cor, sierra)
nisar_cor_sierra <-crop(nisar_cor_sierra_v1 , ext(sierra))
plot(nisar_cor_sierra)

# modscag
modscag_sierra_v1 <-mask(modscag, sierra)
modscag_sierra <-crop(modscag_sierra_v1 , ext(sierra))
plot(modscag_sierra)

# modis
modis_sierra_v1 <-mask(modis, sierra)
modis_sierra <-crop(modis_sierra_v1 , ext(sierra))
plot(modis_sierra[[1]])

# viirs
viirs_sierra_v1 <-mask(viirs, sierra)
viirs_sierra <-crop(viirs_sierra_v1 , ext(sierra))
plot(viirs_sierra[[3]])

# landsat
landsat_sierra_v1 <-mask(landsat, sierra)
landsat_sierra <-crop(landsat_sierra_v1 , ext(sierra))
plot(landsat_sierra)

# flm
flm_sierra_v1 <-mask(flm, sierra)
flm_sierra <-crop(flm_sierra_v1 , ext(sierra))
plot(flm_sierra)

#######
## resample to 80m nisar res
#######

## uavsar
# unw
unw_resamp_v1 <-resample(unw, nisar_cor_sierra)
unw_80m <-crop(unw_resamp_v1, ext(sierra))
plot(unw_80m)
# writeRaster(unw_80m, "./uavsar/feb26_march11_80m/unw_80m.tif")

# inc
inc_resamp_v1 <-resample(inc, nisar_cor_sierra)
inc_80m <-crop(inc_resamp_v1, ext(sierra))
plot(inc_80m)
# writeRaster(inc_80m, "./uavsar/feb26_march11_80m/inc_80m.tif")

# cor
cor_resamp_v1 <-resample(cor, nisar_cor_sierra)
cor_80m <-crop(cor_resamp_v1, ext(sierra))
plot(cor_80m)
# writeRaster(cor_80m, "./uavsar/feb26_march11_80m/cor_80m.tif")

## optical
# modscag
modscag_resamp_v1 <-resample(modscag_sierra, nisar_cor_sierra)
modscag_sierra_80m <-crop(modscag_resamp_v1, ext(sierra))
plot(modscag_sierra_80m)
modscag_sierra_80m
# writeRaster(modscag_sierra_80m, "./uavsar/feb26_march11_80m/modscag_sierra_80m.tif")

# modis
modis_resamp_v1 <-resample(modis_sierra, nisar_cor_sierra)
modis_sierra_80m <-crop(modis_resamp_v1, ext(sierra))
plot(modis_sierra_80m[[1]])
modis_sierra_80m
# writeRaster(modis_sierra_80m[[1]], "./uavsar/feb26_march11_80m/modis_sierra_80m.tif")

# viirs
viirs_resamp_v1 <-resample(viirs_sierra, nisar_cor_sierra)
viirs_sierra_80m <-crop(viirs_resamp_v1, ext(sierra))
plot(viirs_sierra_80m[[3]])
viirs_sierra_80m
# writeRaster(viirs_sierra_80m[[3]], "./uavsar/feb26_march11_80m/viirs_sierra_80m.tif")

# landsat
landsat_resamp_v1 <-resample(landsat_sierra, nisar_cor_sierra)
landsat_sierra_80m <-crop(landsat_resamp_v1, ext(sierra))
plot(landsat_sierra_80m)
landsat_sierra_80m
# writeRaster(landsat_sierra_80m, "./uavsar/feb26_march11_80m/landsat_sierra_80m.tif")

# flm
flm_resamp_v1 <-resample(flm_sierra, nisar_cor_sierra)
flm_sierra_80m <-crop(flm_resamp_v1, ext(sierra))
plot(flm_sierra_80m)
flm_sierra_80m
# writeRaster(flm_sierra_80m, "./uavsar/feb26_march11_80m/flm_sierra_80m.tif")


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



