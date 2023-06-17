# process rasters for sierra swe calc feb 26 - march 11
# jack tarricone
# decemeber 5, 2022

library(terra)

setwd("~/ch3_fusion/rasters")
list.files()

# bring in sierra shape
sierra <-vect("~/ch3_fusion/uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
plot(sierra)

### bring nisar sim data from feb 22 - march 5th
# coherence
nisar_cor_raw <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
nisar_cor <-project(nisar_cor_raw, crs(sierra))
nisar_cor
plot(nisar_cor)
plot(sierra, add = TRUE)
 
###### feb 26 - march 11 UAVSAR VV data
## cor
cor <-rast("./uavsar/sierra_17305_20014-000_20016-005_0014d_s01_L090_01_int_grd/sierra_17305_20014-000_20016-005_0014d_s01_L090VV_01.cor.grd.tiff")
cor
plot(cor)

## unw
unw <-rast("./uavsar/sierra_17305_20014-000_20016-005_0014d_s01_L090_01_int_grd/sierra_17305_20014-000_20016-005_0014d_s01_L090VV_01.unw.grd.tiff")
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

# modscag
unw_modscag <-mask(unw_80m, modscag_sierra_80m, maskvalue = NA)
plot(unw_modscag)
# writeRaster(unw_modscag, "./uavsar/feb26_march11_80m/unw_modscag_mask.tif")

# modis
unw_modis <-mask(unw_80m, modis_sierra_80m, maskvalue = NA)
plot(unw_modis[[1]])
# writeRaster(unw_modis[[1]], "./uavsar/feb26_march11_80m/unw_modis_mask.tif")

# viirs
unw_viirs <-mask(unw_80m, viirs_sierra_80m, maskvalue = NA)
plot(unw_viirs[[3]])
# writeRaster(unw_viirs[[3]], "./uavsar/feb26_march11_80m/unw_viirs_mask.tif")

# landsat
unw_landsat <-mask(unw_80m, landsat_sierra_80m, maskvalue = NA)
plot(unw_landsat)
# writeRaster(unw_landsat, "./uavsar/feb26_march11_80m/unw_landsat_mask.tif")

# flm
unw_flm <-mask(unw_80m, flm_sierra_80m, maskvalue = NA)
plot(unw_flm)
# writeRaster(unw_flm, "./uavsar/feb26_march11_80m/unw_flm_mask.tif")

