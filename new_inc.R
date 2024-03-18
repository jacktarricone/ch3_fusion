library(terra)

setwd("~/ch3_fusion/rasters/cop_dem_30m/")

cop30 <-rast('./output_COP30_small.tif')
plot(cop30)

up <-rast('./multi_seg.up.gc.tif')
plot(up, add = T)

# resamp and mask
cop_re <-mask(resample(cop30, up, method = 'bilinear'),up)
cop_re
plot(cop_re)
# writeRaster(cop_re, "./cop_resamp.tif")

#
new_inc_6m <-rast("./new_inc_6m.tif")
new_inc_6m
plot(new_inc_6m)

# geolocate
ext(new_inc_6m) <-ext(up)
new_inc_6m
plot(new_inc_6m)

# old inc
inc <-rast("~/ch3_fusion/rasters/new_uavsar/inc_80m.tif")
plot(inc)
new_resamp <-resample(new_inc_6m, inc, method = 'bilinear')
new_resamp
plot(new_resamp)
writeRaster(new_resamp, "~/ch3_fusion/rasters/new_uavsar/inc_cop_80m_v2.tif")
new_resamp
