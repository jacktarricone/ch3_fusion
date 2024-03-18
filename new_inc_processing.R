library(terra)

setwd("~/ch3_fusion/rasters/cop_dem_30m/")

cop30 <-rast('./output_COP30_small.tif')
plot(cop30)

up <-rast('./multi_seg.up.gc.tif')*0.3048
plot(up, add = T)

# resamp and mask
cop_re <-mask(resample(cop30, up, method = 'bilinear'),up)
cop_re
plot(cop_re)
writeRaster(cop_re, "./cop_resamp.tif")
