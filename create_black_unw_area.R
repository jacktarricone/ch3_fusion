# create black area rast for SWE plot
# feb 7th 2024

library(terra)

setwd("~/ch3_fusion")

# read in new shape
sierra <-vect("./shapefiles/sierra_multiseg_shp_v4.gpkg")
plot(sierra)

# rasters
p1 <-rast("./rasters/new_uavsar/p1_80m/p1_14d_VV_unw_80m.tif")
p2 <-rast("./rasters/new_uavsar/p2_80m/p2_7d_VV_unw_80m.tif")
p3 <-rast("./rasters/new_uavsar/p3_80m/p3_7d_VV_unw_80m.tif")
p4 <-rast("./rasters/new_uavsar/p4_80m/p4_14d_VV_unw_80m.tif")

# stack
stack <-c(p1,p2,p3,p4)

# test
na1 <-ifel(is.na(stack),-999,stack)
na2 <-ifel(na1 > -999, NA, na1)
na <-mask(na2, sierra)
plot(na)

# save
writeRaster(na, "./rasters/new_uavsar/na_pixels_stack.tif")
