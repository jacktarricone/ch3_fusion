# apply low pass filter to all insar data

library(terra)

setwd('./sierra_isce_multi/')

### read in stacks
# p1
p1_list <-list.files("./insar_geocoded/p1/", pattern = "\\.tif$", full.names = T)
p1_stack <-rast(p1_list)
p1_stack

# p2
p2_list <-list.files("./insar_geocoded/p2/", pattern = "\\.tif$", full.names = T)
p2_stack <-rast(p2_list)
p2_stack

# p3
p3_list <-list.files("./insar_geocoded/p3/", pattern = "\\.tif$", full.names = T)
p3_stack <-rast(p3_list)
p3_stack

# p4
p4_list <-list.files("./insar_geocoded/p4/", pattern = "\\.tif$", full.names = T)
p4_stack <-rast(p4_list)
p4_stack

# plot(p4_stack[[1]])
# plot(p4_stack[[2]])
# plot(p4_stack[[3]])

# apply 3x3 low pass filter
p1_lp <-focal(p1_stack, w=9 ,fun="mean")
plot(p1_lp[[1]])
plot(p1_stack[[1]])
writeRaster(p1_lp[[3]], "~/ch3_fusion/rasters/new_uavsar/testing/p1_unw_filt_v2.tif")

