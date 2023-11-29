# apply low pass filter to all insar data

library(terra)

setwd('~/sierra_isce_multi/')

### read in stacks
# p1
p1_list <-list.files("./insar_geocoded/p1", pattern = "\\.tif$", full.names = T)
p1_coh_unw <-rast(p1_list[c(1,3)])
p1_conncomp <-rast(p1_list[2])

# p2
p2_list <-list.files("./insar_geocoded/p2", pattern = "\\.tif$", full.names = T)
p2_coh_unw <-rast(p2_list[c(1,3)])
p2_conncomp <-rast(p2_list[2])

# p3
p3_list <-list.files("./insar_geocoded/p3", pattern = "\\.tif$", full.names = T)
p3_coh_unw <-rast(p3_list[c(1,3)])
p3_conncomp <-rast(p3_list[2])

# p4
p4_list <-list.files("./insar_geocoded/p4", pattern = "\\.tif$", full.names = T)
p4_coh_unw <-rast(p4_list[c(1,3)])
p4_conncomp <-rast(p4_list[2])

# apply 3x3 low pass filter to coherence and unw data
p1_lp <-focal(p1_coh_unw, w=3, fun="mean")
p2_lp <-focal(p2_coh_unw, w=3, fun="mean")
p3_lp <-focal(p3_coh_unw, w=3, fun="mean")
p4_lp <-focal(p4_coh_unw, w=3, fun="mean")


##### mask unw for conncomp
p1_unw_mask <-mask(p1_lp[[2]], p1_conncomp, maskvalues = 1, inverse = T)
plot(p1_unw_mask)

p2_unw_mask <-mask(p2_lp[[2]], p2_conncomp, maskvalues = 1, inverse = T)
plot(p2_unw_mask)

p3_unw_mask <-mask(p3_lp[[2]], p3_conncomp, maskvalues = 1, inverse = T)
plot(p3_unw_mask)

p4_unw_mask <-mask(p4_lp[[2]], p4_conncomp, maskvalues = 1, inverse = T)
plot(p4_unw_mask)

## save
setwd("~/ch3_fusion/rasters/new_uavsar")

# unw
writeRaster(p1_unw_mask, "./p1/p1_14d_VV_unw_v2.tif")
writeRaster(p2_unw_mask, "./p2/p2_7d_VV_unw_v2.tif")
writeRaster(p3_unw_mask, "./p3/p3_7d_VV_unw_v2.tif")
writeRaster(p4_unw_mask, "./p4/p4_7d_VV_unw_v2.tif")

# coh
writeRaster(p1_lp[[1]], "./p1/p1_14d_VV_coh_v2.tif")
writeRaster(p2_lp[[1]], "./p2/p2_7d_VV_coh_v2.tif")
writeRaster(p3_lp[[1]], "./p3/p3_7d_VV_coh_v2.tif")
writeRaster(p4_lp[[1]], "./p4/p4_7d_VV_coh_v2.tif")

# conncomp
writeRaster(p1_conncomp, "./p1/p1_14d_VV_conncomp_v2.tif")
writeRaster(p2_conncomp, "./p2/p2_7d_VV_conncomp_v2.tif")
writeRaster(p3_conncomp, "./p3/p3_7d_VV_conncomp_v2.tif")
writeRaster(p4_conncomp, "./p4/p4_7d_VV_conncomp_v2.tif")
