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

# create shp for 
# p1
p1_na <-as.polygons(na[[1]], trunc=TRUE, dissolve=FALSE, values=FALSE,
                       na.rm=TRUE, na.all=FALSE)

p1_na_sf <-st_as_sf(p1_na)
plot(p1_na_sf)

# p2
p2_na <-as.polygons(na[[2]], trunc=TRUE, dissolve=FALSE, values=FALSE,
                    na.rm=TRUE, na.all=FALSE)

p2_na_sf <-st_as_sf(p2_na)
plot(p2_na_sf)

# p3
p3_na <-as.polygons(na[[3]], trunc=TRUE, dissolve=FALSE, values=FALSE,
                    na.rm=TRUE, na.all=FALSE)

p3_na_sf <-st_as_sf(p3_na)
plot(p3_na_sf)

# p4
p4_na <-as.polygons(na[[4]], trunc=TRUE, dissolve=FALSE, values=FALSE,
                    na.rm=TRUE, na.all=FALSE)

p4_na_sf <-st_as_sf(p4_na)
plot(p4_na_sf)

# save
writeVector(p1_na, "./rasters/new_uavsar/p1_na.gpkg")
writeVector(p2_na, "./rasters/new_uavsar/p2_na.gpkg")
writeVector(p3_na, "./rasters/new_uavsar/p3_na.gpkg")
writeVector(p4_na, "./rasters/new_uavsar/p4_na.gpkg")
