# create multisegment shape file

library(terra)

setwd('~/ch3_fusion/rasters/new_uavsar')

# swe
cor <-rast('./p1/p1_14d_VV_coh_v2.tif')
plot(cor)
cor

# conver to vect
as_vect <-as.polygons(cor)

## aggregate polyongs up to just data extent
cor_shp <- aggregate(as_vect, dissolve = TRUE, fun = "mean", cores = 12)
plot(cor_shp)
writeVector(cor_shp, "./vectors/sierra_multiseg_shp.gpkg")
