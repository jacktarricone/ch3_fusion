# create multisegment shape file

library(terra)

setwd('~/ch3_fusion/rasters')

# swe
cor <-rast("./new_uavsar/p1/p1_14d_VV_coh_for_shp.tif")
plot(cor)
cor

# bring 80 m data
nisar_cor_raw <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
nisar_cor <-project(nisar_cor_raw, crs(cor))
nisar_cor
plot(nisar_cor)
plot(cor, add = T)

# resamp
cor_80m <-crop(resample(cor,nisar_cor,method = 'bilinear'),ext(cor))
plot(cor_80m)
# cor_80m <-ifel(is.na(cor_80m), 0, cor_80m)

# conver to vect
as_vect <-as.polygons(cor_80m)
plot(as_vect)

## aggregate polyongs up to just data extent
cor_shp <- aggregate(as_vect, dissolve = TRUE, fun = "mean", cores = 12)
plot(cor_80m)
plot(cor_shp, add = TRUE)
writeVector(cor_shp, "~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
