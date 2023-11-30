# create multisegment shape file

library(terra)

setwd('~/ch3_fusion/rasters')

# swe
cor <-rast('./new_uavsar/p2/p2_7d_VV_coh_v2.tif')
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

# conver to vect
as_vect <-as.polygons(cor_80m)

## aggregate polyongs up to just data extent
cor_shp <- aggregate(as_vect, dissolve = TRUE, fun = "mean", cores = 12)
plot(cor_shp)
writeVector(cor_shp, "~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
