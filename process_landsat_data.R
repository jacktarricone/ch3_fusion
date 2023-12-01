# formatting landsat fsca data for usj analysis
# jack tarricone
# november 29th, 2022

library(terra)
library(stringr)

setwd("~/ch3_fusion/rasters/")

# bring in coh from masking and cropping
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
coh_80m <-rast("./new_uavsar/p1_80m/p1_14d_VV_coh_80m.tif")


############################
########## feb 11 ##########
############################

# landsat
# bring in other tile so it covers full basin
l_0217_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200210_20210504_02_SNOW/LC08_CU_003010_20200210_20210504_02_GROUND_SNOW.TIF")
l_0217_09 <-rast("./landsat_fsca/h3_v09_2020/LC08_CU_003009_20200210_20210504_02_SNOW/LC08_CU_003009_20200210_20210504_02_GROUND_SNOW.TIF")

# merge and reproject
l_0217 <-merge(l_0217_10, l_0217_09)
l_0217_v1 <-l_0217/10
plot(l_0217)

# reproj
l_0217_v2 <-project(l_0217_v1, "EPSG:4326", method = "bilinear")
l_0217_v2
plot(l_0217_v1)

# crop and mask to 
l_0217_v3 <-crop(mask(l_0217_v2, sierra),ext(sierra))
plot(l_0217_v3)
writeRaster(l_0217_v3, "./new_optical/p2_80m_20200212_20200219/landsat_fsca_30m_20200217.tif")

# resample to 
l_0217_80m <-resample(l_0217_v3, coh_80m, method = 'bilinear')
plot(l_0217_80m)
writeRaster(l_0217_80m, "./new_optical/p2_80m_20200212_20200219/landsat_fsca_80m_20200217.tif")



############################
########## feb 17 ##########
############################

# landsat
# bring in other tile so it covers full basin
l_0217_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200217_20210504_02_SNOW/LC08_CU_003010_20200217_20210504_02_GROUND_SNOW.TIF")
l_0217_09 <-rast("./landsat_fsca/h3_v09_2020/LC08_CU_003009_20200217_20210504_02_SNOW/LC08_CU_003009_20200217_20210504_02_GROUND_SNOW.TIF")

# merge and reproject
l_0217 <-merge(l_0217_10, l_0217_09)
l_0217_v1 <-l_0217/10
plot(l_0217)

# reproj
l_0217_v2 <-project(l_0217_v1, "EPSG:4326", method = "bilinear")
l_0217_v2
plot(l_0217_v1)

# crop and mask to 
l_0217_v3 <-crop(mask(l_0217_v2, sierra),ext(sierra))
plot(l_0217_v3)
# writeRaster(l_0217_v3, "./new_optical/p2_80m_20200212_20200219/landsat_fsca_30m_20200217.tif")

# resample to 
l_0217_80m <-resample(l_0217_v3, coh_80m, method = 'bilinear')
plot(l_0217_80m)
# writeRaster(l_0217_80m, "./new_optical/p2_80m_20200212_20200219/landsat_fsca_80m_20200217.tif")
