# formatting landsat fsca data for usj analysis
# jack tarricone
# november 29th, 2022

library(terra)
library(stringr)

setwd("~/ch3_fusion/rasters/")

# bring in coh from masking and cropping
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
flm <-rast("./new_optical/p1_80m_20200131_20200212/flm_0212_80m.tif")

############################
########## feb 01 ##########
############################

# landsat
# bring in other tile so it covers full basin
l_0201_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200201_20210504_02_SNOW/LC08_CU_003010_20200201_20210504_02_GROUND_SNOW.TIF")
l_0201_09 <-rast("./landsat_fsca/h3_v09_2020/LC08_CU_003009_20200201_20210504_02_SNOW/LC08_CU_003009_20200201_20210504_02_GROUND_SNOW.TIF")

# merge and reproject
l_0201 <-merge(l_0201_10, l_0201_09)
l_0201_v1 <-l_0201/10
plot(l_0201)

# reproj
l_0201_v2 <-project(l_0201_v1, "EPSG:4326", method = "bilinear")
l_0201_v2
plot(l_0201_v1)

# crop and mask to 
l_0201_v3 <-crop(mask(l_0201_v2, sierra),ext(sierra))
plot(l_0201_v3)
writeRaster(l_0201_v3, "./new_optical/p1_80m_20200131_20200212/landsat_fsca_30m_20200201.tif")

# resample to 
l_0201_80m <-resample(l_0201_v3, flm, method = 'bilinear')
plot(l_0201_80m)
writeRaster(l_0201_80m, "./new_optical/p1_80m_20200131_20200212/landsat_fsca_80m_20200201.tif")



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
writeRaster(l_0217_v3, "./new_optical/p2_80m_20200212_20200219/landsat_fsca_30m_20200217.tif")

# resample to 
l_0217_80m <-resample(l_0217_v3, flm, method = 'bilinear')
plot(l_0217_80m)
writeRaster(l_0217_80m, "./new_optical/p2_80m_20200212_20200219/landsat_fsca_80m_20200217.tif")


############################
########## mar 04 ##########
############################

# landsat
# bring in other tile so it covers full basin
l_0304_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200304_20210504_02_SNOW/LC08_CU_003010_20200304_20210504_02_GROUND_SNOW.TIF")
l_0304_09 <-rast("./landsat_fsca/h3_v09_2020/LC08_CU_003009_20200304_20210504_02_SNOW/LC08_CU_003009_20200304_20210504_02_GROUND_SNOW.TIF")

# merge and reproject
l_0304 <-merge(l_0304_10, l_0304_09)
l_0304_v1 <-l_0304/10
plot(l_0304)

# reproj
l_0304_v2 <-project(l_0304_v1, "EPSG:4326", method = "bilinear")
l_0304_v2
plot(l_0304_v1)

# crop and mask to 
l_0304_v3 <-crop(mask(l_0304_v2, sierra),ext(sierra))
plot(l_0304_v3)
writeRaster(l_0304_v3, "./new_optical/p4_80m_20200226_20200311/landsat_fsca_30m_20200304.tif")

# resample to 
l_0304_80m <-resample(l_0304_v3, flm, method = 'bilinear')
plot(l_0304_80m)
writeRaster(l_0304_80m, "./new_optical/p4_80m_20200226_20200311/landsat_fsca_80m_20200304.tif")
