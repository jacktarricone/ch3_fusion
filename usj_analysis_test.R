## workshoping analysis in the upper san joaquin
# november 16, 2020
# jack tarricone

library(terra)

setwd("/Users/jacktarricone")

##### read in rasters
## landsat_fsca

# path to h3_v8 folder
folders_list <-list.files("./ch3_fusion/rasters/landsat_fsca/h3_v9_2020", full.names = TRUE) 
tifs <-list.files(folders_list,  pattern = "GROUND_SNOW.TIF", full.names = TRUE) 

# stack
stack <-rast(tifs)
stack
plot(stack[[6]])

# bring in other tile so it covers full basin
ten_tile <-rast("/Users/jacktarricone/ch3_fusion/rasters/landsat_fsca/h3_v10_2020/LC08_CU_003010_20200201_20210504_02_SNOW/LC08_CU_003010_20200201_20210504_02_GROUND_SNOW.TIF")

# merge
full <-merge(stack[[6]], ten_tile)
plot(full)

# reproj
feb_1 <-project(full, "EPSG:4326", method = "bilinear")
values(feb_1)[values(feb_1) == 0] <- NA
plot(feb_1)
feb_1

# read in ard shp
landsat_ard_raw <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/conus_c2_ard_grid.shp")
landsat_ard <-project(landsat_ard_raw, crs(feb_1))
plot(landsat_ard, add = TRUE)

# read in usj shp
usj_raw <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/upper_san_joaquin.gpkg")
usj <-project(usj_raw, crs(feb_1))
plot(usj, add = TRUE)

# crop and mask
fsca_usj_v1 <-crop(feb_1, ext(usj))
fsca_usj_v2 <-mask(fsca_usj_v1, usj)
plot(fsca_usj_v2)

# reading in feb 1 modis_fsca
modis_fsca <-rast("/Users/jacktarricone/ch3_fusion/rasters/for_Q/modis_fsca_feb1_1_2020.tif")
modis_fsca
plot(modis_fsca)

# crop and mask
modis_fsca_v1 <-crop(modis_fsca, ext(usj))
modis_fsca_v2 <-mask(modis_fsca_v1, usj)
plot(modis_fsca_v2)

# reading in feb 1 modis_fsca
flm_fsca_raw <-rast("/Users/jacktarricone/ch3_fusion/rasters/flm/SSN.downscaled.20200201.v4.3e+05.tif")
values(flm_fsca_raw)[values(flm_fsca_raw) == 0] <- NA
flm_fsca <-project(flm_fsca_raw, crs(feb_1))
flm_fsca
plot(flm_fsca)

# crop and mask
flm_fsca_v1 <-crop(flm_fsca, ext(usj))
flm_fsca_v2 <-mask(flm_fsca_v1, usj)

plot(flm_fsca_v2)
plot(modis_fsca_v2)
plot(fsca_usj_v2)
