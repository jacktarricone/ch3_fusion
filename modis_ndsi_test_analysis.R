# nsidc-download_MOD10A1.061_2022-11-08.py test

library(terra)

setwd("/Users/jacktarricone/ch3_fusion")
data_list <-list.files("./MOD10A1_wy2020", pattern = "\\.hdf$", full.names = TRUE)
head(data_list)

# read in modis tile in sinusodal projection from april 3rd 2020
april_3 <-data_list[185]
modis_sin <-rast(april_3)

# test plot NDSI layer
plot(modis_sin[[1]])

# reproject to geogrpahic coords
ndsi <-project(modis_sin, 'EPSG:4326')
ndsi
plot(ndsi[[1]])

# read in karls fused data for croping
flm_raw <-rast("./fused_landsat_modis/SSN.downscaled.20200403.v4.3e+05.tif")
values(flm_raw)[values(flm_raw) == 0] <- NA
flm <-project(flm_raw, 'EPSG:4326')
flm

# crop modis ndsi down 
ndsi_crop <-crop(ndsi, ext(flm))
values(ndsi_crop[[1]])[values(ndsi_crop[[1]]) > 100] <- NA
values(ndsi_crop[[1]])[values(ndsi_crop[[1]]) == 0] <- NA
plot(flm, col = viridis::viridis(100, option = "viridis"))
plot(ndsi_crop[[1]], col = viridis::viridis(20, option = "plasma"), add = TRUE)

?viridis
 