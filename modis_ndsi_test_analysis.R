# working with modis ndsi and karl's fused product
# jack tarricone
# november 8th, 2022

# data downloaded using this script
# nsidc-download_MOD10A1.061_2022-11-08.py test


library(terra)

setwd("/Users/jacktarricone/ch3_fusion")
data_list <-list.files("./rasters/MOD10A1_wy2020", pattern = "\\.hdf$", full.names = TRUE)
head(data_list)

# read in modis tile in sinusodal projection from april 3rd 2020
april_3 <-data_list[185]
modis_sin <-rast(april_3)

# test plot NDSI layer
plot(modis_sin[[1]])

# reproject to geograpahic coords
ndsi <-project(modis_sin, 'EPSG:4326')
ndsi
plot(ndsi[[1]])

# read in karls fused data for croping
flm_raw <-rast("./rasters/fused_landsat_modis/SSN.downscaled.20200403.v4.3e+05.tif")
values(flm_raw)[values(flm_raw) == 0] <- NA
flm <-project(flm_raw, 'EPSG:4326')
flm
plot(flm)

# crop modis ndsi down 
ndsi_crop <-crop(ndsi, ext(flm))

# set values above 100, which are data flags, to NA, and 0
values(ndsi_crop[[1]])[values(ndsi_crop[[1]]) > 100] <- NA
values(ndsi_crop[[1]])[values(ndsi_crop[[1]]) == 0] <- NA

# plot both
plot(flm, col = viridis::viridis(100, option = "plasma"))
plot(ndsi_crop[[1]], col = viridis::viridis(100, option = "plasma"))

plot(ndsi_crop[[1]], col = viridis::viridis(20, option = "plasma"))
plot(flm, col = viridis::viridis(100, option = "viridis"), add = TRUE)

# save rasters for plotting in Q
# writeRaster(flm, "./rasters/for_Q/flm_april_3_2020.tif")
# writeRaster(ndsi_crop[[1]], "./rasters/for_Q/modis_ndsi_april_3_2020.tif")


### load in insar data
# setwd
setwd('./sen1_nisar_sim/jan29-feb4_2020/')
list.files()

# list insar data
insar_files <-list.files("./insar", full.names = TRUE, pattern = '.tif')
insar_stack <-rast(insar_files) # rasterize
insar_stack <-project(insar_stack, 'EPSG:4326') # project

# inspect both stacks
insar_stack

# plot
grey_ramp <-RColorBrewer::brewer.pal(9, "Greys") # set color ramp
plot(insar_stack[[2]], col = grey_ramp)
plot(ndsi_crop[[1]], col = viridis::viridis(20, option = "plasma"), add = TRUE)
plot(flm, col = viridis::viridis(100, option = "viridis"), add = TRUE)

 