# make usj for 80m masked fsca stacks for plotting: modis, modscag, viirs, flm
# jack tarricone
# december 20th, 2022

library(terra)
library(lubridate)
library(ggplot2);theme_set(theme_classic(12))

setwd("/Users/jacktarricone/ch3_fusion/rasters/")
list.files()

# usj shp
usj <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/usj.shp")
plot(usj)

#### bring in full usj rast: coherence
cor_v1 <-rast("./clips/usj/cor_usj_20200305.tif")
cor <-project(cor_v1, crs('EPSG:4326'))
cor_utm <-project(cor_v1, crs('EPSG:32611'))
plot(cor_utm)
cor_utm

#####################
###### modis ########
#####################

# bring in modis stack
modis_list <-list.files("./MOD10A1F_wy2020/fsca/", pattern = '.tif', full.names = TRUE)
modis_stack <-rast(modis_list)
modis_stack

# pull out modis snow cover
sc_seq <-seq(1,1825,5)
modis_fsca <-modis_stack[[sc_seq]]
modis_fsca_crop <-crop(modis_fsca, cor)
values(modis_fsca_crop)[values(modis_fsca_crop) < 15] = NA # mask for %15 fsca
modis_fsca_crop

# resample and mask
modis_nisar_v1 <-resample(modis_fsca_crop, cor, method = "bilinear")
modis_fsca_usj_80m <-mask(modis_nisar_v1, usj)
modis_fsca_usj_80m
writeRaster(modis_fsca_usj_80m, "./fsca_usj_80m/modis_fsca_usj_80m_stack.tif")

#####################
###### viirs ########
#####################

# bring in viirs stack
viirs_list <-list.files("./VNP10A1F_wy2020/sierra_fsca", pattern = '.tif', full.names = TRUE)
viirs_stack <-rast(viirs_list)
viirs_stack

# pull out viirs snow cover
sc_seq <-seq(3,1828,5)
viirs_fsca <-viirs_stack[[sc_seq]]
viirs_fsca_crop <-crop(viirs_fsca, cor)
values(viirs_fsca_crop)[values(viirs_fsca_crop) < 15] = NA # mask for 15% fsca
viirs_fsca_crop

# resample and mask
viirs_nisar_v1 <-resample(viirs_fsca_crop, cor, method = "bilinear")
viirs_fsca_usj_80m <-mask(viirs_nisar_v1, usj)
viirs_fsca_usj_80m
writeRaster(viirs_fsca_usj_80m, "./fsca_usj_80m/viirs_fsca_usj_80m_stack.tif")

#####################
######  flm  ########
#####################

# bring in flm stack
flm_list <-list.files("/Users/jacktarricone/ch3_fusion/rasters/flm/raw", full.names = TRUE)
flm_rast <-rast(flm_list)

# loop for processing
for (i in 1:length(flm_list)){
  
  r <-flm_rast[[i]] # bring in rast
  r_c <-crop(r, ext(cor_utm)) # crop, this makes things much better
  values(r_c)[values(r_c) < 15] = NA # mask for 15% fsca
  reproj <-project(r_c, "EPSG:4326", method = "bilinear") # reproject
  usj_clip <-resample(reproj, cor, method = "bilinear") # resample to NISAR usj extent
  usj_mask <-mask(usj_clip, usj) # mask for usj
  name <-basename(flm_list[i]) # extract basename
  saving_name <-paste0("/Users/jacktarricone/ch3_fusion/rasters/flm/formatted/flm_fsca_usj_80m_",name) # create saving location
  writeRaster(usj_mask, saving_name) # save
  
}

# read in rasters, stack, save
flm_formatted <-list.files("/Users/jacktarricone/ch3_fusion/rasters/flm/formatted/", full.names = TRUE)
flm_stack <-rast(flm_formatted)
writeRaster(flm_stack, "/Users/jacktarricone/ch3_fusion/rasters/fsca_usj_80m/flm_fsca_usj_80m_stack.tif")


#####################
###### modscag ########
#####################

# # bring in modscag stack
# modscag_list <-list.files("./modscag/", pattern = '.tif', full.names = TRUE)
# modscag_stack <-rast(modscag_list)
# values(modscag_stack)[values(modscag_stack) < 15] = NA # mask for 
# modscag_stack
# 
# # resample and mask
# modscag_nisar_v1 <-resample(modscag_stack, cor, method = "bilinear")
# modscag_fsca_usj_80m <-mask(modscag_nisar_v1, usj)
# modscag_fsca_usj_80m
# plot(modscag_fsca_usj_80m[[20]])
# writeRaster(modscag_fsca_usj_80m, "./fsca_usj_80m/modscag_fsca_usj_80m_stack.tif")

