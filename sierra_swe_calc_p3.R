# sierra swe calc with different fsca masks: pair 3, feb 19 -- feb 26
# jack tarricone
# decemeber 5, 2022

library(terra)
library(dplyr)
library(ggplot2);theme_set(theme_classic(12))

setwd("~/ch3_fusion/rasters/")

## bring in inc
unw <-rast("./new_uavsar/p3_80m/p3_7d_VV_unw_80m.tif")
cor <-rast("./new_uavsar/p3_80m/p3_7d_VV_coh_80m.tif")
inc <-rast("./new_uavsar/inc_80m.tif")

# bring masked unwrapped phase rastesr
ims <-rast("./new_optical/p3_80m_20200219_20200226/ims_0226_80m.tif")
modscag <-rast("./new_optical/p3_80m_20200219_20200226/modscag_0226_80m.tif")
modis <-rast("./new_optical/p3_80m_20200219_20200226/modis_0226_80m.tif")
viirs <-rast("./new_optical/p3_80m_20200219_20200226/viirs_0226_80m.tif")
flm <-rast("./new_optical/p3_80m_20200219_20200226/flm_0226_80m.tif")
landsat <-rast("./new_optical/p3_80m_20200219_20200226/landsat_fsca_80m_20200217.tif")

# stack
stack <-c(ims,modscag,modis,viirs,flm,landsat)

# mask pixels below 50% fsca
stack_50 <-ifel(stack < 50, NA, stack)
plot(stack_50)

# bring masked unwrapped phase rastesr
unw_ims <-mask(unw, stack_50[[1]], maskvalue = NA)
unw_modscag <-mask(unw, stack_50[[2]], maskvalue = NA)
unw_modis <-mask(unw, stack_50[[3]], maskvalue = NA)
unw_viirs <-mask(unw, stack_50[[4]], maskvalue = NA)
unw_flm <-mask(unw, stack_50[[5]], maskvalue = NA)
unw_landsat <-mask(unw, stack_50[[6]], maskvalue = NA)

# stack phase data
unw_stack <-c(unw_ims,unw_modscag,unw_modis,unw_viirs,unw_flm,unw_landsat)

# import leinss swe change function
devtools::source_url("https://raw.githubusercontent.com/jacktarricone/jemez_zenodo/main/insar_swe_functions.R")

# function(phase, alpha, inc_angle) {
#   
#   wavelength <- 0.238403545
#   k <- 2 * pi / wavelength
#   return(phase / (alpha * k * (1.59 + inc_angle^2.5)))
# }
# 

############### 
## calc dswe and convert to cm
###############

dswe_raw <-leinss_swe(phase = unw_stack, alpha = 1, inc_angle = inc)*100

####### bring in snow pillow data
# pull out location info into separate df
pillow_locations <-read.csv("~/ch3_fusion/csvs/cadwr_pillows_meta_uavsar_v1.csv", header = TRUE)
pillow_locations

# plot pillow location using terra vector functionality
pillow_point <-vect(pillow_locations, geom = c("lon","lat"), crs = crs(unw_modis)) #needs to be 
plot(dswe_raw[[1]])
points(pillow_point, cex = 1)
text(pillow_point, labels = c("VLC", "DPO", "MHP","UBC","CUES"), pos = 3)

# calculate SWE change at pillow
cadwr_swe1 <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csv")
cues_swe <-read.csv("~/ch3_fusion/csvs/cues_swe.csv")
cadwr_swe1$date <-mdy(cadwr_swe1$date)
cues_swe$date <-mdy(cues_swe$date)

# bind
cadwr_swe <-bind_rows(cadwr_swe1,cues_swe)
tail(cadwr_swe)

# test plot from vlc cadwr pillow
ggplot(cadwr_swe, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# study period filter
sp <-dplyr::filter(cadwr_swe, date > "2020-02-18" & date < "2020-02-27")

# test plot from vlc cadwr pillow
ggplot(sp, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# define insar pair length
length <-nrow(filter(sp, id == "CUES"))

# calc change in SWE at pillow from feb 12 -- 19
station_dswe <- sp %>%
  group_by(id) %>%
  summarize(dswe_cm = swe_cm[length] - swe_cm[1])

station_dswe

# extract using that vector
pillow_cell_dswe <-terra::extract(dswe_raw, pillow_point,  cells = TRUE, xy = TRUE, ID = TRUE)
pillow_cell_dswe$ID <-c(pillow_point$code)
pillow_cell_dswe

# extract 8 surronding cells
test_cells <-adjacent(dswe_raw, pillow_cell_dswe$cell, direction = 8)

# for five stations
vlc_cells <-c(pillow_cell_dswe$cell[1],test_cells[1,])
dpo_cells <-c(pillow_cell_dswe$cell[2],test_cells[2,])
mhp_cells <-c(pillow_cell_dswe$cell[3],test_cells[3,])
ubc_cells <-c(pillow_cell_dswe$cell[4],test_cells[4,])
cues_cells <-c(pillow_cell_dswe$cell[5],test_cells[5,])

# extract
vlc_vals <-terra::extract(dswe_raw, vlc_cells)
colnames(vlc_vals) <-rep("vlc", ncol(vlc_vals))
dpo_vals <-terra::extract(dswe_raw, dpo_cells)
colnames(dpo_vals) <-rep("dpo", ncol(dpo_vals))
mhp_vals <-terra::extract(dswe_raw, mhp_cells)
colnames(mhp_vals) <-rep("mph", ncol(mhp_vals))
ubc_vals <-terra::extract(dswe_raw, ubc_cells)
colnames(ubc_vals) <-rep("ubc", ncol(ubc_vals))
cues_vals <-terra::extract(dswe_raw, cues_cells)
colnames(cues_vals) <-rep("cues", ncol(cues_vals))

# make df
vlc_mean <-mean(colMeans(vlc_vals, na.rm = TRUE), na.rm = TRUE)
dpo_mean <-mean(colMeans(dpo_vals, na.rm = TRUE), na.rm = TRUE)
mhp_mean <-mean(colMeans(mhp_vals, na.rm = TRUE), na.rm = TRUE)
ubc_mean <-mean(colMeans(ubc_vals, na.rm = TRUE), na.rm = TRUE)
cues_mean <-mean(colMeans(cues_vals, na.rm = TRUE), na.rm = TRUE)

# mean station dswe
mean_pillow_dswe <-mean(station_dswe$dswe_cm)
mean_pillow_dswe

# mean them all, pretty much the same
mean_insar_dswe <-mean(c(vlc_mean,dpo_mean,mhp_mean,ubc_mean,cues_mean),na.rm = TRUE)
mean_insar_dswe

# create tether value
tether_value <- mean_pillow_dswe - mean_insar_dswe
tether_value

########## calc absolute dswe
dswe <-dswe_raw + tether_value
plot(dswe)
hist(dswe, breaks = 100)

# list names
names <-c("ims","modscag","modis","viirs","flm","landsat")

# save with looop
for (i in 1:length(names)) {
  
  dataset <-names[i]
  writeRaster(dswe[[i]], paste0("~/ch3_fusion/rasters/new_dswe/p3/p3_",dataset,"_dswe_cm_v8.tif"))
  
}


