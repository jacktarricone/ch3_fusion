# sierra swe calc with different fsca masks: pair 2, feb 12 -- feb 19
# jack tarricone

library(terra)
library(dplyr)
library(lubridate)
library(ggplot2);theme_set(theme_classic(12))

setwd("~/ch3_fusion/rasters/")

## bring in inc
unw <-rast("./new_uavsar/p2_80m/p2_7d_VV_unw_80m.tif")
inc <-rast("./new_uavsar/inc_80m.tif")

# import leinss swe change function
devtools::source_url("https://raw.githubusercontent.com/jacktarricone/jemez_zenodo/main/insar_swe_functions.R")

############### 
## calc dswe and convert to cm
###############

dswe_raw <-leinss_swe(phase = unw, alpha = 1, inc_angle = inc)*100

####### bring in snow pillow data
# pull out location info into separate df
pillow_locations <-read.csv("~/ch3_fusion/csvs/cadwr_pillows_meta_uavsar_v1.csv", header = TRUE)

# plot pillow location using terra vector functionality
pillow_point <-vect(pillow_locations, geom = c("lon","lat"), crs = crs(unw)) #needs to be 

# calculate SWE change at pillow
cadwr_swe1 <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csv")
cues_swe <-read.csv("~/ch3_fusion/csvs/cues_swe.csv")
cadwr_swe1$date <-mdy(cadwr_swe1$date)
cues_swe$date <-mdy(cues_swe$date)

# study period filter
sp <-dplyr::filter(cadwr_swe, date > "2020-02-11" & date < "2020-02-20")

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
writeRaster(dswe, "~/ch3_fusion/rasters/wus_marg/pairs/p2_uavsar_dswe_80m.tif")


