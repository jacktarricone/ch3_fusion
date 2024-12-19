# sierra swe calc with different fsca masks: pair 1 jan 31 -- feb 11
# jack tarricone
# decemeber 5, 2022

library(terra)
library(dplyr)
library(ggplot2);theme_set(theme_classic(12))

setwd("~/ch9_uavsar_wus-sr/rasters/")

## bring in inc
inc_deg <-rast("./uavsar/fraser/wy21/inc_copdem_6m.tif")
inc_rad <-inc_deg*(pi/180)
inc_rad
plot(inc_rad)

# load in insar dswe data
unw_files <-list.files("./uavsar/fraser/wy21", pattern = "unw", full.names = T)
unw_list <-lapply(unw_files, rast)

# extend ext so they are equal and can stack
unw_ext <-lapply(unw_list, function(x) extend(x,ext(-106.5, -105.3, 39.5, 40.5)))
plot(unw_ext[[7]])
unw_ext

# stack and convert to meters from mm
unw_stack <-rast(unw_ext)
unw_stack
# plot(unw_stack)
# hist(idswe_stack[[3]], breaks = 100)

# crop down to inc
unw_crop <-crop(unw_stack, ext(inc))
# marg_crop <-crop(marg_mask, ext(-106.3, -105.55, 40.25, 40.85))
unw_crop
plot(unw_crop[[7]])
plot(inc, add = T)

# resample inc to stack
inc_resamp <-resample(inc_rad, unw_crop)
plot(inc_resamp)

# import leinss swe change function
devtools::source_url("https://raw.githubusercontent.com/jacktarricone/jemez_zenodo/main/insar_swe_functions.R")

############### 
## calc dswe and convert to cm
###############

dswe_raw <-leinss_swe(phase = unw_crop, alpha = 1, inc_angle = inc_resamp)*100
hist(dswe_raw[[8]], breaks = 100)
plot(dswe_raw[[7]])

# save layers
new_pairs <-c("p1","p2","p3","p4","p5","p6","p8")

# save 500 uavsar data that's masked and cropped
for (i in 1:nlyr(dswe_raw)){
  
  pair_name <-new_pairs[i]
  writeRaster(dswe_raw[[i]], paste0("~/ch9_uavsar_wus-sr/rasters/uavsar/fraser/wy21/dswe_raw/fraser21_",pair_name,"_uavsar_6m_dswe_raw_v1.tif"))
  
}

