# sierra swe calc with different fsca masks: pair 1 jan 31 -- feb 11
# jack tarricone
# decemeber 5, 2022

library(terra)
library(dplyr)
library(ggplot2);theme_set(theme_classic(12))

setwd("~/ch3_fusion/rasters/")

## bring in inc
unw <-rast("./new_uavsar/p1_80m/p1_14d_VV_unw_80m.tif")
cor <-rast("./new_uavsar/p1_80m/p1_14d_VV_coh_80m.tif")
inc <-rast("./new_uavsar/inc_80m.tif")

# bring in fsca data
ims <-rast("./new_optical/p1_80m_20200131_20200212/ims_0212_80m.tif")
modscag <-rast("./new_optical/p1_80m_20200131_20200212/modscag_0212_80m.tif")
modis <-rast("./new_optical/p1_80m_20200131_20200212/modis_0212_80m.tif")
viirs <-rast("./new_optical/p1_80m_20200131_20200212/viirs_0212_80m.tif")
flm <-rast("./new_optical/p1_80m_20200131_20200212/flm_0212_80m.tif")
landsat <-rast("./new_optical/p1_80m_20200131_20200212/landsat_fsca_80m_20200201.tif")

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

# radar wave length from uavsar annotation file (cm)
uavsar_wL <- 23.8403545

# import depth_from_phase function
devtools::source_url("https://raw.githubusercontent.com/jacktarricone/snowex_uavsar/master/insar_swe_functions.R")

# from translated from uavsar_pytools function
# depth_from_phase <-function(delta_phase, inc_angle, perm, wavelength = 0.238403545){
#   
#   delta_z = (-delta_phase * wavelength) / (4 * pi * (cos(inc_angle) - sqrt(perm - sin(inc_angle)^2)))
#   
# }

# take mean pit values from cues and panorama
snowex <-read.csv("/Users/jtarrico/ch3_fusion/snowex_insitu/SNEX20_TS_SP_Summary_SWE_v02.csv")

# convert to date_time
snowex$date_time <-ymd_hm(snowex$date_time)
snowex$date <-as_date(snowex$date_time)
mam <-filter(snowex, location == "Mammoth Lakes")
mam
mam_p1 <-mam %>% dplyr::filter(date > "2020-01-30" & date < "2020-02-13")
mam_p1

# calculate mean density between the four pits and two dates
density <-mean(mam_p1$density_mean)
print(density)

# calc perm using guni equation
sierra_perm <- 1 + 1.6 * (density/1000) + 1.8 * (density/1000)^3

############### 
## unw_stack ##
###############

depth_change <-depth_from_phase(delta_phase = unw_stack,
                                inc_angle = inc,
                                perm = sierra_perm,
                                wavelength = uavsar_wL)

# convert to SWE change
dswe_raw <-depth_change*(density/1000)
plot(dswe_raw)
hist(dswe_raw[[1]])

####### bring in snow pillow data
# pull out location info into separate df
pillow_locations <-read.csv("~/ch3_fusion/csvs/cadwr_pillows_meta_uavsar_v1.csv", header = TRUE)

# plot pillow location using terra vector functionality
pillow_point <-vect(pillow_locations, geom = c("lon","lat"), crs = crs(unw_modis)) #needs to be 
plot(dswe_raw[[1]])
points(pillow_point, cex = 1)
text(pillow_point, labels = c("VLC", "DPO", "MHP","UBC","WWC"), pos = 3)

# calculate SWE change at pillow
cadwr_swe <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csv")
cadwr_swe$date <-as.Date(cadwr_swe$date)

# test plot from vlc cadwr pillow
ggplot(cadwr_swe, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# study period filter
sp <-dplyr::filter(cadwr_swe, date > "2020-01-30" & date < "2020-02-13")

ggplot(sp, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# calc change in SWE at pillow from jan 31 - feb 12
station_dswe <- sp %>%
  group_by(id) %>%
  summarize(dswe_cm = swe_cm[13] - swe_cm[1])

station_dswe

# extract using that vector
pillow_cell_dswe <-terra::extract(dswe_raw, pillow_point,  cells = TRUE, xy = TRUE, ID = TRUE)
pillow_cell_dswe$id <-c("VLC", "DPO", "MHP","UBC","WWC")
pillow_cell_dswe

# extract 8 surronding cells
test_cells <-adjacent(dswe_raw, pillow_cell_dswe$cell, direction = 8)

# for five stations
vlc_cells <-c(pillow_cell_dswe$cell[1],test_cells[1,])
dpo_cells <-c(pillow_cell_dswe$cell[2],test_cells[2,])
mhp_cells <-c(pillow_cell_dswe$cell[3],test_cells[3,])
ubc_cells <-c(pillow_cell_dswe$cell[4],test_cells[4,])
# wwc_cells <-c(pillow_cell_dswe$cell[5],test_cells[5,])

# extract
vlc_vals <-terra::extract(dswe_raw, vlc_cells)
colnames(vlc_vals) <-rep("vlc", ncol(vlc_vals))
dpo_vals <-terra::extract(dswe_raw, dpo_cells)
colnames(dpo_vals) <-rep("dpo", ncol(dpo_vals))
mhp_vals <-terra::extract(dswe_raw, mhp_cells)
colnames(mhp_vals) <-rep("mph", ncol(mhp_vals))
ubc_vals <-terra::extract(dswe_raw, ubc_cells)
colnames(ubc_vals) <-rep("ubc", ncol(ubc_vals))
# wwc_vals <-terra::extract(dswe_raw, wwc_cells)
# colnames(wwc_vals) <-"wwc"

# make df
vlc_mean <-mean(colMeans(vlc_vals, na.rm = TRUE), na.rm = TRUE)
dpo_mean <-mean(colMeans(dpo_vals, na.rm = TRUE), na.rm = TRUE)
mhp_mean <-mean(colMeans(mhp_vals, na.rm = TRUE), na.rm = TRUE)
ubc_mean <-mean(colMeans(ubc_vals, na.rm = TRUE), na.rm = TRUE)

# mean station dswe
mean_pillow_dswe <-mean(station_dswe$dswe_cm)
mean_pillow_dswe

# mean them all, pretty much the same
mean_insar_dswe <-mean(c(vlc_mean,dpo_mean,mhp_mean,ubc_mean),na.rm = TRUE)
mean_insar_dswe

# create tether value
tether_value <- mean_pillow_dswe - mean_insar_dswe
tether_value
########## calc absolute dswe
# modscag
dswe <-dswe_raw + tether_value
plot(dswe)
hist(dswe, breaks = 100)

# list names
names <-c("ims","modscag","modis","viirs","flm","landsat")

# save with looop
for (i in 1:length(names)) {
  
  dataset <-names[i]
  writeRaster(dswe[[i]], paste0("~/ch3_fusion/rasters/new_dswe/p1/p1_",dataset,"_dswe_cm_v4.tif"))
  
}
