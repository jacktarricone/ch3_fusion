# sierra swe calc with different fsca masks: pair 4, feb 26 -- march 11
# jack tarricone
# decemeber 5, 2022

library(terra)
library(dplyr)
library(ggplot2);theme_set(theme_classic(12))

setwd("~/ch3_fusion/rasters/")

## bring in inc
unw <-rast("./new_uavsar/p4_80m/p4_7d_VV_unw_80m.tif")
cor <-rast("./new_uavsar/p4_80m/p4_7d_VV_coh_80m.tif")
inc <-rast("./new_uavsar/inc_80m.tif")

# bring masked unwrapped phase rastesr
ims <-rast("./new_optical/p4_80m_20200226_20200311/ims_0311_80m.tif")
modscag <-rast("./new_optical/p4_80m_20200226_20200311/modscag_0311_80m.tif")
modis <-rast("./new_optical/p4_80m_20200226_20200311/modis_0311_80m.tif")
viirs <-rast("./new_optical/p4_80m_20200226_20200311/viirs_0311_80m.tif")
flm <-rast("./new_optical/p4_80m_20200226_20200311/flm_0311_80m.tif")
landsat <-rast("./new_optical/p4_80m_20200226_20200311/landsat_fsca_80m_20200304.tif")

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

# approx density from mammoth pits
density <-390 # check this

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
cadwr_swe <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csvs")
cadwr_swe$date <-as.Date(cadwr_swe$date)

# test plot from vlc cadwr pillow
ggplot(cadwr_swe, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# study period filter
sp <-dplyr::filter(cadwr_swe, date > "2020-02-25" & date < "2020-03-12")

ggplot(sp, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# calc change in SWE at pillow from feb 26 - march 11
station_dswe <- sp %>%
  group_by(id) %>%
  summarize(dswe_cm = swe_cm[8] - swe_cm[1])

station_dswe

# extract using that vector
pillow_cell_dswe <-terra::extract(dswe_raw[[1]], pillow_point,  cells = TRUE, xy = TRUE, ID = TRUE)
pillow_cell_dswe$id <-c("VLC", "DPO", "MHP","UBC","WWC")
pillow_cell_dswe

# extract 8 surronding cells
test_cells <-adjacent(dswe_raw, pillow_cell_dswe$cell, direction = 8)

# for five stations
vlc_cells <-c(pillow_cell_dswe$cell[1],test_cells[1,])
# dpo_cells <-c(pillow_cell_dswe$cell[2],test_cells[2,])
mhp_cells <-c(pillow_cell_dswe$cell[3],test_cells[3,])
ubc_cells <-c(pillow_cell_dswe$cell[4],test_cells[4,])
# wwc_cells <-c(pillow_cell_dswe$cell[5],test_cells[5,])

# extract
vlc_vals <-terra::extract(dswe_raw, vlc_cells)
colnames(vlc_vals) <-"vlc"
# dpo_vals <-terra::extract(modscag_dswe_raw, dpo_cells)
# colnames(dpo_vals) <-"dpo"
mhp_vals <-terra::extract(dswe_raw, mhp_cells)
colnames(mhp_vals) <-"mhp"
ubc_vals <-terra::extract(dswe_raw, ubc_cells)
colnames(ubc_vals) <-"ubc"
# wwc_vals <-terra::extract(dswe_raw, wwc_cells)
# colnames(wwc_vals) <-"wwc"

# make df
vals_df <-cbind(vlc_vals, mhp_vals, ubc_vals)

# bind and find average swe change
bind_v2 <-left_join(pillow_cell_dswe, station_dswe)
bind_v2

# mean them all, pretty much the same
mean_insar_dswe <-mean(c(vals_df$vlc, vals_df$mhp, vals_df$ubc, vals_df$wwc), na.rm = TRUE)

# calc mean
mean_pillow_dswe <-mean(bind_v2$dswe_cm)
# mean_insar_dswe <-mean(bind_v2$'sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.unw.grd')

# create tether value
tether_value <- mean_pillow_dswe - mean_insar_dswe

########## calc absolute dswe
# modscag
dswe <-dswe_raw + tether_value
plot(dswe)
hist(dswe, breaks = 100)

# save
writeRaster(dswe[[1]], "./new_dswe/p4/p4_ims_dswe_cm_v2.tif")
writeRaster(dswe[[2]], "./new_dswe/p4/p4_modscag_dswe_cm_v2.tif")
writeRaster(dswe[[3]], "./new_dswe/p4/p4_modis_dswe_cm_v2.tif")
writeRaster(dswe[[4]], "./new_dswe/p4/p4_viirs_dswe_cm_v2.tif")
writeRaster(dswe[[5]], "./new_dswe/p4/p4_flm_dswe_cm_v2.tif")
writeRaster(dswe[[6]], "./new_dswe/p4/p4_landsat_dswe_cm_v2.tif")

