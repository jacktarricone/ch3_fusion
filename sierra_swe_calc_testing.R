# sierra swe calc with different fsca masks
# jack tarricone
# decemeber 5, 2022

library(terra)
library(dplyr)
library(ggplot2);theme_set(theme_classic(12))

setwd("~/ch3_fusion/rasters/uavsar/")
list.files()

## bring in inc
inc <-rast("./data_80m_0226_0311/inc_80m.tif")
unw_v1 <-rast("~/ch3_fusion/rasters/new_uavsar/p1_80m/p1_14d_VV_unw_80m.tif")
cor <-rast("./data_80m_0226_0311/cor_80m.tif")
unw <-resample(unw_v1, cor)

# bring masked unwrapped phase rastesr
ims <-rast("./data_80m_0226_0311/ims_sierra_80m.tif")
modscag <-rast("./data_80m_0226_0311/modscag_sierra_80m.tif")
modis <-rast("./data_80m_0226_0311/modis_sierra_80m.tif")
viirs <-rast("./data_80m_0226_0311/viirs_sierra_80m.tif")
flm <-rast("./data_80m_0226_0311/flm_sierra_80m.tif")
landsat <-rast("./data_80m_0226_0311/landsat_sierra_80m.tif")

# bring masked unwrapped phase rastesr
unw_ims <-mask(unw, ims, maskvalue = NA)
unw_modscag <-mask(unw, modscag, maskvalue = NA)
unw_modis <-mask(unw, modis, maskvalue = NA)
unw_viirs <-mask(unw, viirs, maskvalue = NA)
unw_flm <-mask(unw, flm, maskvalue = NA)
unw_landsat <-mask(unw, landsat, maskvalue = NA)

# radar wave length from uavsar annotation file
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
density <-370

# calc perm using guni equation
sierra_perm <- 1 + 1.6 * (density/1000) + 1.8 * (density/1000)^3

############# 
## modscag ##
#############
modscag_depth_change <-depth_from_phase(delta_phase = unw_modscag,
                                        inc_angle = inc,
                                        perm = sierra_perm,
                                        wavelength = uavsar_wL)

# convert to SWE change
modscag_dswe_raw <-modscag_depth_change*(370/1000)
plot(modscag_dswe_raw)
hist(modscag_dswe_raw , breaks = 100)


##### 
## modis
#####

modis_depth_change <-depth_from_phase(delta_phase = unw_modis,
                                        inc_angle = inc,
                                        perm = sierra_perm,
                                        wavelength = uavsar_wL)

# convert to SWE change
modis_dswe_raw <-modis_depth_change*(sierra_density/1000)
plot(modis_dswe_raw)
hist(modis_dswe_raw , breaks = 100)

##### 
## viirs
#####

viirs_depth_change <-depth_from_phase(delta_phase = unw_viirs,
                                      inc_angle = inc,
                                      perm = sierra_perm,
                                      wavelength = uavsar_wL)

# convert to SWE change
viirs_dswe_raw <-viirs_depth_change*(sierra_density/1000)
plot(viirs_dswe_raw)
hist(viirs_dswe_raw , breaks = 100)

##### 
## landsat
#####

landsat_depth_change <-depth_from_phase(delta_phase = unw_landsat,
                                      inc_angle = inc,
                                      perm = sierra_perm,
                                      wavelength = uavsar_wL)

# convert to SWE change
landsat_dswe_raw <-landsat_depth_change*(sierra_density/1000)
plot(landsat_dswe_raw)
hist(landsat_dswe_raw , breaks = 100)

##### 
## flm
#####

flm_depth_change <-depth_from_phase(delta_phase = unw_flm,
                                        inc_angle = inc,
                                        perm = sierra_perm,
                                        wavelength = uavsar_wL)

# convert to SWE change
flm_dswe_raw <-flm_depth_change*(sierra_density/1000)
plot(flm_dswe_raw)
hist(flm_dswe_raw , breaks = 100)


##### 
## ims
#####

ims_depth_change <-depth_from_phase(delta_phase = unw_ims,
                                    inc_angle = inc,
                                    perm = sierra_perm,
                                    wavelength = uavsar_wL)

# convert to SWE change
ims_dswe_raw <-ims_depth_change*(sierra_density/1000)
plot(ims_dswe_raw)
hist(ims_dswe_raw , breaks = 100)

####### bring in snow pillow data
# pull out location info into separate df
pillow_locations <-read.csv("~/ch3_fusion/csvs/cadwr_pillows_meta_uavsar_v1.csv", header = TRUE)

# plot pillow location using terra vector functionality
pillow_point <-vect(pillow_locations, geom = c("lon","lat"), crs = crs(unw_modis)) #needs to be 
plot(modscag_dswe_raw)
points(pillow_point, cex = 1)
text(pillow_point, labels = c("VLC", "DPO", "MHP","UBC","WWC"), pos = 3)

# calculate SWE change at pillow
cadwr_swe <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csvs")
cadwr_swe$date <-as.Date(cadwr_swe$date)

# test plot from vlc cadwr pillow
ggplot(cadwr_swe, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# study period filter
sp <-dplyr::filter(cadwr_swe, date > "2020-02-26" & date < "2020-03-11")

ggplot(sp, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# calc change in SWE at pillow from feb 26 - march 11
station_dswe <- sp %>%
  group_by(id) %>%
  summarize(dswe_cm = swe_cm[13] - swe_cm[1])

station_dswe

# extract using that vector
pillow_cell_dswe <-terra::extract(modscag_dswe_raw, pillow_point,  cells = TRUE, xy = TRUE, ID = TRUE)
pillow_cell_dswe$id <-c("VLC", "DPO", "MHP","UBC","WWC")
pillow_cell_dswe

# extract 8 surronding cells
test_cells <-adjacent(modscag_dswe_raw, pillow_cell_dswe$cell, direction = 8)

# for five stations
vlc_cells <-c(pillow_cell_dswe$cell[1],test_cells[1,])
dpo_cells <-c(pillow_cell_dswe$cell[2],test_cells[2,])
mhp_cells <-c(pillow_cell_dswe$cell[3],test_cells[3,])
ubc_cells <-c(pillow_cell_dswe$cell[4],test_cells[4,])
wwc_cells <-c(pillow_cell_dswe$cell[5],test_cells[5,])

# extract
vlc_vals <-terra::extract(modscag_dswe_raw, vlc_cells)
colnames(vlc_vals) <-"vlc"
dpo_vals <-terra::extract(modscag_dswe_raw, dpo_cells)
colnames(dpo_vals) <-"dpo"
mhp_vals <-terra::extract(modscag_dswe_raw, mhp_cells)
colnames(mhp_vals) <-"mhp"
ubc_vals <-terra::extract(modscag_dswe_raw, ubc_cells)
colnames(ubc_vals) <-"ubc"
wwc_vals <-terra::extract(modscag_dswe_raw, wwc_cells)
colnames(wwc_vals) <-"wwc"

# make df
vals_df <-cbind(vlc_vals, dpo_vals, mhp_vals, ubc_vals, wwc_vals)


# bind and find average swe change
bind <-left_join(pillow_cell_dswe, station_dswe)
bind_v2 <-dplyr::filter(bind, id != "DPO")
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
modscag_dswe <-modscag_dswe_raw + tether_value
plot(modscag_dswe)
hist(modscag_dswe, breaks = 100)
# writeRaster(modscag_dswe, "~/ch3_fusion/rasters/uavsar/dswe/modscag_dswe_v2.tif")

# modis
modis_dswe <-modis_dswe_raw + tether_value
# writeRaster(modis_dswe, "./rasters/uavsar/dswe/modis_dswe_v2.tif")

# viirs
viirs_dswe <-viirs_dswe_raw + tether_value
plot(viirs_dswe)
# writeRaster(viirs_dswe, "./rasters/uavsar/dswe/viirs_dswe_v2.tif")

# landsat
landsat_dswe <-landsat_dswe_raw + tether_value
plot(landsat_dswe)
# writeRaster(landsat_dswe, "./rasters/uavsar/dswe/landsat_dswe_v2.tif")

# flm
flm_dswe <-flm_dswe_raw + tether_value
plot(flm_dswe)
# writeRaster(flm_dswe, "./rasters/uavsar/dswe/flm_dswe_v2.tif")

# ims
ims_dswe <-ims_dswe_raw + tether_value
plot(ims_dswe)
# writeRaster(ims_dswe, "./rasters/uavsar/dswe/ims_dswe_v2.tif")
