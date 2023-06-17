# sierra swe calc with different fsca masks
# jack tarricone
# decemeber 5, 2022

library(terra)
library(ggplot2);theme_set(theme_classic(12))

setwd("/Users/jacktarricone/ch3_fusion/rasters/uavsar/feb26_march11_80m/")
list.files()

## bring in inc
inc <-rast("./inc_80m.tif")
plot(inc)

# bring masked unwrapped phase rastesr
unw_modscag <-rast("./unw_modscag_mask.tif")
plot(unw_modscag)

unw_modis <-rast("./unw_modis_mask.tif")
plot(unw_modis)

unw_viirs <-rast("./unw_viirs_mask.tif")
plot(unw_viirs)

unw_landsat <-rast("./unw_landsat_mask.tif")
plot(unw_landsat)

unw_flm <-rast("./unw_flm_mask.tif")
plot(unw_flm)

# stack, yup that works
stack <-c(inc,unw_modscag,unw_modis,unw_viirs,unw_landsat,unw_flm)

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

##### 
## modscag
#####

modscag_depth_change <-depth_from_phase(delta_phase = unw_modscag,
                                        inc_angle = inc,
                                        perm = 1.3,
                                        wavelength = uavsar_wL)

# convert to SWE change
modscag_dswe_raw <-modscag_depth_change*(270/1000)
plot(modscag_dswe_raw)
hist(modscag_dswe_raw , breaks = 100)
writeRaster()

##### 
## modis
#####

modis_depth_change <-depth_from_phase(delta_phase = unw_modis,
                                        inc_angle = inc,
                                        perm = 1.3,
                                        wavelength = uavsar_wL)

# convert to SWE change
modis_dswe_raw <-modis_depth_change*(270/1000)
plot(modis_dswe_raw)
hist(modis_dswe_raw , breaks = 100)

##### 
## viirs
#####

viirs_depth_change <-depth_from_phase(delta_phase = unw_viirs,
                                      inc_angle = inc,
                                      perm = 1.3,
                                      wavelength = uavsar_wL)

# convert to SWE change
viirs_dswe_raw <-viirs_depth_change*(270/1000)
plot(viirs_dswe_raw)
hist(viirs_dswe_raw , breaks = 100)

##### 
## landsat
#####

landsat_depth_change <-depth_from_phase(delta_phase = unw_landsat,
                                      inc_angle = inc,
                                      perm = 1.3,
                                      wavelength = uavsar_wL)

# convert to SWE change
landsat_dswe_raw <-landsat_depth_change*(270/1000)
plot(landsat_dswe_raw)
hist(landsat_dswe_raw , breaks = 100)

##### 
## flm
#####

flm_depth_change <-depth_from_phase(delta_phase = unw_flm,
                                        inc_angle = inc,
                                        perm = 1.3,
                                        wavelength = uavsar_wL)

# convert to SWE change
flm_dswe_raw <-flm_depth_change*(270/1000)
plot(flm_dswe_raw)
hist(flm_dswe_raw , breaks = 100)

####### bring in snow pillow data

# pull out location info into separate df
vlc_loc <-read.csv("/Users/jacktarricone/ch3_fusion/in_situ/vlc_loc.csv", header = TRUE)

loc <-data.frame(lat = vlc_loc$lat[1],
                 lon = vlc_loc$lon[1])

# plot pillow location using terra vector functionality
pillow_point <-vect(loc, geom = c("lon","lat"), crs = crs(unw_modis)) #needs to be 
plot(modis_dswe_raw)
points(pillow_point, cex = 1)

# calculate SWE change at pillow
vlc <-read.csv("/Users/jacktarricone/ch3_fusion/in_situ/VOLCANIC KNOB (VLC).csv")
colnames(vlc)[1:2] <-c("date","swe_in")
vlc

vlc$date <-lubridate::mdy(vlc$date)
vlc$swe_in <-as.numeric(vlc$swe_in)
vlc$swe_cm <-as.numeric(vlc$swe_in *2.54)

# test plot from vlc cadwr pillow
ggplot(vlc) +
  geom_line(aes(x = date, y = swe_cm))

# study period filter
sp <-dplyr::filter(vlc, date > "2020-02-26" & date < "2020-03-11")

ggplot(sp) +
  geom_line(aes(x = date, y = swe_cm))

# calc change in SWE at pillow from feb 26 - march 11
insitu_dswe <-sp$swe_cm[13] - sp$swe_cm[1]

# extract using that vector
pillow_cell_dswe <-terra::extract(modis_dswe_raw, pillow_point,  cells = TRUE, xy = TRUE)

# create tether value
tether_value <-insitu_dswe - pillow_cell_dswe

########## calc absolute dswe

# modscag
modscag_dswe <-modscag_dswe_raw + tether_value$lyr1
plot(modscag_dswe)
writeRaster(modscag_dswe, "./modscag_dswe.tif")

# modis
modis_dswe <-modis_dswe_raw + tether_value$lyr1
# writeRaster(modis_dswe, "./modis_dswe.tif")

# viirs
viirs_dswe <-viirs_dswe_raw + tether_value$lyr1
plot(viirs_dswe)
# writeRaster(viirs_dswe, "./viirs_dswe.tif")

# landsat
landsat_dswe <-landsat_dswe_raw + tether_value$lyr1
plot(landsat_dswe)
# writeRaster(landsat_dswe, "./landsat_dswe.tif")

# flm
flm_dswe <-flm_dswe_raw + tether_value$lyr1
plot(flm_dswe)
# writeRaster(flm_dswe, "./flm_dswe.tif")
