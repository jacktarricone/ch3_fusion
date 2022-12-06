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






# testing
flm_depth_change <-depth_from_phase(delta_phase = unw_flm,
                                inc_angle = inc,
                                perm = 1.3,
                                wavelength = uavsar_wL)

# convert to SWE change
flm_dswe_raw <-flm_depth_change*(270/1000)
plot(flm_dswe_raw )
hist(flm_dswe_raw , breaks = 100)
writeRaster(flm_dswe_raw, "./flm_dswe_raw.tif")

# testing
modis_depth_change <-depth_from_phase(delta_phase = unw_modis,
                                    inc_angle = inc,
                                    perm = 1.3,
                                    wavelength = uavsar_wL)

# convert to SWE change
modis_dswe_raw <-modis_depth_change*(270/1000)
plot(modis_dswe_raw)
hist(modis_dswe_raw, breaks = 100)
#writeRaster(modis_dswe_raw, "./modis_dswe_raw.tif")





# bring in snow pillow data
vlc <-read.csv("/Users/jacktarricone/ch3_fusion/in_situ/VOLCANIC KNOB (VLC).csv")
colnames(vlc)[1:2] <-c("date","swe_in")
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

# calc change in SWE at pillow
insitu_dswe <-sp$swe_cm[13] - sp$swe_cm[1]
