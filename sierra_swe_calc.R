# sierra swe calc with different fsca masks
# jack tarricone
# decemeber 5, 2022

library(terra)

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
