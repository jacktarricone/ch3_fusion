# fsca mask stats
# jack tarricone
# december 6th, 2022

library(terra)

setwd("/Users/jacktarricone/ch3_fusion/rasters/")
list.files()

#### bring in full usj rast: coherence
cor <-rast("./clips/usj/cor_usj_20200305.tif")
cor
plot(cor)

# count pixels for NA and value
usj_pixels <-as.integer(global(cor, fun="notNA", na.rm = TRUE))

# pixels in domain = 624613

############################################################
##### mask phase data with the different fsca products #####
############################################################

# modscag
unw_modscag <-rast("./clips/usj/unw_modscag.tif")
unw_modscag
plot(unw_modscag)

# modis
unw_modis <-rast("./clips/usj/unw_modis.tif")
unw_modis
plot(unw_modis)

# viirs
unw_viirs <-rast("./clips/usj/unw_viirs.tif")
unw_viirs
plot(unw_viirs)

# landsat
unw_landsat <-rast("./clips/usj/unw_landsat.tif")
unw_landsat
plot(unw_landsat)

# flm
unw_flm <-rast("./clips/usj/unw_flm.tif")
unw_flm
plot(unw_flm)

######## calc pixel mask percent
# define function
percent_snow <-function(x){
  x_pixels <-as.integer(global(x, fun="notNA", na.rm = TRUE))
  print(x_pixels)
  percent_masked <-as.integer((x_pixels / usj_pixels) * 100)
  return(percent_masked)
}

# calc for all 5 layers
modscag_mp <-percent_snow(unw_modscag)
modis_mp <-percent_snow(unw_modis)
viirs_mp <-percent_snow(unw_viirs)
landsat_mp <-percent_snow(unw_landsat)
flm_mp <-percent_snow(unw_flm)

df <-data.frame(c("MODSCAG","MODIS","VIIRS","Landsat","FLM"),
                c(modscag_mp,modis_mp,viirs_mp,landsat_mp,flm_mp))

colnames(df)[1:2] <-c("sensor","pecernt")
write.csv(df, "/Users/jacktarricone/ch3_fusion/in_situ/mask_stats.csv")
