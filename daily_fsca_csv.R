# calculate daily % and m^2 fsca mask stats for USJ
# jack tarricone
# December 20th, 2022

library(terra)

setwd("/Users/jacktarricone/ch3_fusion/rasters/")
list.files()

### bring nisar sim data from feb 22 - march 5th
# coherence
nisar_cor_raw <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
nisar_cor <-project(nisar_cor_raw, crs('EPSG:4326'))
nisar_cor
plot(nisar_cor)

## resampling test
modis <-rast("./MOD10A1F_wy2020/fsca/modis_fsca_20200217.tif")

#### test the different resample methods

# modis_bi <-resample(modis, nisar_cor, method = "bilinear")
# modis_nn <-resample(modis, nisar_cor, method = "near")
# modis_cubic <-resample(modis, nisar_cor, method = "cubic")
# modis_cs <-resample(modis, nisar_cor, method = "cubicspline")  
# modis_la <-resample(modis, nisar_cor, method = "lanczos")  
# modis_sum <-resample(modis, nisar_cor, method = "sum") 
# 
# modis_bi
# modis_nn
# modis_cubic
# modis_cs
# modis_la
# modis_sum
# 
# plot(modis_bi[[1]])
# plot(modis_nn[[1]])
# plot(modis_cubic[[1]])
# plot(modis_cs[[1]])
# plot(modis_la[[1]])
# plot(modis_sum[[1]])
  

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
