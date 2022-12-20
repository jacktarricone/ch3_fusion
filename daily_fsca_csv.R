# calculate daily % and m^2 fsca mask stats for USJ
# jack tarricone
# December 20th, 2022

library(terra)

setwd("/Users/jacktarricone/ch3_fusion/rasters/")
list.files()

# bring in modis stack
modis_list <-list.files("./MOD10A1F_wy2020/fsca/", pattern = '.tif', full.names = TRUE)
modis_stack <-rast(modis_list)
modis_stack

# pull out modis snow cover
sc_seq <-seq(1,1825,5)
modis_fsca <-modis_stack[[sc_seq]]
modis_fsca

# usj shp
usj <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/upper_san_joaquin.gpkg")

### bring nisar sim data from feb 22 - march 5th
# coherence
nisar_cor_raw <-rast("./sen1_nisar_sim/wy2020/S1-GUNW-D-R-144-tops-20200305_20200222-135950-38726N_36751N-PP-0915-v2_0_2-coherence.tif")
nisar_cor <-project(nisar_cor_raw, crs('EPSG:4326'))
nisar_cor
plot(nisar_cor)

#### bring in full usj rast: coherence
cor_v1 <-rast("./clips/usj/cor_usj_20200305.tif")
cor <-project(cor_v1, crs('EPSG:4326'))
plot(cor)
cor

# resample and mask
modis_nisar_v1 <-resample(modis_fsca, cor, method = "bilinear")
modis_fsca_usj_80m <-mask(modis_nisar_v1, usj)
modis_fsca_usj_80m
# writeRaster(modis_fsca_usj_80m, "./MOD10A1F_wy2020/fsca_usj_80m/modis_fsca_usj_80m_stack.tif")
plot(modis_usj_80m[[1]])

# calculate area in basin
expanse(modis_usj_80m[[1]], unit = "km")
usj_area <-expanse(cor, unit = "km")

# define function for calculating percent fsca area
# at given fsca threshold
fsca_percent_calc <-function(fsca_rast, total_area, threshold){
  
  # fsca_rast: rast object
  # threshold: number above which pixel is considered snow covered
  test <-fsca_rast # create dummy stack
  values(test)[values(test) < threshold] = NA # mask for threshold
  fsca_area <-expanse(test, unit = "km") # calculate fsca area
  fsca_percent <-(fsca_area/total_area)*100 # calculate percent of basin covered
  return(fsca_percent)
}


# test on full stack
fsca_percent_calc(modis_fsca, total_area = usj_area, threshold = 90)

# pixels in domain = 624613
test <-modis_usj_80m[[1]]
values(test)[values(test) < 15] = NA
plot(test)
fsca_area <-expanse(test, unit = "km")
fsca_percent <-(fsca_area/usj_area)*100


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
