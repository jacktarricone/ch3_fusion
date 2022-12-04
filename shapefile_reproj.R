# shape file reprojection from map
# jack tarricone
# december 4th

library(terra)
library(sf)

# usj
usj_raw <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/upper_san_joaquin.gpkg")
plot(usj_raw)

# snsr
snsr_raw <-vect("/Users/jacktarricone/ch1_margulis/static/polygon/SNSR_shp_fixed.shp")
plot(snsr_raw)

# uavsar sierra
sierra_raw <-vect("/Users/jacktarricone/ch3_fusion/uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
plot(sierra_raw)

# states
states_raw_v1 <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
states_raw <-crop(states_raw_v1, ext(-125, 75, 23, 50))
plot(states_raw)

setwd("./ch3_fusion/shapefiles/")
## reproj
states <-terra::project(states_raw, "ESRI:102009")
plot(states, add = TRUE)
writeVector(states, "states_lamb.shp")

snsr <-terra::project(snsr_raw, "ESRI:102009")
plot(snsr)
writeVector(snsr, "snsr_lamb.shp")

usj <-terra::project(usj_raw, "ESRI:102009")
plot(usj, add = TRUE)
writeVector(usj, "usj_lamb.shp")

sierra <-terra::project(sierra_raw, "ESRI:102009")
plot(sierra, add = TRUE)
writeVector(sierra, "sierra_lamb.shp")
