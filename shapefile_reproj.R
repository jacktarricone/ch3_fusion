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
states_raw <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
plot(states_raw)
