# create fsca stats table
# jack tarricone
# jan 29, 2024

library(terra)
library(dplyr)
library(ggplot2);theme_set(theme_classic(12))

setwd("~/ch3_fusion/rasters/")

# define sensor names
names <-c("flm","ims","landsat","modis","modscag","viirs")

##### load in fsca raster stacks for the four pairs
# p1
p1_list <-list.files("./new_optical/p1_80m_20200131_20200212", pattern = "_80m", full.names = TRUE)
p1_stack <-rast(p1_list)
names(p1_stack) <-names
p1_stack

# p2
p2_list <-list.files("./new_optical/p2_80m_20200212_20200219", pattern = "_80m", full.names = TRUE)
p2_stack <-rast(p2_list)
names(p2_stack) <-names
p2_stack

# p3
p3_list <-list.files("./new_optical/p3_80m_20200219_20200226", pattern = "_80m", full.names = TRUE)
p3_stack <-rast(p3_list)
names(p3_stack) <-names
p3_stack

# p4
p4_list <-list.files("./new_optical/p4_80m_20200226_20200311", pattern = "_80m", full.names = TRUE)
p4_stack <-rast(p4_list)
names(p4_stack) <-names
p4_stack

## load in sierra shape
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
plot(sierra)

# calc total study area
total_area <-expanse(sierra, unit="km", transform=TRUE)

# mask for 50% fsca
calc_fsca_stats <-function(fsca_stack){
  p_50 <-ifel(fsca_stack< 50, NA, fsca_stack)
  result <-expanse(p_50, unit="km", transform=TRUE)
  result$percent <-round((result$area/total_area)*100, digits = 0)
  result$dataset <-names
  df <-result[-c(1,2)]
  df <-df[c(2,1)]
  return(df)
}

# calc stats for each stack
p1_stats <-calc_fsca_stats(p1_stack) 
p2_stats <-calc_fsca_stats(p2_stack) 
p3_stats <-calc_fsca_stats(p3_stack) 
p4_stats <-calc_fsca_stats(p4_stack) 


# create df
full_df1 <-left_join(p1_stats,p2_stats, by = "dataset")
full_df2 <-left_join(full_df1,p3_stats, by = "dataset")
full_df <-left_join(full_df2,p4_stats, by = "dataset")
colnames(full_df)[2:5] <-c("p1","p2","p3","p4")
full_df
