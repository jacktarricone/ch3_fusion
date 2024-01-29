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

# p1
p2_list <-list.files("./new_optical/p2_80m_20200212_20200219", pattern = "_80m", full.names = TRUE)
p2_stack <-rast(p2_list)
names(p2_stack) <-names
p2_stack

