# dswe smoothing with new data
# jack tarricone

library(terra)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

theme_classic <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # show axes
      # axis.line      = element_line(colour = "black", linewidth = rel(1)),
      
      # match legend key to panel.background
      legend.key       = element_blank(),
      
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      
      complete = TRUE
    )
}

theme_set(theme_classic(16))

setwd("~/ch3_fusion/rasters/new_dswe/")

# load sensor names
names <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")

# load in 80 m insar dswe products, and convert to meters
p1_list <-list.files("./p1", pattern = ".tif", full.names = T)
p1_m <-rast(p1_list)/100
file_name <-basename(p1_list)
names(p1_m) <-names
p1_m

p2_list <-list.files("./p2", pattern = ".tif", full.names = T)
p2_m <-rast(p2_list)/100
file_name <-basename(p2_list)
names(p2_m) <-names
p2_m

p3_list <-list.files("./p3", pattern = ".tif", full.names = T)
p3_m <-rast(p3_list)/100
file_name <-basename(p3_list)
names(p3_m) <-names

p4_list <-list.files("./p4", pattern = ".tif", full.names = T)
p4_m <-rast(p4_list)/100
file_name <-basename(p4_list)
names(p4_m) <-names

# bring in cc, mask, and resample
cc_v3 <-rast("~/ch3_fusion/rasters/geo_layers/cc_domain.tif")
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
cc_v1 <-mask(cc_v3, sierra)
cc <-resample(cc_v1, p1_m, method = 'bilinear')
cc

# calculate average cell area in cubic meters
cell_size_rast_m2 <-cellSize(p1_m, unit = "m")
cell_size_rast_m2

global(cell_size_rast_m2, mean)

########## p1
# 41 x 41 moving window sum in cubic meters
# conver to cubic meters


##################### UPDATE FOR ROSS AND CSV AND PLOTS
p1_m3 <-p1_m * cell_size_rast_m2

# apply moving window
p1_41 <-focal(p1_m3, c(41,41), na.rm=TRUE, fun = "sum")
p1_mw <-(p1_41/(41*41)) # divide by

# names for looop
col_name <-c("flm","ims","landsat","modis","modscag","viirs")

for (i in 1:length(col_name)){
  
  dataset <-col_name[i]
  writeRaster(p1_mw[[i]], paste0("~/ch3_fusion/rasters/dswe_variabilty_analysis/p1/p1_",dataset,"_m3_41x41_v6.tif"))
  
}

# save csv
p1_csv <-as.data.frame(p1_mw)
# data.table::fwrite(p1_csv, "~/ch3_fusion/rasters/dswe_variabilty_analysis/p1_m3_41x41_20200131_20200212_v6.csv")


########## p2
# 41 x 41 moving window sum in cubic meters
p2_m3 <-p2_m * cell_size_rast_m2

# apply moving window
p2_41 <-focal(p2_m3, c(41,41), na.rm=TRUE, fun = "sum")
p2_mw <-(p2_41/(41*41))

for (i in 1:length(col_name)){
  
  dataset <-col_name[i]
  writeRaster(p2_mw[[i]], paste0("~/ch3_fusion/rasters/dswe_variabilty_analysis/p2/p2_",dataset,"_m3_41x41_v6.tif"))
  
}

# save csv
p2_csv <-as.data.frame(p2_mw)
# data.table::fwrite(p2_csv, "~/ch3_fusion/rasters/dswe_variabilty_analysis/p2_m3_41x41_20200212_20200219_v6.csv")


########## p3
# 41 x 41 moving window sum in cubic meters
p3_m3 <-p3_m * cell_size_rast_m2

# apply moving window
p3_41 <-focal(p3_m3, c(41,41), na.rm=TRUE, fun = "sum")
p3_mw <-(p3_41/(41*41))

for (i in 1:length(col_name)){
  
  dataset <-col_name[i]
  writeRaster(p3_mw[[i]], paste0("~/ch3_fusion/rasters/dswe_variabilty_analysis/p3/p3_",dataset,"_m3_41x41_v6.tif"))
  
}

# save csv
p3_csv <-as.data.frame(p3_mw)
# data.table::fwrite(p3_csv, "~/ch3_fusion/rasters/dswe_variabilty_analysis/p3_m3_41x41_20200219_20200226_v6.csv")

########## p4
# 41 x 41 moving window sum in cubic meters
p4_m3 <-p4_m * cell_size_rast_m2

# apply moving window
p4_41 <-focal(p4_m3, c(41,41), na.rm=TRUE, fun = "sum")
p4_mw <-(p4_41/(41*41))

for (i in 1:length(col_name)){
  
  dataset <-col_name[i]
  writeRaster(p4_mw[[i]], paste0("~/ch3_fusion/rasters/dswe_variabilty_analysis/p4/p4_",dataset,"_m3_41x41_v6.tif"))
  
}

# save csv
p4_csv <-as.data.frame(p4_mw)
# data.table::fwrite(p4_csv, "~/ch3_fusion/rasters/dswe_variabilty_analysis/p4_m3_41x41_20200226_20200311_v6.csv")

### format data.frames for plotting
p1_df <-as.data.frame(p1_mw, xy = TRUE)
p1_df$pair <-rep("P1", nrow(p1_df))
p1_df_l <-pivot_longer(p1_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p2_df <-as.data.frame(p2_mw, xy = TRUE)
p2_df$pair <-rep("P2", nrow(p2_df))
p2_df_l <-pivot_longer(p2_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p3_df <-as.data.frame(p3_mw, xy = TRUE)
p3_df$pair <-rep("P3", nrow(p3_df))
p3_df_l <-pivot_longer(p3_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p4_df <-as.data.frame(p4_mw, xy = TRUE)
p4_df$pair <-rep("P4", nrow(p4_df))
p4_df_l <-pivot_longer(p4_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))


# bind for plotting
plotting_df <-rbind(p1_df_l,p2_df_l,p3_df_l,p4_df_l)
plotting_df$data_set <-ifelse(plotting_df$data_set == "MODSCAG","STC",plotting_df$data_set)
colnames(plotting_df)[5] <-"dswe_m3"
head(plotting_df)
# data.table::fwrite(plotting_df, "~/ch3_fusion/csvs/dswe_new_41_plotting_v7.csv")
