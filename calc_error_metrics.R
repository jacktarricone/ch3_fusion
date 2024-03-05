# calc error metrics
library(terra)
library(ggplot2)
library(dplyr)
library(Metrics)

setwd("~/ch3_fusion/rasters/new_dswe/")

# read in dswe stacks
p1 <-rast(list.files("./p1", pattern = ".tif", full.names = T))
p2 <-rast(list.files("./p2", pattern = ".tif", full.names = T))
p3 <-rast(list.files("./p3", pattern = ".tif", full.names = T))
p4 <-rast(list.files("./p4", pattern = ".tif", full.names = T))

####### bring in snow pillow data
# pull out location info into separate df
pillow_locations <-read.csv("~/ch3_fusion/csvs/cadwr_pillows_meta_uavsar_v1.csv", header = TRUE)

# plot pillow location using terra vector functionality
pillow_point <-vect(pillow_locations, geom = c("lon","lat"), crs = crs(p1)) #needs to be 
plot(p1[[1]])
points(pillow_point, cex = 1)
text(pillow_point, labels = c("VLC", "DPO", "MHP","UBC","WWC"), pos = 3)

# calculate SWE change at pillow
cadwr_swe <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csv")
cadwr_swe$date <-lubridate::mdy(cadwr_swe$date)

# create dates for filtering
uavsar_dates <-lubridate::ymd(c("2020-01-31","2020-02-12","2020-02-19","2020-02-26","2020-03-12"))

# function for calc in situ change
insitu_swe_change <-function(d1,d2){
  
  # study period filter
  sp <-dplyr::filter(cadwr_swe, date >= d1 & date <= d2)
  
  # calc study period length
  dpo <-filter(sp, id == "DPO")
  length <-nrow(dpo)
  
  # calc change in SWE at pillow from feb 12 - 19
  station_dswe <- sp %>%
  group_by(id) %>%
  summarize(dswe_cm = swe_cm[length] - swe_cm[1])

  return(station_dswe)
}

# calc in situ
p1_insitu <-insitu_swe_change(uavsar_dates[1],uavsar_dates[2])
p2_insitu <-insitu_swe_change(uavsar_dates[2],uavsar_dates[3])
p3_insitu <-insitu_swe_change(uavsar_dates[3],uavsar_dates[4])
p4_insitu <-insitu_swe_change(uavsar_dates[4],uavsar_dates[5])

# fun for error mets
calc_error_mets <-function(insar_dswe, insitu_dswe, name){
  
  # extract using that vector
  pillow_cell_dswe <-terra::extract(insar_dswe, pillow_point,  cells = TRUE, xy = TRUE, ID = TRUE)
  pillow_cell_dswe$id <-c("VLC", "DPO", "MHP","UBC","WWC")
  
  # extract 8 surronding cells
  test_cells <-adjacent(insar_dswe, pillow_cell_dswe$cell, direction = 8)
  
  # for five stations
  vlc_cells <-c(pillow_cell_dswe$cell[1],test_cells[1,])
  dpo_cells <-c(pillow_cell_dswe$cell[2],test_cells[2,])
  mhp_cells <-c(pillow_cell_dswe$cell[3],test_cells[3,])
  ubc_cells <-c(pillow_cell_dswe$cell[4],test_cells[4,])
  
  #### calc error metics
  sar_dpo_vals <-terra::extract(insar_dswe, dpo_cells)
  sar_dpo_mean <-mean(colMeans(sar_dpo_vals, na.rm = TRUE), na.rm = TRUE)
  
  sar_mhp_vals <-terra::extract(insar_dswe, mhp_cells)
  sar_mhp_mean <-mean(colMeans(sar_mhp_vals, na.rm = TRUE), na.rm = TRUE)
  
  sar_ubc_vals <-terra::extract(insar_dswe, ubc_cells)
  sar_ubc_mean <-mean(colMeans(sar_ubc_vals,  na.rm = TRUE), na.rm = TRUE)
  
  sar_vlc_vals <-terra::extract(insar_dswe, vlc_cells)
  sar_vlc_mean <-mean(colMeans(sar_vlc_vals, na.rm = TRUE), na.rm = TRUE)
  
  # calc station mean
  mean_sar_swe <-c(sar_dpo_mean,sar_mhp_mean,sar_ubc_mean,sar_vlc_mean)
  
  # mae and rmse
  mae <-mae(insitu_dswe$dswe_cm, mean_sar_swe)
  rmse <-rmse(insitu_dswe$dswe_cm, mean_sar_swe)
  dat <-c(name,as.numeric(mae),as.numeric(rmse))
  return(dat)
}

# calc
p1_error <-calc_error_mets(p1, p1_insitu,"p1")
p2_error <-calc_error_mets(p2, p2_insitu,"p2")
p3_error <-calc_error_mets(p3, p3_insitu,"p3")
p4_error <-calc_error_mets(p4, p4_insitu,"p4")

# conver to df
mat <-rbind(p1_error,p2_error,p3_error,p4_error)
df <-as.data.frame(mat)
colnames(df)[1:3] <-c("pair","mae","rmse")
df$mae <-as.numeric(df$mae)
df$rmse <-as.numeric(df$rmse)
write.csv(df, "~/ch3_fusion/csvs/error_metrics.csv")

mean(df$mae)
mean(df$rmse)
