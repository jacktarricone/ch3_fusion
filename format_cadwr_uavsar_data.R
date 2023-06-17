# format cadwr SWE and depth data
# june 17th

library(tidyverse)
library(lubridate)

setwd("~/ch3_fusion")

# laod in files
# swe
swe_paths <-list.files("./csvs/cadwer_swe_data", pattern = '*swe.csv', full.names = TRUE)
swe_list <-lapply(swe_paths, read.csv)
swe_df <-bind_rows(swe_list, .id = "column_label")

# sd
sd_paths <-list.files("./csvs/cadwer_swe_data", pattern = '*sd.csv', full.names = TRUE)
sd_list <-lapply(sd_paths, read.csv)
sd_df <-bind_rows(sd_list, .id = "column_label")
head(sd_df)

# rename swe cols
new_names <-c("col_label","id","duration","sensor_numnber","snow_wc","date_time","date_raw","swe_in","flag","units")
colnames(swe_df) <-new_names

# sd
new_names <-c("col_label","id","duration","sensor_numnber","snow_dp","date_time","date_raw","snow_depth_in","flag","units")
colnames(sd_df) <-new_names

# join
joined_df <-full_join(swe_df,sd_df)
joined_df$date <-ymd(joined_df$date_raw)
joined_df <-dplyr::select(joined_df, c("id", "date", "flag", "swe_in", "snow_depth_in"))
head(joined_df)

