# format cadwr SWE and depth data
# june 17th

library(tidyverse)
library(lubridate)

setwd("~/ch3_fusion")

# laod in files
# swe
swe_paths <-list.files("./csvs/cadwr_swe_data", pattern = '*swe.csv', full.names = TRUE)
swe_list <-lapply(swe_paths, read.csv)
swe_df <-bind_rows(swe_list, .id = "column_label")
head(swe_df)

# sd
sd_paths <-list.files("./csvs/cadwr_swe_data", pattern = '*sd.csv', full.names = TRUE)
sd_list <-lapply(sd_paths, read.csv)
sd_df <-bind_rows(sd_list, .id = "column_label")
head(sd_df)

# rename swe cols
new_names <-c("col_label","id","duration","sensor_numnber","snow_wc","date_time","date_raw","swe_in","flag","units")
colnames(swe_df) <-new_names
swe_df$date <-rep(ymd(seq(ymd("2019-10-01"), ymd("2020-09-30"),1)),5)
swe_df$swe_in <-ifelse(swe_df$swe_in < 0, 0, swe_df$swe_in)
swe_df <-dplyr::select(swe_df, c("id", "date", "swe_in"))
head(swe_df)

# sd
new_names <-c("col_label","id","duration","sensor_numnber","snow_dp","date_time","date_raw","snow_depth_in","flag","units")
colnames(sd_df) <-new_names
sd_df$date <-rep(ymd(seq(ymd("2019-10-01"), ymd("2020-09-30"),1)),5)
sd_df$snow_depth_in <-ifelse(sd_df$snow_depth_in < 0, 0, sd_df$snow_depth_in)
sd_df <-dplyr::select(sd_df, c("id", "date", "snow_depth_in"))

# join
identical(sd_df$date, swe_df$date)
joined_df <-full_join(swe_df,sd_df)
head(joined_df)

# filter for wacky summer months
winter <-filter(joined_df, date >= as.Date("2020-01-01") & date <= as.Date("2020-06-01"))

# add more information
df <-winter %>%
  mutate(swe_cm = swe_in*2.54,
         snow_depth_cm = snow_depth_in*2.54,
         bulk_density = (swe_cm/snow_depth_cm)*1000)

# move unrealistic density values
df$bulk_density <-ifelse(df$bulk_density > 500, NA, df$bulk_density)
df$bulk_density <-ifelse(df$bulk_density < 50, NA, df$bulk_density)

head(df)
hist(df$snow_depth_cm, breaks = 50)
hist(df$swe_cm, breaks = 50)
hist(df$bulk_density, breaks = 50)

write.csv(df, "./csvs/cadwr_swe_depth_qaqc_v1.csvs")
