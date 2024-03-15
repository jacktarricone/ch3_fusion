library(dplyr)
library(lubridate)
library(ggplot2);theme_set(theme_classic(12))


setwd("~/ch3_fusion")

# read df
df <-read.csv("./snowex_insitu/mam_SNEX20_TS_SP_Summary_SWE_v01.csv")
df$date_time <-ymd_hm(df$date_time)
df$date <-as.Date(df$date_time)
df

# filter to insar dates
insar_dates <- df %>%
  filter(date %in% as.Date(c("2020-01-29", "2020-02-12", "2020-02-19", "2020-02-26", "2020-03-11")))

print(insar_dates)

# Calculate differences in swe_mean_mm
diff_site <- insar_dates %>%
  group_by(site) %>%
  summarize(diff_cm = diff(swe_mean_mm)/10)

diff_site
diff(insar_dates$swe_mean_mm)/10
write.csv(diff_site, "./csvs/mam_pit_diff.csv")
