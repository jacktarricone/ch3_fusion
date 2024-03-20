# format sesame hourly swe data

library(dplyr)
library(lubridate)

setwd("~/ch3_fusion/csvs/")

# read in sesame pillow
sesame_hr <-read.csv("./sesame_pillow_raw.csv")
sesame_hr$date_time <-mdy_hm(sesame_hr$date_time)
sesame_hr$date <-as.Date(sesame_hr$date_time)

# Compute daily average of swe_cm
sesame_swe <- sesame_hr %>%
  group_by(date) %>%
  summarise(swe_cm = mean(swe_cm))

# View the result
print(sesame_swe)

# test plot
ggplot(sesame_swe, aes(x = date, y = swe_cm)) +
  geom_line()

write.csv(sesame_swe, "./sesame_swe.csv")

# dri pillows
