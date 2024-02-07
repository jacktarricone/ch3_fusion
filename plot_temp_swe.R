# time series of swe and temp
# jack tarricone

# 4 cadwr stations

library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(cowplot)

# set path to '/jemez_lband_swe_code_data' that was downloaded and unzipped from zenodo
# all other file paths are relative
setwd("~/ch3_fusion")
list.files() #pwd

# set custom theme
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

theme_set(theme_classic(14))


# read in pillow data
cadwr_swe <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csv")
cadwr_swe$date <-mdy(cadwr_swe$date)
cadwr_swe <-filter(cadwr_swe, date >= "2020-01-01" & date <= "2020-06-01")

# read in data from noah
temp_path <-list.files("./csvs/", pattern = "_temp.csv", full.names = T)
temp_list <-lapply(temp_path, read.csv)

# convert all to int
temp_list[[1]]$VALUE <-as.integer(temp_list[[1]]$VALUE) 
temp_list[[2]]$VALUE <-as.integer(temp_list[[2]]$VALUE)
temp_list[[3]]$VALUE <-as.integer(temp_list[[3]]$VALUE) 

# bind rows
temp_csv <-bind_rows(temp_list)

####### format for plotting
temp_csv$OBS.DATE <-NULL
temp_csv$UNITS <-NULL
colnames(temp_csv)[1:7] <-c("id","duration","sensor_number","type","date_time", "temp_f","flag")
temp_csv$temp_f <-ifelse(temp_csv$temp_f > 130, NA, temp_csv$temp_f)
temp_csv$temp_c <-(temp_csv$temp_f - 32) * (5/9)
temp_csv$date_time <-ymd_hm(temp_csv$date_time)

# filter and plot for 2/12-2/26
filt <-filter(temp_csv, date_time >= "2020-01-01 00:00:00" & date_time <= "2020-06-01 18:50:00")

# Extract the date and calculate the mean temperature for each day
daily_temp <- filt %>%
  group_by(date = as.Date(date_time)) %>%
  summarize(mean_temp_c = mean(temp_c, na.rm = TRUE),
            max_temp_c = max(temp_c, na.rm = TRUE),
            min_temp_c = min(temp_c, na.rm = TRUE))

# Print the result
print(daily_temp)

# landsat aquisistions
# fine storm start and end
ls1 <-daily_temp$date[32]
ls2 <-daily_temp$date[48]
ls3 <-daily_temp$date[64]

# fine storm start and end
start <-daily_temp$date[31]
end <-daily_temp$date[71]

# define flight dates and times for uavsar
flight2 <-daily_temp$date[43]
flight3 <-daily_temp$date[50]
flight4 <-daily_temp$date[58]

# quick test plot
temp <-ggplot(daily_temp)+
  geom_hline(yintercept = 0, linetype=3, col = "gray30", alpha = 1) +
  geom_vline(xintercept = ls1, linetype=2, col = "purple4", alpha = 1) +
  geom_vline(xintercept = ls2, linetype=2, col = "purple4", alpha = 1) +
  geom_vline(xintercept = ls3, linetype=2, col = "purple4", alpha = 1) +
  geom_vline(xintercept = start, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = flight2, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = flight3, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = flight4, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = end, linetype=2, col = "orange", alpha = 1) +
  annotate("rect", xmin = start, xmax = end,
           ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(aes(x = date, y = max_temp_c), color = "red2", size = .8) +
  geom_line(aes(x = date, y = min_temp_c), color = "blue3", size = .9) +
  ylab("Air Temp (°C)")+
  xlab("Date") +
  scale_x_date(date_labels = "%m",
               date_breaks = "1 month",
               expand = c(0,3))+
               # limits = ymd(c("2020-01-01", "2020-06-01")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

# plot swe
swe <-ggplot(cadwr_swe)+
  geom_vline(xintercept = ls1, linetype=2, col = "purple4", alpha = 1) +
  geom_vline(xintercept = ls2, linetype=2, col = "purple4", alpha = 1) +
  geom_vline(xintercept = ls3, linetype=2, col = "purple4", alpha = 1) +
  geom_vline(xintercept = start, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = flight2, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = flight3, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = flight4, linetype=2, col = "orange", alpha = 1) +
  geom_vline(xintercept = end, linetype=2, col = "orange", alpha = 1) +
  annotate("rect", xmin = start, xmax = end,
           ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(aes(x = date, y = swe_cm, group = id),  size = .5)+
  scale_x_date(date_labels = "%m/%y",
               date_breaks = "1 month",
               expand = c(0,3))+
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,70),
                     breaks = c(seq(0,70,10)))+
  ylab("SWE (cm)")+
  xlab("Date") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))

# stack with cow plot
plot_grid(temp, swe,
          labels = c("(a)","(b)"),
          align = "v", 
          nrow = 2, 
          rel_heights = c(.48, .52))

ggsave("~/ch3_fusion/plots/temp_swe_v1.pdf",
       width = 7,
       height = 5,
       units = "in")

system("open ~/ch3_fusion/plots/temp_swe_v1.pdf")






