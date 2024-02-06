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

theme_set(theme_classic(12))

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
temp_csv$temp_c <-(temp_csv$temp_f - 32) * (5/9)
temp_csv$date_time <-ymd_hm(temp_csv$date_time)


# change names
names(dat) <-headers

# change to numeric
dat[, 2:28] <- sapply(dat[, 2:28], as.numeric)

# quick test plot
ggplot(dat)+
  geom_line(aes(x = date_time, y = DSDepth_6))+
  lims(y = c(0, 100))

# filter and plot for 2/12-2/26
filt <-filter(dat, date_time >= "2020-02-11 00:00:00" & date_time <= "2020-03-07 18:50:00")

######## read in HQ met data
vg_met_data <-read.csv("./in_situ/vg_met_data_qaqc.csv")
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time, tz = "MST")

# filter down to same date range
vg_filt <-filter(vg_met_data, date_time >= "2020-02-11 00:00:00" & date_time <= "2020-03-07 18:50:00")
vg_filt$vg_snow_depth_cm <-vg_filt$sd_interp_v3_smooth30
tail(vg_filt$date_time)

# quick test plot
ggplot(vg_filt)+
  geom_line(aes(x = date_time, y = vg_snow_depth_cm)) +
  lims(y = c(0, 100))

######### read in redondo met data
redondo_met_data <-read.csv("./in_situ/redondo_met_data_qaqc.csv")
redondo_met_data$date_time <-ymd_hms(redondo_met_data$date_time, tz = "MST")
redondo_met_data$date <-ymd(redondo_met_data$date)

# filter down to same date range
redondo_filt <-filter(redondo_met_data, date_time >= "2020-02-11 00:00:00" & date_time <= "2020-03-07 18:50:00")
tail(redondo_filt$date_time)

# bind snow depth and date_time cols to df
insar <-cbind(filt,
              vg_filt$vg_snow_depth_cm)

# rename new cols
names(insar)[29] <- "vg_snow_depth_cm"

# test_plot
ggplot(insar) +
  geom_point(aes(x = date_time, y = vg_snow_depth_cm))

# define flight dates and times for uavsar
flight1 <-as.numeric(insar$date_time[33]) # row for correct date time
flight2 <-as.numeric(insar$date_time[202])
flight3 <-as.numeric(insar$date_time[369])

# landsat aquisistions
fsca1 <-vg_met_data$date_time[424]
fsca2 <-vg_met_data$date_time[805]

# fine storm start and end
storm_start <-insar$date_time[264]
storm_end <-insar$date_time[294]

##############################
#### plot snow depth #########
##############################

snow_depth <-ggplot(insar)+
  geom_vline(xintercept = flight1, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = fsca1, linetype=2, col = "darkorange", alpha = .7) +
  geom_vline(xintercept = fsca2, linetype=2, col = "darkorange", alpha = .7) +
  annotate("rect", xmin = storm_start, xmax = storm_end,
           ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(aes(x = date_time, y = DSDepth_1), col = "gray50", size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_3), col = "gray50", size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_4), col = "gray50", size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_6), col = "gray50", size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_7), col = "gray50", size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_9), col = "gray50", size = .5)+
  geom_line(aes(x = date_time, y = vg_snow_depth_cm), col = "black", size = .7)+
  scale_y_continuous(expand = c(0,0), 
                     limits = c(40,120),
                     breaks = c(seq(40,120,20)))+
  ylab(expression(atop("Snow Depth",paste("(cm)"))))+
  xlab("Date") +
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "1 day",
                   expand = c(0,1),
                   limits = ymd_hms(c("2020-02-11 00:00:00", "2020-03-06 00:00:00"), tz = "MST")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

plot(snow_depth)

##############################
#### plot air temperature ####
##############################

temp <-ggplot() +
  geom_hline(yintercept = 0, linetype = 3, col = "grey50", alpha = .7) +
  geom_vline(xintercept = flight1, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = fsca1, linetype=2, col = "darkorange", alpha = .7) +
  geom_vline(xintercept = fsca2, linetype=2, col = "darkorange", alpha = .7) +
  annotate("rect", xmin = storm_start, xmax = storm_end,
           ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(data = vg_met_data, aes(x = date_time, y = mean_air_temp_c), col = "black", size = .4) + 
  geom_line(data = redondo_met_data, aes(x = date_time, y = mean_air_temp_c), col = "red", size = .4) +
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "1 day",
                   expand = c(0,1),
                   limits = ymd_hms(c("2020-02-11 00:00:00", "2020-03-06 00:00:00"), tz = "MST"))+
  scale_y_continuous(breaks = seq(-20,10,5), 
                     limit = c(-20,10),
                     expand = c(0,0))+
  xlab("Date") +
  ylab(expression(atop("Air Temperature",paste("(°C)"))))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

plot(temp)

##############################
#### plot wind speed #########
##############################

wind <-ggplot() +
  geom_hline(yintercept = 0, linetype = 3, col = "grey50", alpha = .7) +
  geom_vline(xintercept = flight1, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = fsca1, linetype=2, col = "darkorange", alpha = .7) +
  geom_vline(xintercept = fsca2, linetype=2, col = "darkorange", alpha = .7) +
  annotate("rect", xmin = storm_start, xmax = storm_end,
           ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(data = vg_met_data, aes(x = date_time, y = avg_wind_ms), col = "black", size = .3) + 
  geom_line(data = redondo_met_data, aes(x = date_time, y = avg_wind_ms), col = "red", size = .3) + 
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "1 day",
                   expand = c(0,1),
                   limits = ymd_hms(c("2020-02-11 00:00:00", "2020-03-06 00:00:00"), tz = "MST"))+
  scale_y_continuous(breaks = seq(0,8,2), 
                     limit = c(0,8),
                     expand = c(0,0))+
  xlab("Date") + 
  ylab(expression(atop("Wind Speed",paste("(m/s)"))))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

plot(wind)

##############################
#### plot solar rad #########
##############################

solar <-ggplot() +
  geom_hline(yintercept = 0, linetype = 3, col = "grey50", alpha = .7) +
  geom_vline(xintercept = flight1, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = fsca1, linetype=2, col = "darkorange", alpha = .7) +
  geom_vline(xintercept = fsca2, linetype=2, col = "darkorange", alpha = .7) +
  annotate("rect", xmin = storm_start, xmax = storm_end,
           ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(data = vg_met_data, aes(x = date_time, y = solar_rad_kwh), col = "black", size = .3) + 
  geom_line(data = redondo_met_data, aes(x = date_time, y = solar_rad), col = "red", size = .3) + 
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "2 day",
                   expand = c(0,1),
                   limits = ymd_hms(c("2020-02-11 00:00:00", "2020-03-06 00:00:00"), tz = "MST"))+
  scale_y_continuous(breaks = seq(0,1,.2), 
                     limit = c(0,1),
                     expand = c(0,0))+
  xlab("Date") + 
  ylab(expression(atop("Insolation",paste(~'(kWh/m'^{"2"},')'))))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1))

plot(solar)


# stack with cow plot
plot_grid(snow_depth, temp, wind, solar,
          labels = c("(a)","(b)","(c)","(d)"),
          align = "v", 
          nrow = 4, 
          rel_heights = c(1/4, 1/4, 1/4, 1/4))

# ggsave("./plots/fig05.pdf",
#        width = 7,
#        height = 7,
#        units = "in")





