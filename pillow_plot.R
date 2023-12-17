# sierra swe calc with different fsca masks: pair 1 jan 31 -- feb 11
# jack tarricone
# decemeber 5, 2022

library(terra)
library(dplyr)
library(ggplot2)

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

theme_set(theme_classic(15))


setwd("~/ch3_fusion/rasters/")


# calculate SWE change at pillow
cadwr_swe <-read.csv("~/ch3_fusion/csvs/cadwr_swe_depth_qaqc_v1.csv")
cadwr_swe$date <-as.Date(cadwr_swe$date)





# landsat aquisistions
fsca1 <-vg_met_data$date_time[424]
fsca2 <-vg_met_data$date_time[805]

# fine storm start and end
start <-cadwr_swe$date[31]
end <-cadwr_swe$date[71]

# define flight dates and times for uavsar
flight2 <-as.numeric(cadwr_swe$date[43])
flight3 <-as.numeric(cadwr_swe$date[51])
flight4 <-as.numeric(cadwr_swe$date[59])




p <-ggplot(cadwr_swe)+
  geom_vline(xintercept = start, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = flight4, linetype=2, col = "darkblue", alpha = .7) +
  geom_vline(xintercept = end, linetype=2, col = "darkblue", alpha = .7) +
  annotate("rect", xmin = start, xmax = end,
           ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(aes(x = date, y = swe_cm, col = id),  size = 1)+
  # geom_line(aes(x = date_time, y = DSDepth_3), col = "gray50", size = .5)+
  # geom_line(aes(x = date_time, y = DSDepth_4), col = "gray50", size = .5)+
  # geom_line(aes(x = date_time, y = DSDepth_6), col = "gray50", size = .5)+
  # geom_line(aes(x = date_time, y = DSDepth_7), col = "gray50", size = .5)+
  # geom_line(aes(x = date_time, y = DSDepth_9), col = "gray50", size = .5)+
  # geom_line(aes(x = date_time, y = vg_snow_depth_cm), col = "black", size = .7)+
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,70),
                     breaks = c(seq(0,70,10)))+
  ylab("SWE (cm)")+
  xlab("Date") +
  # scale_x_datetime(date_labels = "%m/%d",
  #                  date_breaks = "1 day",
  #                  expand = c(0,1),
  #                  limits = ymd_hms(c("2020-02-11 00:00:00", "2020-03-06 00:00:00"), tz = "MST")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
      legend.position = c(.1,.8),
      legend.direction = 'vertical',
      legend.title = element_blank(),
      plot.margin = unit(c(.25,.25, 0,.25), "cm"))

p



ggsave(p,
       file = "./plots/pillow_plot_v1.png",
       width = 8, 
       height = 5,
       units = "in",
       dpi = 300) 

system("open ./plots/pillow_plot_v1.png")


# study period filter
sp <-dplyr::filter(cadwr_swe, date > "2020-01-30" & date < "2020-02-13")

ggplot(sp, aes(x = date, y = swe_cm, color = id)) +
  geom_line()

# calc change in SWE at pillow from feb 26 - march 11
station_dswe <- sp %>%
  group_by(id) %>%
  summarize(dswe_cm = swe_cm[13] - swe_cm[1])

station_dswe

# extract using that vector
pillow_cell_dswe <-terra::extract(dswe_raw[[1]], pillow_point,  cells = TRUE, xy = TRUE, ID = TRUE)
pillow_cell_dswe$id <-c("VLC", "DPO", "MHP","UBC","WWC")
pillow_cell_dswe

# extract 8 surronding cells
test_cells <-adjacent(dswe_raw, pillow_cell_dswe$cell, direction = 8)

# for five stations
vlc_cells <-c(pillow_cell_dswe$cell[1],test_cells[1,])
dpo_cells <-c(pillow_cell_dswe$cell[2],test_cells[2,])
mhp_cells <-c(pillow_cell_dswe$cell[3],test_cells[3,])
ubc_cells <-c(pillow_cell_dswe$cell[4],test_cells[4,])
wwc_cells <-c(pillow_cell_dswe$cell[5],test_cells[5,])

# extract
vlc_vals <-terra::extract(dswe_raw, vlc_cells)
colnames(vlc_vals) <-"vlc"
# dpo_vals <-terra::extract(modscag_dswe_raw, dpo_cells)
# colnames(dpo_vals) <-"dpo"
mhp_vals <-terra::extract(dswe_raw, mhp_cells)
colnames(mhp_vals) <-"mhp"
ubc_vals <-terra::extract(dswe_raw, ubc_cells)
colnames(ubc_vals) <-"ubc"
# wwc_vals <-terra::extract(dswe_raw, wwc_cells)
# colnames(wwc_vals) <-"wwc"

# make df
vals_df <-cbind(vlc_vals, mhp_vals, ubc_vals)

# bind and find average swe change
bind_v2 <-left_join(pillow_cell_dswe, station_dswe)
bind_v2

# mean them all, pretty much the same
mean_insar_dswe <-mean(c(vals_df$vlc, vals_df$mhp, vals_df$ubc, vals_df$wwc), na.rm = TRUE)

# calc mean
mean_pillow_dswe <-mean(bind_v2$dswe_cm)
# mean_insar_dswe <-mean(bind_v2$'sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.unw.grd')

# create tether value
tether_value <- mean_pillow_dswe - mean_insar_dswe

########## calc absolute dswe
# modscag
dswe <-dswe_raw + tether_value
plot(dswe)
hist(dswe, breaks = 100)

# save
writeRaster(dswe[[1]], "./new_dswe/p1/p1_ims_dswe_cm_v2.tif")
writeRaster(dswe[[2]], "./new_dswe/p1/p1_modscag_dswe_cm_v2.tif")
writeRaster(dswe[[3]], "./new_dswe/p1/p1_modis_dswe_cm_v2.tif")
writeRaster(dswe[[4]], "./new_dswe/p1/p1_viirs_dswe_cm_v2.tif")
writeRaster(dswe[[5]], "./new_dswe/p1/p1_flm_dswe_cm_v2.tif")
writeRaster(dswe[[6]], "./new_dswe/p1/p1_landsat_dswe_cm_v2.tif")

