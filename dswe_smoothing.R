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

# load in 80 m insar dswe products, and convert to meters
p1_m <-rast(list.files("./p1", pattern = ".tif", full.names = T))/100
p2_m <-rast(list.files("./p2", pattern = ".tif", full.names = T))/100
p3_m <-rast(list.files("./p3", pattern = ".tif", full.names = T))/100
p4_m <-rast(list.files("./p4", pattern = ".tif", full.names = T))/100


# bring in cc, mask, and resample
cc_v2 <-rast("~/ch3_fusion/rasters/geo_layers/cc_domain.tif")
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
cc_v1 <-mask(cc_v2, sierra)
cc <-resample(cc_v1, p4_m, method = 'bilinear')
plot(cc)
cc

# calculate average cell area in cubic meters
cell_size_rast_m2 <-cellSize(p1_m, unit = "m")
# cell_size_m2 <-as.numeric(global(cell_size_v1, 'max') + global(cell_size_v1, 'min'))/2

########## p1
# 41 x 41 moving window sum in cubic meters
p1_sm <-focal(p1_m, c(41,41), na.rm=TRUE, fun = "sum")
p1_sm_m3 <-p1_sm * cell_size_rast_m2
plot(p1_sm_m3)

names <-c("ims","modscag","modis","viirs","flm","landsat")

for (i in 1:length(names)) {
  
  dataset <-names[i]
  writeRaster(p1_sm_m3[[i]], paste0("~/ch3_fusion/rasters/dswe_variabilty_analysis/p1/p1_",dataset,"_m3_41x41_v1.tif"))
  
}

########## p2
# 41 x 41 moving window sum in cubic meters
p2_sm <-focal(p2_m, c(41,41), na.rm=TRUE, fun = "sum")
p2_sm_m3 <-p2_sm * cell_size_rast_m2
plot(p2_sm_m3)

for (i in 1:length(names)) {
  
  dataset <-names[i]
  writeRaster(p2_sm_m3[[i]], paste0("~/ch3_fusion/rasters/dswe_variabilty_analysis/p2/p2_",dataset,"_m3_41x41_v1.tif"))
  
}







# loss
loss_mw_41x41 <-focal(loss_stack_m3, c(41,41), na.rm=TRUE, fun = "sum")
plot(loss_mw_41x41[[4]])
# writeRaster(loss_mw_41x41, "./rasters/dswe_variabilty_analysis/loss_stack_m3_41x41_v1.tif")

# define sd with na remove
sd_na_rm <-function(x){sd(x, na.rm = TRUE)}

# calculate pixelwise standard deviation
# gain
gain_sd <-app(gain_mw_41x41, fun = sd_na_rm)
gain_sd_dam3 <-gain_sd/1e3
plot(gain_sd_dam3)
# writeRaster(gain_sd_dam3, "./rasters/dswe_variabilty_analysis/gain_sd_dam3_41x41_v1.tif")

# loss
loss_sd <-app(loss_mw_41x41, fun = sd_na_rm)
loss_sd_dam3 <-loss_sd/1e3
plot(loss_sd_dam3)
# writeRaster(loss_sd_dam3, "./rasters/dswe_variabilty_analysis/loss_sd_dam3_41x41_v1.tif")

## cc focal
cc_mw <-focal(cc, c(41,41), na.rm=TRUE, fun = "mean")
plot(cc_mw)
# writeRaster(cc_mw, "./rasters/dswe_variabilty_analysis/cc_mw_mean_41x41.tif")

######### cc vs sd scatter plot
# mask out low change values
cc_sd <-c(cc_mw, gain_sd_dam3, loss_sd_dam3)

# convert to df
cc_sd_df <-as.data.frame(cc_sd, xy = TRUE, cells = TRUE)
colnames(cc_sd_df)[4:6] <-c("cc_mean", "gain_sd_dam3","loss_sd_dam3")
head(cc_sd_df)
# data.table::fwrite(cc_sd_df, "./csvs/cc_sd_df_dam3_41x41.csv")

cc_sd_df <-data.table::fread("./csvs/cc_sd_df_dam3_41x41.csv")

# quick hists
hist(cc_sd_df$cc_mean, breaks = 100)
hist(cc_sd_df$gain_sd_dam3, breaks = 100)
hist(cc_sd_df$loss_sd_dam3, breaks = 100)
max(cc_sd_df$gain_sd_dam3, na.rm = TRUE)
max(cc_sd_df$loss_sd_dam3, na.rm = TRUE)

# # filter cc of 0
# df_v2 <-dplyr::filter(cc_sd_df, cc_mean >= 0)
# hist(df_v2$cc_mean, breaks = 100)

# add bins col for box plot
df_v3 <-cc_sd_df %>%
  mutate(bin = cut_width(cc_mean, width = 5, boundary=0))

# cc scale
cc_scale <-colorRampPalette(c("#f7fcf5", "#00441b"))
cc_scale(13)


# starting plot
### gains
gains_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = gain_sd_dam3, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .6, varwidth = TRUE, outlier.size = .001, outlier.shape = 4, 
               outlier.colour = "grey80", outlier.alpha  = .01) +
  xlab("CC (%)") + ylab(expression(SWE~SD~(dam^3)))+ 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,25)) +
  scale_fill_discrete(type = cc_scale(10)) +
  theme_classic(10) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))
gains_plot

# loss
loss_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = loss_sd_dam3, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .6, varwidth = TRUE, outlier.size = .001, outlier.shape = 4,
               outlier.colour = "grey80", outlier.alpha  = .01) +
  xlab("CC (%)") + ylab(expression(SWE~SD~(dam^3)))+ 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_discrete(type = cc_scale(13)) +
  theme_classic(10) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))
loss_plot

# cowplot test
cow <-plot_grid(gains_plot, loss_plot,
                 labels = c("(a) SWE Gain", "(b) SWE Loss"),
                 nrow = 2, 
                 align = "hv",
                 label_size = 15,
                 vjust =  3,
                 hjust = -.7,
                 rel_heights = c(1/2,1/2))
# test save
# make tighter together
ggsave(cow,
       file = "./plots/swe_sd_bp_dam3_41x41_v1.png",
       width = 8, 
       height = 6,
       dpi = 300)

system("open ./plots/swe_sd_bp_dam3_41x41_v1.png")
        
