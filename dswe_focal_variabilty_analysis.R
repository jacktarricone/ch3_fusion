# dswe variabilty analysis
# june 13th, 2023
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

setwd("~/ch3_fusion")

# load in 80 m insar dswe products
modscag <-rast("./rasters/uavsar/dswe/modscag_dswe_v2.tif")
modis <-rast("./rasters/uavsar/dswe/modis_dswe_v2.tif")
viirs <-rast("./rasters/uavsar/dswe/viirs_dswe_v2.tif")
landsat <-rast("./rasters/uavsar/dswe/landsat_dswe_v2.tif")
flm <-rast("./rasters/uavsar/dswe/flm_dswe_v2.tif")
ims <-rast("./rasters/uavsar/dswe/ims_dswe_v2.tif")

# bring in cc, mask, and resample
cc_v2 <-rast("/Users/jacktarricone/ch3_fusion/rasters/geo_layers/cc_domain.tif")
sierra <-vect("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
cc_v1 <-mask(cc_v2, sierra)
cc <-resample(cc_v1, flm, method = 'bilinear')
plot(cc)

# calculate average cell area
cell_size_v1 <-cellSize(landsat, unit = "m")
cell_size_m2 <-as.numeric(global(cell_size_v1, 'max') + global(cell_size_v1, 'min'))/2

#### calculate SWE gains in cubic meters
modscag_gain <-ifel(modscag < 0, NA, modscag)
modscag_gain_m3 <-modscag_gain*cell_size_m2

modis_gain <-ifel(modis < 0, NA, modis)
modis_gain_m3 <-modis_gain*cell_size_m2

viirs_gain <-ifel(viirs < 0, NA, viirs)
viirs_gain_m3 <-viirs_gain*cell_size_m2

landsat_gain <-ifel(landsat < 0, NA, landsat)
landsat_gain_m3 <-landsat_gain*cell_size_m2

flm_gain <-ifel(flm < 0, NA, flm)
flm_gain_m3 <-flm_gain*cell_size_m2

ims_gain <-ifel(ims < 0, NA, ims)
ims_gain_m3 <-ims_gain*cell_size_m2


#### calculate SWE losss in cubic meters
modscag_loss <-ifel(modscag > 0, NA, modscag)
modscag_loss_m3 <-modscag_loss*cell_size_m2

modis_loss <-ifel(modis > 0, NA, modis)
modis_loss_m3 <-modis_loss*cell_size_m2

viirs_loss <-ifel(viirs > 0, NA, viirs)
viirs_loss_m3 <-viirs_loss*cell_size_m2

landsat_loss <-ifel(landsat > 0, NA, landsat)
landsat_loss_m3 <-landsat_loss*cell_size_m2

flm_loss <-ifel(flm > 0, NA, flm)
flm_loss_m3 <-flm_loss*cell_size_m2

ims_loss <-ifel(ims > 0, NA, ims)
ims_loss_m3 <-ims_loss*cell_size_m2

# sum gains using 11x11 moving window
modscag_gains_mw <-focal(modscag_gain_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(modscag_gains_mw)
# writeRaster(modscag_gains_mw, "./rasters/dswe_variabilty_analysis/modscag_gains_m3_11x11_v2.tif")

modis_gains_mw <-focal(modis_gain_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(modis_gains_mw)
# writeRaster(modis_gains_mw, "./rasters/dswe_variabilty_analysis/modis_gains_m3_11x11_v2.tif")

viirs_gains_mw <-focal(viirs_gain_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(viirs_gains_mw)
# writeRaster(viirs_gains_mw, "./rasters/dswe_variabilty_analysis/viirs_gains_m3_11x11_v2.tif")

landsat_gains_mw <-focal(landsat_gain_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(landsat_gains_mw)
# writeRaster(landsat_gains_mw, "./rasters/dswe_variabilty_analysis/landsat_gains_m3_11x11_v2.tif")

flm_gains_mw <-focal(flm_gain_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(flm_gains_mw)
# writeRaster(flm_gains_mw, "./rasters/dswe_variabilty_analysis/flm_gains_m3_11x11_v2.tif")

ims_gains_mw <-focal(ims_gain_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(ims_gains_mw)
# writeRaster(ims_gains_mw, "./rasters/dswe_variabilty_analysis/ims_gains_m3_11x11_v2.tif")

# sum loss using 11x11 moving window
modscag_loss_mw <-focal(modscag_loss_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(modscag_loss_mw)
# writeRaster(modscag_loss_mw, "./rasters/dswe_variabilty_analysis/modscag_loss_m3_11x11_v2.tif")

modis_loss_mw <-focal(modis_loss_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(modis_loss_mw)
# writeRaster(modis_loss_mw, "./rasters/dswe_variabilty_analysis/modis_loss_m3_11x11_v2.tif")

viirs_loss_mw <-focal(viirs_loss_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(viirs_loss_mw)
# writeRaster(viirs_loss_mw, "./rasters/dswe_variabilty_analysis/viirs_loss_m3_11x11_v2.tif")

landsat_loss_mw <-focal(landsat_loss_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(landsat_loss_mw)
# writeRaster(landsat_loss_mw, "./rasters/dswe_variabilty_analysis/landsat_loss_m3_11x11_v2.tif")

flm_loss_mw <-focal(flm_loss_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(flm_loss_mw)
# writeRaster(flm_loss_mw, "./rasters/dswe_variabilty_analysis/flm_loss_m3_11x11_v2.tif")

ims_loss_mw <-focal(ims_loss_m3, c(17,17), na.rm=TRUE, fun = "sum")
plot(ims_loss_mw)
# writeRaster(ims_loss_mw, "./rasters/dswe_variabilty_analysis/ims_loss_m3_11x11_v2.tif")

# stack gains and losses
gains_stack <-c(modscag_gains_mw, modis_gains_mw, viirs_gains_mw, landsat_gains_mw, flm_gains_mw)
loss_stack <-c(modscag_loss_mw, modis_loss_mw, viirs_loss_mw, landsat_loss_mw, flm_loss_mw)

plot(gains_stack[[1]])
plot(gains_stack[[4]])

# sd with na remove
sd_na_rm <-function(x){sd(x, na.rm = TRUE)}

# caculate pixelwise standard deviation
gains_sd_v1 <-app(gains_stack, fun = sd_na_rm)
gains_sd <-gains_sd_v1*10e-6
plot(gains_sd)
writeRaster(gains_sd, "./rasters/dswe_variabilty_analysis/gains_sd_17x17.tif")

loss_sd_v1 <-app(loss_stack, sd_na_rm)
loss_sd <-loss_sd_v1*10e-6
plot(loss_sd)
writeRaster(loss_sd, "./rasters/dswe_variabilty_analysis/loss_sd_17x17.tif", overwrite=T)

## cc focal
plot(cc)
cc_mw_v1 <-focal(cc, c(17,17), na.rm=TRUE, fun = "mean")
cc_mw <-mask(cc_mw_v1, gains_sd)
plot(cc_mw)
writeRaster(cc_mw, "./rasters/dswe_variabilty_analysis/cc_mw_mean_17x17.tif")

######### cc vs sd scatter plot
# mask out low change values
cc_sd <-c(cc_mw, gains_sd, loss_sd, landsat_gains_mw, modscag_gains_mw)

# convert to df
cc_sd_df <-as.data.frame(cc_sd, xy = TRUE, cells = TRUE)
colnames(cc_sd_df)[4:8] <-c("cc_mean", "gain_sd","loss_sd","landsat_gains","modscag_gains")
head(cc_sd_df)
# data.table::fwrite(cc_sd_df, "./csvs/cc_sd_df_17x17.csv")

# quick hists
hist(cc_sd_df$cc_mean, breaks = 100)
hist(cc_sd_df$gain_sd, breaks = 100)
hist(cc_sd_df$loss_sd, breaks = 100)
max(cc_sd_df$gain_sd, na.rm = TRUE)

# filter cc of 0
df_v2 <-dplyr::filter(cc_sd_df, cc_mean >= 0)
hist(df_v2$cc_mean, breaks = 100)

# add bins col for box plot
df_v3 <-df_v2 %>%
  mutate(bin = cut_width(cc_mean, width = 5, boundary=0))

# cc scale
cc_scale <-colorRampPalette(c("#f7fcf5", "#00441b"))
cc_scale(13)


# starting plot
### gains
gains_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = gain_sd, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .5, varwidth = TRUE, outlier.size = .001, outlier.shape = NA) +
  xlab("CC (%)") + ylab(expression(SWE~SD~(10^6~m^3)))+ 
  scale_x_continuous(limits = c(0,65), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,20)) +
  scale_fill_discrete(type = cc_scale(13)) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))
gains_plot

# loss
loss_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = loss_sd, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .5, varwidth = TRUE, outlier.size = .001, outlier.shape = NA) +
  xlab("CC (%)") + ylab(expression(SWE ~SD~(10^6~m^3)))+ 
  scale_x_continuous(limits = c(0,65), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,50)) +
  scale_fill_discrete(type = cc_scale(13)) +
  theme_classic(14) +
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
       file = "./plots/swe_sd_bp_17x17.png",
       width = 8, 
       height = 6,
       dpi = 300)

system("open ./plots/swe_sd_bp_17x17.png")
        