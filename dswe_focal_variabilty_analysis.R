# dswe variabilty analysis
# june 13th, 2023
# jack tarricone

library(terra)
library(ggplot2)
library(tidyverse)

setwd("~/ch3_fusion")

# load in 80 m insar dswe products
modscag <-rast("./rasters/uavsar/feb26_march11_80m/modscag_dswe.tif")
modis <-rast("./rasters/uavsar/feb26_march11_80m/modis_dswe.tif")
viirs <-rast("./rasters/uavsar/feb26_march11_80m/viirs_dswe.tif")
landsat <-rast("./rasters/uavsar/feb26_march11_80m/landsat_dswe.tif")
flm <-rast("./rasters/uavsar/feb26_march11_80m/flm_dswe.tif")

# bring in cc, mask, and resample
cc_v2 <-rast("/Users/jacktarricone/ch3_fusion/rasters/geo_layers/cc_domain.tif")
sierra <-vect("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
cc_v1 <-mask(cc_v2, sierra)
cc <-resample(cc_v1, flm, method = 'bilinear')
plot(cc)

# calculate average cell area
cell_size_v1 <-cellSize(landsat_gain, unit = "m")
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

# sum gains using 11x11 moving window
modscag_gains_mw <-focal(modscag_gain_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(modscag_gains_mw)
writeRaster(modscag_gains_mw, "./rasters/dswe_variabilty_analysis/modscag_gains_m3_11x11.tif")

modis_gains_mw <-focal(modis_gain_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(modis_gains_mw)
writeRaster(modis_gains_mw, "./rasters/dswe_variabilty_analysis/modis_gains_m3_11x11.tif")

viirs_gains_mw <-focal(viirs_gain_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(viirs_gains_mw)
writeRaster(viirs_gains_mw, "./rasters/dswe_variabilty_analysis/viirs_gains_m3_11x11.tif")

landsat_gains_mw <-focal(landsat_gain_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(landsat_gains_mw)
writeRaster(landsat_gains_mw, "./rasters/dswe_variabilty_analysis/landsat_gains_m3_11x11.tif")

flm_gains_mw <-focal(flm_gain_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(flm_gains_mw)
writeRaster(flm_gains_mw, "./rasters/dswe_variabilty_analysis/flm_gains_m3_11x11.tif")

# sum loss using 11x11 moving window
modscag_loss_mw <-focal(modscag_loss_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(modscag_loss_mw)
writeRaster(modscag_loss_mw, "./rasters/dswe_variabilty_analysis/modscag_loss_m3_11x11.tif")

modis_loss_mw <-focal(modis_loss_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(modis_loss_mw)
writeRaster(modis_loss_mw, "./rasters/dswe_variabilty_analysis/modis_loss_m3_11x11.tif")

viirs_loss_mw <-focal(viirs_loss_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(viirs_loss_mw)
writeRaster(viirs_loss_mw, "./rasters/dswe_variabilty_analysis/viirs_loss_m3_11x11.tif")

landsat_loss_mw <-focal(landsat_loss_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(landsat_loss_mw)
writeRaster(landsat_loss_mw, "./rasters/dswe_variabilty_analysis/landsat_loss_m3_11x11.tif")

flm_loss_mw <-focal(flm_loss_m3, c(11,11), na.rm=TRUE, fun = "sum")
plot(flm_loss_mw)
writeRaster(flm_loss_mw, "./rasters/dswe_variabilty_analysis/flm_loss_m3_11x11.tif")

# stack gains and losses
gains_stack <-c(modscag_gains_mw, modis_gains_mw, viirs_gains_mw, landsat_gains_mw, flm_gains_mw)
loss_stack <-c(modscag_loss_mw, modis_loss_mw, viirs_loss_mw, landsat_loss_mw, flm_loss_mw)

plot(gains_stack[[1]])
plot(gains_stack[[4]])

# caculate pixelwise standard deviation
gains_sd_v1 <-app(gains_stack, fun = sd)
gains_sd <-gains_sd_v1*10e-6
plot(gains_sd)
writeRaster(gains_sd, "./rasters/dswe_variabilty_analysis/gains_sd.tif")

loss_sd_v1 <-app(loss_stack, sd)
loss_sd <-loss_sd_v1*10e-6
plot(loss_sd)
writeRaster(loss_sd, "./rasters/dswe_variabilty_analysis/loss_sd.tif")

## cc focal
plot(cc)
cc_mw_v1 <-focal(cc, c(11,11), na.rm=TRUE, fun = "mean")
cc_mw <-mask(cc_mw_v1, gains_sd)
plot(cc_mw)
writeRaster(cc_mw, "./rasters/dswe_variabilty_analysis/cc_mw_mean_11x11_v1.tif")

######### cc vs sd scatter plot
# mask out low change values
cc_sd <-c(cc_mw, gains_sd, loss_sd, landsat_gains_mw, modscag_gains_mw)

# convert to df
cc_sd_df <-as.data.frame(cc_sd, xy = TRUE, cells = TRUE)
colnames(cc_sd_df)[4:8] <-c("cc_mean", "gain_sd","loss_sd","landsat_gains","modscag_gains")
head(cc_sd_df)

# quick hists
hist(cc_sd_df$cc_mean, breaks = 100)
hist(cc_sd_df$gain_sd, breaks = 100)
hist(cc_sd_df$loss_sd, breaks = 100)
max(cc_sd_df$gain_sd, na.rm = TRUE)

# filter cc of 0
df_v2 <-dplyr::filter(cc_sd_df, cc_mean >= 1)
hist(cc_sd_df_v2$cc_mean, breaks = 100)


# add bins col for box plot
df_v3 <-df_v2 %>%
  mutate(bin = cut_width(cc_mean, width = 5, boundary=0))


## plot
scale1 <-c("grey99",viridisLite::viridis(30, option = "H", direction = 1))

## density
ggplot(df_v3, aes(x = cc_mean, y = gain_sd, fill = ..density..)) +
  geom_bin2d(bins = 50) +
  # geom_smooth(method = "lm", se = FALSE) +
  scale_fill_gradientn(colors = scale1) +
  # scale_y_continuous(limits = c(0,10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# starting plot
ggplot(df_v3, mapping = aes(x = cc_mean, y = gain_sd, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .3, width = .2, outlier.size = .01, outlier.shape = 1) +
  xlab("CC (%)") + ylab(expression(SWE~Gain~(10^6~m^3)))+ 
  # scale_y_continuous(limits = c(0,1)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.margin=margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))


        