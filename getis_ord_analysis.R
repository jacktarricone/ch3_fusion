# getis-ord test

library(terra)
library(raster)
remotes::install_github("biggis-project/soh")
library(soh)
library(ggplot2)

setwd("~/ch3_fusion")

# load in 80 m insar dswe products
flm <-rast("./rasters/uavsar/feb26_march11_80m/flm_dswe.tif")
modscag <-rast("./rasters/uavsar/feb26_march11_80m/modscag_dswe.tif")
modis <-rast("./rasters/uavsar/feb26_march11_80m/modis_dswe.tif")
viirs <-rast("./rasters/uavsar/feb26_march11_80m/viirs_dswe.tif")
landsat <-rast("./rasters/uavsar/feb26_march11_80m/landsat_dswe.tif")
cc_v2 <-rast("/Users/jacktarricone/ch3_fusion/rasters/geo_layers/cc_domain.tif")
plot(cc_v2)
sierra <-vect("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
cc_v1 <-mask(cc_v2, sierra)
cc <-resample(cc_v1, flm, method = 'bilinear')
plot(cc)

# stack and inspect
stack <-c(flm,modscag,modis,viirs,landsat)
plot(stack)
x <- focal3D(stack, c(5,5,5), na.rm=TRUE, fun = "sd")
plot(x)
writeRaster(x, "./rasters/focal_3d_uavsar_5x5_v2.tif")


# summing analysis
landsat_gain <-ifel(landsat < 0, NA, landsat)
plot(landsat_gain)

landsat_loss <-ifel(landsat > 0, NA, landsat)
plot(landsat_loss)

landsat_gains_sum <-focal(landsat_gain, c(11,11), na.rm=TRUE, fun = "sum")
plot(landsat_gains_sum)

loss_sum <- focal(landsat_loss, c(11,11), na.rm=TRUE, fun = "sum")
plot(loss_sum)
plot(gains_sum, add = TRUE)


# summing analysis
flm_gain <-ifel(flm < 0, NA, flm)
plot(flm_gain)

flm_loss <-ifel(flm > 0, NA, flm)
plot(flm_loss)

flm_gains_sum <- focal(flm_gain, c(11,11), na.rm=TRUE, fun = "sum")
plot(flm_gains_sum)
writeRaster(flm_gains_sum, "./rasters/flm_gains_sum_v1.tif")
# writeRaster(landsat_gains_sum, "./rasters/landsat_gains_sum_v2.tif")
diff <-flm_gains_sum - landsat_gains_sum
plot(diff)
writeRaster(diff, "./rasters/diff_flm_landsat_sum_v1.tif")

## cc focal
cc_mean <- focal(flm_gain, c(11,11), na.rm=TRUE, fun = "mean")
plot(cc_mean)
writeRaster(cc_mean, "./rasters/cc_mean_11x11_v1.tif")

# mask out low change values
diff_masked <-ifel(diff > -30 & diff < 30 , NA, diff)
plot(diff_masked)

cc_mean_v2 <-mask(cc_mean, diff_masked)
plot(cc_mean_v2)

cc_diff_stack <-c(diff_masked, cc_mean_v2)
cc_diff_df <-as.data.frame(cc_diff_stack, xy = TRUE, cells = TRUE)
colnames(cc_diff_df)[4:5] <-c("dswe_sum", "cc_mean")
head(cc_diff_df)
hist(cc_diff_df$dswe_sum, breaks = 100)
hist(cc_diff_df$cc_mean, breaks = 100)

ggplot(cc_diff_df, aes(x = dswe_sum, y = cc_mean)) +
  geom_point(size = (1/100), alpha = .05, color = 'darkred') +
  scale_y_continuous(limits = c(0,10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")


hist(modis_gains_sum, breaks = 30)
hist(landsat_gains_sum, breaks = 30, add = TRUE, col = 'red')
global(modis_gains_sum, mean, na.rm = TRUE)
global(landsat_gains_sum, mean, na.rm = TRUE)

loss_sum <- focal(modis_loss, c(3,3), na.rm=TRUE, fun = "sum")
plot(loss_sum)
plot(gains_sum, add = TRUE)







r <- raster(stack[[5]])
w <- weight_matrix_circular_fade(3, 3)
st <- GetisOrdStandardStats(r)
plot(GetisOrd(r, w, st))

autocor(mat, w=matrix(c(3,3)), method="gi*", global=TRUE)
        