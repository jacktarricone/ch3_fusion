# plot uavsar fsca data
# june 17th, 2023
# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(viridis)

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

theme_set(theme_classic(17))

setwd("~/ch3_fusion")

# load in 80 m insar dswe products
modscag <-rast("./rasters/uavsar/data_80m_0226_0311/modscag_sierra_80m.tif")
modis <-rast("./rasters/uavsar/data_80m_0226_0311/modis_sierra_80m.tif")
viirs <-rast("./rasters/uavsar/data_80m_0226_0311/viirs_sierra_80m.tif")
landsat <-rast("./rasters/uavsar/data_80m_0226_0311/landsat_sierra_80m.tif")
flm <-rast("./rasters/uavsar/data_80m_0226_0311/flm_sierra_80m.tif")
ims <-rast("./rasters/uavsar/data_80m_0226_0311/ims_sierra_80m.tif")

# read in sierra shp
sierra_v1 <-st_read("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
sierra_sf <-st_geometry(sierra_v1)

# stack
stack <-c(modscag,modis, viirs,landsat,flm,ims)
names(stack) <-c("modscag","modis","viirs","landsat","flm","ims")

# make df
stack_df <-as.data.frame(stack, xy = TRUE, cell = TRUE)

############
### modis
############

# set scolor scale
scale1 <-c("gray50",viridis(10, option = "G", direction = 1))

# plot funciton
fsca_plot_no_scale <-function(data, label){
  
  p <-ggplot(stack_df) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = data)) + 
  geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = label, size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(15,100), oob = squish, na.value = "gray50") + # max of color bar so it saturates
  labs(fill = "fSCA (%)") +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 11,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 
  return(p)
}


# make
ims_p <-fsca_plot(data = stack_df$ims, label = "IMS")
modscag_p <-fsca_plot(data = stack_df$modscag, label = "MODSCAG")
modis_p <-fsca_plot(data = stack_df$modis, label = "MODIS")
viirs_p <-fsca_plot(data = stack_df$viirs, label = "VIIRS")
flm_p <-fsca_plot(data = stack_df$flm, label = "FLM")
landsat_p <-fsca_plot(data = stack_df$landsat, label = "Landsat")
ims_p

# plot
modis_p <-ggplot(stack_df) +
  geom_sf(data = sierra_sf, fill = "black", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = modis)) + 
  geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "MODIS", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,100), oob = squish, na.value = "black") + # max of color bar so it saturates
  labs(fill = "fSCA (%)") +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 11,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 
modis_p
