# plot uavsar fsca data
# june 25th, 2023
# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(viridis)
library(ggpubr)

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

theme_set(theme_classic(23))

setwd("~/ch3_fusion/rasters")

# load in 80 m insar dswe products
## bring in inc
unw <-rast("./new_uavsar/p3_80m/p3_7d_VV_unw_80m.tif")
cor <-rast("./new_uavsar/p3_80m/p3_7d_VV_coh_80m.tif")
inc <-rast("./new_uavsar/inc_80m.tif")

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
sierra_sf <-st_geometry(sierra_v1)

# make df
unw_df <-as.data.frame(unw, xy = TRUE, cell = TRUE)
colnames(unw_df)[4] <- "phase"
cor_df <-as.data.frame(cor, xy = TRUE, cell = TRUE)
colnames(cor_df)[4] <- "cor"
inc_df <-as.data.frame(inc, xy = TRUE, cell = TRUE)
colnames(inc_df)[4] <- "rad"
head(unw_df)
head(cor_df)
head(inc_df)
hist(unw)



# set scolor scale
unw_scale <-viridis(9, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
inc_scale <-brewer.pal(9, "Spectral")
cor_scale <-rev(brewer.pal(9, "Greys"))

# phase
unw_p <-ggplot(unw_df) +
      geom_sf(data = sierra_sf, fill = "black", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
      geom_raster(mapping = aes(x,y, fill = phase)) + 
      scale_fill_gradientn(colors = unw_scale, limits = c(-2,2), oob = squish, na.value = "black", guide = "none") + 
      geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
      #geom_sf(data = nans_sf, fill = "black", color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
      labs(fill = "Upwrapped Phase (rad)")+
      # annotate("text", x = -118.98, y = 37.87, label = "(a)", size = 10) +
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
                                 title.hjust = -.5,
                                 barwidth = 11,
                                 barheight = 1.5,
                                 frame.colour = "black", 
                                 ticks.colour = "black")) 



inc_p <-ggplot(inc_df) +
  geom_sf(data = sierra_sf, fill = "black", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = rad)) + 
  scale_fill_gradientn(colors = inc_scale, limits = c(0,2), oob = squish, na.value = "black", guide = "none") + 
  geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  #geom_sf(data = nans_sf, fill = "black", color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  labs(fill = "Incidence Angle (rad)")+
  # annotate("text", x = -118.98, y = 37.87, label = "(a)", size = 10) +
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
                               title.hjust = -.5,
                               barwidth = 11,
                               barheight = 1.5,
                               frame.colour = "black", 
                               ticks.colour = "black")) 


cor_p <-ggplot(cor_df) +
  geom_sf(data = sierra_sf, fill = "black", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = cor)) + 
  scale_fill_gradientn(colors = cor_scale, limits = c(0,1), oob = squish, na.value = "black", guide = "none") + 
  geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  #geom_sf(data = nans_sf, fill = "black", color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  labs(fill = "Coherence")+
  # annotate("text", x = -118.98, y = 37.87, label = "(a)", size = 10) +
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
                               title.hjust = -.5,
                               barwidth = 11,
                               barheight = 1.5,
                               frame.colour = "black", 
                               ticks.colour = "black")) 




# Create grid
cow <-plot_grid(unw_p,cor_p,inc_p, # list of plots
                    labels = c("(a)", "(b)", "(c)"),
                    ncol = 3, 
                    align = "v",
                    label_size = 23,
                    vjust =  1.5,
                    hjust = -.2,
                    rel_widths = c(1/3,1/3,1/3))

# test save
# make tighter together
ggsave(cow,
       file = "~/ch3_fusion/plots/uavsar_data_agu.png",
       width = 8.5, 
       height = 9,
       dpi = 300)

system("open ~/ch3_fusion/plots/uavsar_data_agu.png") 
