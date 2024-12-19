# dswe variabilty analysis
# june 13th, 2023
# jack tarricone

library(terra)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(sf)
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

theme_set(theme_classic(22))

setwd("~/ch3_fusion/rasters")

# load in 80 m insar dswe products
# without landsat
p1_list <-list.files("./dswe_variabilty_analysis/p1", pattern = ".tif", full.names = T)
p1_stack <-rast(p1_list[-3])
p2_list <-list.files("./dswe_variabilty_analysis/p2", pattern = ".tif", full.names = T)
p2_stack <-rast(p2_list[-3])
p3_list <-list.files("./dswe_variabilty_analysis/p3", pattern = ".tif", full.names = T)
p3_stack <-rast(p3_list[-3])
p4_list <-list.files("./dswe_variabilty_analysis/p4", pattern = ".tif", full.names = T)
p4_stack <-rast(p4_list[-3])

# bring in cc, mask, and resample
cc_v2 <-rast("~/ch3_fusion/rasters/geo_layers/cc_domain.tif")
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
cc_v1 <-mask(cc_v2, sierra)
cc <-resample(cc_v1, p4_stack, method = 'bilinear')
cc_mw <-focal(cc, c(41,41), na.rm=TRUE, fun = "mean")
cc_df <-as.data.frame(cc_mw, xy = TRUE)
head(cc_df)

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
sierra_sf <-st_geometry(sierra_v1)

####################
###################
# define sd with na remove
iqr_na_rm <-function(x){IQR(x, na.rm = TRUE)}

# calculate pixelwise standard deviation
p1_iqr <-app(p1_stack, fun = iqr_na_rm)
p1_df <-as.data.frame(p1_iqr, xy = TRUE)
p1_df$pair <-rep("(a) P1", nrow(p1_df))

p2_iqr <-app(p2_stack, fun = iqr_na_rm)
p2_df <-as.data.frame(p2_iqr, xy = TRUE)
p2_df$pair <-rep("(b) P2", nrow(p2_df))

p3_iqr <-app(p3_stack, fun = iqr_na_rm)
p3_df <-as.data.frame(p3_iqr, xy = TRUE)
p3_df$pair <-rep("(c) P3", nrow(p3_df))

p4_iqr <-app(p4_stack, fun = iqr_na_rm)
p4_df <-as.data.frame(p4_iqr, xy = TRUE)
p4_df$pair <-rep("(d) P4", nrow(p4_df))


# bind for facet plotting
plotting_df <-rbind(p1_df,p2_df,p3_df,p4_df)
hist(plotting_df$lyr.1)
max(plotting_df$lyr.1)

# set scolor scale
scale1 <-c(viridis(10, option = "H", direction = 1))

#############
#### plot four pairs SD rasters
#############

p1_p <-ggplot(plotting_df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = lyr.1)) + 
  facet_wrap(vars(pair), scales = "fixed", dir = "h", strip.position = "top", nrow = 1) +
  scale_fill_gradientn(colors = scale1, limits = c(0,150), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(IQR~(m^3))) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(),
        strip.text = element_text(size = 25)) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 40,
                               barheight = 1.5,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# save
ggsave(p1_p,
       file = "~/ch3_fusion/plots/test_sd1.pdf",
       width = 8, 
       height = 7)

system("open ~/ch3_fusion/plots/test_sd1.pdf")



# set scale 
cc_scale <-c(brewer.pal(9, 'YlGn'))

cc <-ggplot(cc_df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = focal_mean)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.85, label = "(e) CC", size = 9) +
  scale_fill_gradientn(colors = cc_scale, limits = c(0,60), oob = squish) + # max of color bar so it saturates
  labs(fill = "CC (%)") +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,.5,0), "cm"),
        legend.box.spacing = unit(13, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 10,
                               barheight = 1.5,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# save
ggsave(cc,
       file = "~/ch3_fusion/plots/cc_map_41x41_v3.png",
       width = 3.5, 
       height = 8)

system("open ~/ch3_fusion/plots/cc_map_41x41_v2.png")


# cowplot test
cow <-plot_grid(p1_p, cc,
                ncol = 2, 
                align = "hv",
                rel_widths = c(4/5, .25))
# test save
# make tighter together
ggsave(cow,
       file = "~/ch3_fusion/plots/fig7_sd_vs_cc_plot_v12.pdf",
       width = 13, 
       height = 9)

system("open ~/ch3_fusion/plots/fig7_sd_vs_cc_plot_v12.pdf")

