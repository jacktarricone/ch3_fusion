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

theme_set(theme_classic(16))

setwd("~/ch3_fusion/rasters")

# load in 80 m insar dswe products
p1_stack <-rast(list.files("./dswe_variabilty_analysis/p1", pattern = ".tif", full.names = T))
p2_stack <-rast(list.files("./dswe_variabilty_analysis/p2", pattern = ".tif", full.names = T))
p3_stack <-rast(list.files("./dswe_variabilty_analysis/p3", pattern = ".tif", full.names = T))
p4_stack <-rast(list.files("./dswe_variabilty_analysis/p4", pattern = ".tif", full.names = T))


# bring in cc, mask, and resample
cc_v2 <-rast("~/ch3_fusion/rasters/geo_layers/cc_domain.tif")
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
cc_v1 <-mask(cc_v2, sierra)
cc <-resample(cc_v1, p4_stack, method = 'bilinear')
cc_mw <-focal(cc, c(41,41), na.rm=TRUE, fun = "mean")
cc_df <-as.data.frame(cc_mw, xy = TRUE)
head(cc_df)

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
sierra_sf <-st_geometry(sierra_v1)

####################
###################
# define sd with na remove
sd_na_rm <-function(x){sd(x, na.rm = TRUE)}

# calculate pixelwise standard deviation
p1_sd <-app(p1_stack, fun = sd_na_rm)/1e5
hist(p1_sd,breaks = 100)
p1_df <-as.data.frame(p1_sd, xy = TRUE)
plot(p1_sd)

p2_sd <-app(p2_stack, fun = sd_na_rm)/1e5
p2_df <-as.data.frame(p2_sd, xy = TRUE)
plot(p2_sd)

p3_sd <-app(p3_stack, fun = sd_na_rm)/1e5
p3_df <-as.data.frame(p3_sd, xy = TRUE)
plot(p3_sd)

p4_sd <-app(p4_stack, fun = sd_na_rm)/1e5
p4_df <-as.data.frame(p4_sd, xy = TRUE)
plot(p4_sd)

head(p1_df)
hist(p1_sd, breaks = 100)

# set scolor scale
scale1 <-c(viridis(10, option = "H", direction = 1))

#############
#### p1 #####
#############

p1_p <-ggplot(p1_df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = lyr.1)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "P1", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,2), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~SD~(m^3~10^5))) +
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
                               barwidth = 30,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# save
ggsave(p1_p,
       file = "~/ch3_fusion/plots/p1_sd.png",
       width = 3.5, 
       height = 8,
       dpi = 300)

system("open ~/ch3_fusion/plots/p1_sd.png")


#############
#### p2 #####
#############


p2_p <-ggplot(p2_df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = lyr.1)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "P2", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,2), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~SD~(m^3~10^5))) +
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

# save
ggsave(p2_p,
       file = "~/ch3_fusion/plots/p2_sd.png",
       width = 3.5, 
       height = 8,
       dpi = 300)

system("open ~/ch3_fusion/plots/p2_sd.png")

#############
#### p3 #####
#############

p3_p <-ggplot(p3_df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = lyr.1)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "P3", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,2), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~SD~(m^3~10^5))) +
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

# save
ggsave(p3_p,
       file = "~/ch3_fusion/plots/p3_sd.png",
       width = 3.5, 
       height = 8,
       dpi = 300)

system("open ~/ch3_fusion/plots/p3_sd.png")



#############
#### p4 #####
#############

p4_p <-ggplot(p4_df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = lyr.1)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "P4", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,2), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~SD~(m^3~10^5))) +
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
                               barwidth = 30,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# save
ggsave(p4_p,
       file = "~/ch3_fusion/plots/p4_sd.png",
       width = 3.5, 
       height = 8,
       dpi = 300)

system("open ~/ch3_fusion/plots/p4_sd.png")



#### loss
# set scale 
cc_scale <-c(brewer.pal(9, 'YlGn'))

cc <-ggplot(cc_df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = focal_mean)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "CC", size = 10) +
  scale_fill_gradientn(colors = cc_scale, limits = c(0,60), oob = squish) + # max of color bar so it saturates
  labs(fill = "CC (%)") +
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

# save
ggsave(cc,
       file = "~/ch3_fusion/plots/cc_map_41x41.png",
       width = 3.5, 
       height = 8)

system("open ~/ch3_fusion/plots/cc_map_41x41.png")


# cowplot test
cow <-plot_grid(p1_p, p2_p, p3_p, p4_p, cc,
                labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
                ncol = 5, 
                align = "hv",
                label_size = 22,
                vjust =  2,
                hjust = -.2,
                rel_widths = c(1/5, 1/5, 1/5, 1/5, 1/5))
# test save
# make tighter together
ggsave(cow,
       file = "~/ch3_fusion/plots/sd_vs_cc_plot_v1.png",
       width = 12.5, 
       height = 9,
       dpi = 300)

system("open ~/ch3_fusion/plots/sd_vs_cc_plot_v1.png")

## try ggarrange
sd <-ggarrange(p1_p, p2_p, p3_p, p4_p, # list of plots
                labels = c("(a)", "(b)", "(c)","(d)"), # labels
                common.legend = T, # COMMON LEGEND
                legend = "bottom", # legend position
                align = "hv", # Align them both, horizontal and vertical
                ncol = 4,
                nrow = 1,
                vjust =  2,
                hjust = -.2,
                font.label = list(size = 23, color = "black", face = "bold"))  

# test save
# make tighter together
ggsave(sd,
       file = "~/ch3_fusion/plots/sd_vs_cc_plot_v2.png",
       width = 10, 
       height = 9,
       dpi = 300)

system("open ~/ch3_fusion/plots/sd_vs_cc_plot_v2.png")

# cowplot test
sd_cc <-plot_grid(sd,cc,
                labels = c("", "(e)"),
                ncol = 2, 
                align = "v",
                label_size = 22,
                vjust =  2,
                hjust = -.2,
                rel_widths = c(4/5, 1/5))
# test save
# make tighter together
ggsave(sd_cc,
       file = "~/ch3_fusion/plots/sd_vs_cc_plot_v3.png",
       width = 12.5, 
       height = 9,
       dpi = 300)

system("open ~/ch3_fusion/plots/sd_vs_cc_plot_v3.png")
