# plot swe change sd rasters
# jack tarricone
# june 13th

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

# read in plotting df
df <-data.table::fread("./csvs/cc_sd_df_17x17.csv")
hist(df$loss_sd, breaks = 100)


# read in sierra shp
sierra_v1 <-st_read("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
sierra_sf <-st_geometry(sierra_v1)

############
### swe gain SD
############

# set scolor scale
scale1 <-c(viridis(10, option = "H", direction = 1))

# plot
sd_gain <-ggplot(df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = gain_sd)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "SWE Gain", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,10), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~SD~(10^6~m^3))) +
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
ggsave(sd_gain,
       file = "./plots/sd_gain_map_17x17.png",
       width = 3.5, 
       height = 8,
       dpi = 300)

system("open ./plots/sd_gain_map_17x17.png")

#### loss
sd_loss <-ggplot(df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = loss_sd)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "SWE Loss", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,20), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~SD~(10^6~m^3))) +
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
sd_loss
# save
ggsave(sd_loss,
       file = "./plots/sd_loss_map_17x17.png",
       width = 3.5, 
       height = 8)

system("open ./plots/sd_loss_map_17x17.png")

#### loss
# set scale 
cc_scale <-c(brewer.pal(9, 'YlGn'))

cc <-ggplot(df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = cc_mean)) + 
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
       file = "./plots/cc_map_17x17.pdf",
       width = 3.5, 
       height = 8)

system("open ./plots/cc_map_17x17.pdf")


# cowplot test
cow <-plot_grid(sd_gain, sd_loss, cc,
                 labels = c("(a)", "(b)", "(c)"),
                 ncol = 3, 
                 align = "hv",
                 label_size = 22,
                 vjust =  2,
                 hjust = -.2,
                 rel_widths = c(1/3, 1/3, 1/3))
# test save
# make tighter together
ggsave(cow,
       file = "./plots/sd_vs_cc_map_17x17.png",
       width = 9, 
       height = 9,
       dpi = 300)

system("open ./plots/sd_vs_cc_map_17x17.png")
