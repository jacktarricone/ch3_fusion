# plot 41 x 41 moving window rasters and 80 m dswe

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(viridis)
library(ggpubr)
library(data.table)
library(tidyr)

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

setwd("~/ch3_fusion/rasters")


# load in 80 m insar dswe products
p1_stack_cm <-rast(list.files("./new_dswe/p1", full.names = T))
names(p1_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
p2_stack_cm <-rast(list.files("./new_dswe/p2", full.names = T))
names(p2_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
p3_stack_cm <-rast(list.files("./new_dswe/p3", full.names = T))
names(p3_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
p4_stack_cm <-rast(list.files("./new_dswe/p4", full.names = T))
names(p4_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")

# create cell size raster in m^2
cell_size_m2 <- cellSize(p4_stack_cm, unit = "m")

# convert SWE cm to m^3
# p1_stack_m3 <-(p1_stack_cm/100 * cell_size_m2)
# p2_stack_m3 <-(p2_stack_cm/100 * cell_size_m2)
# p3_stack_m3 <-(p3_stack_cm/100 * cell_size_m2)
# p4_stack_m3 <-(p4_stack_cm/100 * cell_size_m2)

### format data.frames for plotting
p1_df <-as.data.frame(p1_stack_cm, xy = TRUE)
p1_df$pair <-rep("P1", nrow(p1_df))
p1_df_l <-pivot_longer(p1_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","STC","VIIRS"),
                       names_to = c("data_set"))
p1_df_l$data_set <-factor(p1_df_l$data_set, 
                               levels=c("IMS","MODIS","VIIRS","STC","Landsat","FLM"))

p2_df <-as.data.frame(p2_stack_cm, xy = TRUE)
p2_df$pair <-rep("P2", nrow(p2_df))
p2_df_l <-pivot_longer(p2_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","STC","VIIRS"),
                       names_to = c("data_set"))
p2_df_l$data_set <-factor(p2_df_l$data_set, 
                          levels=c("IMS","MODIS","VIIRS","STC","Landsat","FLM"))


p3_df <-as.data.frame(p3_stack_cm, xy = TRUE)
p3_df$pair <-rep("P3", nrow(p3_df))
p3_df_l <-pivot_longer(p3_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","STC","VIIRS"),
                       names_to = c("data_set"))
p3_df_l$data_set <-factor(p3_df_l$data_set, 
                          levels=c("IMS","MODIS","VIIRS","STC","Landsat","FLM"))

p4_df <-as.data.frame(p4_stack_cm, xy = TRUE)
p4_df$pair <-rep("P4", nrow(p4_df))
p4_df_l <-pivot_longer(p4_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","STC","VIIRS"),
                       names_to = c("data_set"))
p4_df_l$data_set <-factor(p4_df_l$data_set, 
                          levels=c("IMS","MODIS","VIIRS","STC","Landsat","FLM"))

# full <-bind_rows(p1_df_l,p2_df_l,p3_df_l,p4_df_l)
# head(full)
# unique(full$pair)
# hist(full$value, breaks = 100)
# fwrite(full, "~/ch3_fusion/csvs/dswe_80m_from_fig5.csv")

# bring in NA df
na_stack <-rast("./new_uavsar/na_pixels_stack.tif")
na_df <-as.data.frame(na_stack, xy = T)

# read in NA SHAPES
p1_na <-vect("~/ch3_fusion/rasters/new_uavsar/p1_na.gpkg")
p1_na_sf <-st_as_sf(p1_na)

p2_na <-vect("~/ch3_fusion/rasters/new_uavsar/p2_na.gpkg")
p2_na_sf <-st_as_sf(p2_na)

# read in NA SHAPES
p3_na <-vect("~/ch3_fusion/rasters/new_uavsar/p3_na.gpkg")
p3_na_sf <-st_as_sf(p3_na)

p4_na <-vect("~/ch3_fusion/rasters/new_uavsar/p4_na.gpkg")
p4_na_sf <-st_as_sf(p4_na)


# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
sierra_sf <-st_geometry(sierra_v1)

############
### plot
############

# set scolor scale
# cc scale
dswe_scale <-brewer.pal(9, "RdBu")


# p2
p1 <-ggplot(p1_df_l) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  geom_sf(data = p1_na_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_wrap(vars(data_set), scales = "fixed", dir = "h", strip.position = "top", nrow = 1) +
  scale_fill_gradientn(colors = dswe_scale, limits = c(-8,8), oob = squish, na.value = "gray50", guide = "none") + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~(m^3))) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # legend.position = element_blank(),
        plot.margin = unit(c(0,0,0,1), "cm"),
        strip.background = element_blank(), 
        strip.text = element_text(size = 13, face = "bold"))


# p2
p2 <-ggplot(p2_df_l) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  geom_sf(data = p2_na_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_wrap(vars(data_set), scales = "fixed", dir = "h", strip.position = "top", nrow = 1) +
  scale_fill_gradientn(colors = dswe_scale, limits = c(-8,8), oob = squish, na.value = "gray50", guide = "none") + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~(m^3))) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,1), "cm"),
        strip.background = element_blank(), 
        strip.text = element_blank())



# p3
p3 <-ggplot(p3_df_l) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .2, inherit.aes = FALSE, alpha = 1) +
  geom_sf(data = p3_na_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_wrap(vars(data_set), scales = "fixed", dir = "h", strip.position = "top", nrow = 1) +
  scale_fill_gradientn(colors = dswe_scale, limits = c(-8,8), oob = squish, na.value = "gray50", guide = "none") + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~(m^3))) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,1), "cm"),
        strip.background = element_blank(), 
        strip.text = element_blank())


# p4
p4 <-ggplot(p4_df_l) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_sf(data = p4_na_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_wrap(vars(data_set), scales = "fixed", dir = "h", strip.position = "top", nrow = 1) +
  scale_fill_gradientn(colors = dswe_scale, limits = c(-8,8), oob = squish, na.value = "gray50", guide = "none") + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~(cm))) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,1), "cm"),
        strip.background = element_blank(), 
        legend.box.spacing = unit(0, "pt"), 
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_blank()) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 27,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 


# stack with cow plot
dswe <-plot_grid(p1, p2, p3, p4,
          labels = c("P1","P2","P3","P4"),
          align = "v", 
          nrow = 4, 
          vjust = 10.5,
          rel_heights = c(.26,.23,.23,.33))


ggsave("~/ch3_fusion/plots/fig5_dswe_80m_v6.pdf",
       width = 7,
       height = 12,
       units = "in")

system("open ~/ch3_fusion/plots/fig5_dswe_80m_v6.pdf")

ggsave("~/ch3_fusion/plots/fig5_dswe_80m_v6.png",
       width = 7,
       height = 12,
       dpi = 300,
       units = "in")

system("open ~/ch3_fusion/plots/fig5_dswe_80m_v6.png")


