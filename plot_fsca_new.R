# plot moving window dswe values

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
p1_stack <-rast(list.files("./new_optical/p1_80m_20200131_20200212", pattern = "_80m", full.names = T))
names(p1_stack) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")
p2_stack <-rast(list.files("./new_optical/p2_80m_20200212_20200219/", pattern = "_80m", full.names = T))
names(p2_stack) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")
p3_stack <-rast(list.files("./new_optical/p3_80m_20200219_20200226/", pattern = "_80m", full.names = T))
names(p3_stack) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")
p4_stack <-rast(list.files("./new_optical/p4_80m_20200226_20200311/", pattern = "_80m", full.names = T))
names(p4_stack) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")

### format data.frames for plotting
p1_df <-as.data.frame(p1_stack, xy = TRUE)
p1_df$pair <-rep("P1", nrow(p1_df))
p1_df_l <-pivot_longer(p1_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p2_df <-as.data.frame(p2_stack, xy = TRUE)
p2_df$pair <-rep("P2", nrow(p2_df))
p2_df_l <-pivot_longer(p2_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p3_df <-as.data.frame(p3_stack, xy = TRUE)
p3_df$pair <-rep("P3", nrow(p3_df))
p3_df_l <-pivot_longer(p3_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p4_df <-as.data.frame(p4_stack, xy = TRUE)
p4_df$pair <-rep("P4", nrow(p4_df))
p4_df_l <-pivot_longer(p4_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))


# bind for plotting
plotting_df <-rbind(p1_df_l,p2_df_l,p3_df_l,p4_df_l)
plotting_50 <-plotting_df
plotting_50$value <-ifelse(plotting_50$value < 50, NA, plotting_50$value)
plotting_50$data_set <-factor(plotting_50$data_set, 
                          levels=c("IMS","MODIS","VIIRS","MODSCAG","Landsat","FLM"))

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
sierra_sf <-st_geometry(sierra_v1)

############
### plot
############

# set scolor scale
# cc scale
fsca_scale <-c("gray50",viridis(10, option = "G", direction = 1))

p <-ggplot(plotting_df) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_grid(vars(pair), vars(data_set), scales = "fixed", switch = "y") +
  scale_fill_gradientn(colors = fsca_scale, limits = c(15,100), oob = squish, na.value = "gray50", guide = "none") + # max of color bar so it saturates
  labs(fill = "fSCA (%)") +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(), 
        legend.box.spacing = unit(0, "pt"), 
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(size = 13, face = "bold")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 27,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

ggsave(p,
       file = "./plots/fcsa_plot_v2.png",
       width = 7, 
       height = 12,
       dpi = 300)

system("open ./plots/fcsa_plot_v2.png") 



#################################
# mask for 50 

p50 <-ggplot(plotting_50) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_grid(vars(pair), vars(data_set), scales = "fixed", switch = "y") +
  scale_fill_gradientn(colors = fsca_scale, limits = c(15,100), oob = squish, na.value = "gray50", guide = "none") + # max of color bar so it saturates
  labs(fill = "fSCA (%)") +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(), 
        legend.box.spacing = unit(0, "pt"), 
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(size = 13, face = "bold")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 27,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

ggsave(p50,
       file = "~/ch3_fusion/plots/fig4_fcsa_50mask_plot_v3.pdf",
       width = 7, 
       height = 12)

system("open ~/ch3_fusion/plots/fig4_fcsa_50mask_plot_v3.pdf") 
