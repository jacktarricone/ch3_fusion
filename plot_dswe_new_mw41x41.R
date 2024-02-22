# plot 41 x 41 moving window rasters

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(viridis)
library(ggpubr)
library(data.table)

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

setwd("~/ch3_fusion/")

# load in 80 m insar dswe products
plotting_df <-fread("~/ch3_fusion/csvs/dswe_new_41_plotting_v3.csv")
plotting_df$value2 <-plotting_df$value/10e4
head(plotting_df)
hist(plotting_df$value2)

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
sierra_sf <-st_geometry(sierra_v1)

############
### plot
############

# set color scale
swe_scale <-brewer.pal(9, "RdBu")

p <-ggplot(plotting_df) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value2)) + 
  facet_grid(vars(pair), vars(data_set), scales = "fixed", switch = "y") +
  scale_fill_gradientn(colors = swe_scale, limits = c(-3,3), oob = squish, na.value = "gray50", guide = "none") + 
  labs(fill = expression(Delta~SWE~(10^4~m^3))) +
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
       file = "./plots/dswe_mw_v3.png",
       width = 7, 
       height = 12,
       dpi = 300)

system("open ./plots/dswe_mw_v3.png") 



#################################


