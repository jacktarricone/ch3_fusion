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
plotting_df <-fread("~/ch3_fusion/csvs/dswe_new_41_plotting.csv")
plotting_df$pair <-gsub("p","P",plotting_df$pair)
head(plotting_df)

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
sierra_sf <-st_geometry(sierra_v1)

############
### plot
############

# set scolor scale
# cc scale
swe_scale <-brewer.pal(9, "RdBu")

p <-ggplot(plotting_df) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_grid(vars(pair), vars(data_set), scales = "fixed") +
  scale_fill_gradientn(colors = swe_scale, limits = c(-3,3), oob = squish, na.value = "gray50", guide = "none") + 
  # annotate("text", x = -118.98, y = 37.87, label = label, size = 10) +
  labs(fill = expression(Delta~SWE~(m^3~10^5))) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(), 
        strip.position = c("top","left"),
        legend.box.spacing = unit(0, "pt"), 
        strip.text.y.left = element_text(angle = 0)) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 20,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 
p

# plot funciton
dswe_gain_no_scale <-function(data, label){
  
  p <-ggplot(gains_df) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = value)) + 
  facet_wrap( ~ data_set) +
  scale_fill_gradientn(colors = gains_scale, limits = c(0,150), oob = squish, na.value = "gray50", guide = "none") + 
  annotate("text", x = -118.98, y = 37.87, label = label, size = 10) +
  labs(fill = expression(SWE~Gain~(dam^3))) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
    

  return(p)
}

# plot funciton
dswe_gain_scale <-function(data, label){
  
  p <-ggplot(gains_df) +
    geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
    geom_raster(mapping = aes(x,y, fill = data)) + 
    scale_fill_gradientn(colors = gains_scale, limits = c(0,150), oob = squish, na.value = "gray50", guide = "none") + 
    labs(fill = expression(SWE~Gain~(dam^3)))+
    annotate("text", x = -118.98, y = 37.87, label = label, size = 10) +
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
                                 barwidth = 50,
                                 barheight = 2,
                                 frame.colour = "black", 
                                 ticks.colour = "black")) 
  return(p)
}

# make plots with no scale
ims_gp <-dswe_gain_no_scale(data = gains_df$ims, label = "IMS")
modscag_gp <-dswe_gain_no_scale(data = gains_df$modscag, label = "MODSCAG")
modis_gp <-dswe_gain_no_scale(data = gains_df$modis, label = "MODIS")
viirs_gp <-dswe_gain_no_scale(data = gains_df$viirs, label = "VIIRS")
flm_gp <-dswe_gain_no_scale(data = gains_df$flm, label = "FLM")

# last plot with scale
landsat_gp <-dswe_gain_scale(data = gains_df$landsat, label = "Landsat")
landsat_gp

# Create grid
gains_cow <-ggarrange(ims_gp,modscag_gp,modis_gp,viirs_gp,flm_gp,landsat_gp, # list of plots
                  labels = c("(a)", "(b)", "(c)","(d)", "(e)", "(f)"), # labels
                  common.legend = T, # COMMON LEGEND
                  legend = "bottom", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 6,
                  nrow = 1,
                  vjust =  2,
                  hjust = -.2,
                  font.label = list(size = 23, color = "black", face = "bold"))  

# test save
# make tighter together
ggsave(gains_cow,
       file = "./plots/dswe_gains_mw_dam2_v1.png",
       width = 18, 
       height = 10,
       dpi = 300)

system("open ./plots/dswe_gains_mw_dam2_v1.png") 

#################################
#################################
#################################
#           loss                #
#################################
#################################
#################################

min(loss_df$modscag, na.rm = T)
hist(loss_df$modscag)

# plot funciton
dswe_loss_no_scale <-function(data, label){
  
  p <-ggplot(loss_df) +
    geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
    geom_raster(mapping = aes(x,y, fill = data)) + 
    scale_fill_gradientn(colors = loss_scale, limits = c(-500,0), oob = squish, na.value = "gray50", guide = "none") + 
    annotate("text", x = -118.98, y = 37.87, label = label, size = 10) +
    labs(fill = expression(SWE~Loss~(dam^3))) +
    theme(panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm")) 
  
  return(p)
}

# plot funciton
dswe_loss_scale <-function(data, label){
  
  p <-ggplot(loss_df) +
    geom_sf(data = sierra_sf, fill = "gray50", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
    geom_raster(mapping = aes(x,y, fill = data)) + 
    scale_fill_gradientn(colors = loss_scale, limits = c(-500,0), oob = squish, na.value = "gray50", guide = "none") + 
    labs(fill = expression(SWE~loss~(dam^3)))+
    annotate("text", x = -118.98, y = 37.87, label = label, size = 10) +
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
                                 barwidth = 50,
                                 barheight = 2,
                                 frame.colour = "black", 
                                 ticks.colour = "black")) 
  return(p)
}

# make plots with no scale
ims_lp <-dswe_loss_no_scale(data = loss_df$ims, label = "")
modscag_lp <-dswe_loss_no_scale(data = loss_df$modscag, label = "")
modis_lp <-dswe_loss_no_scale(data = loss_df$modis, label = "")
viirs_lp <-dswe_loss_no_scale(data = loss_df$viirs, label = "")
flm_lp <-dswe_loss_no_scale(data = loss_df$flm, label = "")

# last plot with scale
landsat_lp <-dswe_loss_scale(data = loss_df$landsat, label = "")
landsat_lp

# Create grid
loss_cow <-ggarrange(ims_lp,modscag_lp,modis_lp,viirs_lp,flm_lp,landsat_lp, # list of plots
                      # labels = c("(a)", "(b)", "(c)","(d)", "(e)", "(f)"), # labels
                      common.legend = T, # COMMON LEGEND
                      legend = "bottom", # legend position
                      align = "hv", # Align them both, horizontal and vertical
                      ncol = 6,
                      nrow = 1,
                      vjust =  2,
                      hjust = -.2,
                      font.label = list(size = 23, color = "black", face = "bold"))  

# test save
# make tighter together
ggsave(loss_cow,
       file = "./plots/dswe_loss_mw_dam3_v1.png",
       width = 18, 
       height = 10,
       dpi = 300)

system("open ./plots/dswe_loss_mw_dam3_v1.png") 




# Create grid
full_cow <-ggarrange(gains_cow, loss_cow,
                     labels = c("SWE Gain", "SWE Loss"),
                     nrow = 2,
                     vjust =  18.5,
                     hjust = 0,
                     font.label = list(size = 50, color = "black", face = "bold"))
                    

# test save
# make tighter together
ggsave(full_cow,
       file = "./plots/dswe_mw_full_dam3_v1.png",
       width = 18, 
       height = 20,
       dpi = 300)

system("open ./plots/dswe_mw_full_dam3_v1.png") 


sd(c(-18.14,-18.87, -19.09,-18.65,-18.63))
mean(c(-15900,-15300, -15300, -15300,-15100))
sd(c(-15900,-15300, -15300, -15300,-15100))
mean(c(68,82,37,74))
