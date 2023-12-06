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
plot(cc)
cc

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
sierra_sf <-st_geometry(sierra_v1)

####################
###################
# define sd with na remove
sd_na_rm <-function(x){sd(x, na.rm = TRUE)}

# calculate pixelwise standard deviation
p1_sd <-app(p1_stack, fun = sd_na_rm)/10e4
p1_df <-as.data.frame(p1_sd, xy = TRUE)
plot(p1_sd)

p2_sd <-app(p2_stack, fun = sd_na_rm)/10e4
p2_df <-as.data.frame(p2_sd, xy = TRUE)
plot(p2_sd)

p3_sd <-app(p3_stack, fun = sd_na_rm)/10e4
p3_df <-as.data.frame(p3_sd, xy = TRUE)
plot(p3_sd)

p4_sd <-app(p4_stack, fun = sd_na_rm)/10e4
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
  labs(fill = expression(Delta~SWE~SD~(m^3~10^4))) +
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
  labs(fill = expression(Delta~SWE~SD~(m^3~10^4))) +
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
  labs(fill = expression(Delta~SWE~SD~(m^3~10^4))) +
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








#### loss
sd_loss <-ggplot(df) +
  geom_sf(data = sierra_sf, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = loss_sd_dam3)) + 
  # geom_sf(data = sierra_sf, fill = NA, color = "black", linewidth = .7, inherit.aes = FALSE, alpha = 1) +
  annotate("text", x = -118.98, y = 37.87, label = "SWE Loss", size = 10) +
  scale_fill_gradientn(colors = scale1, limits = c(0,100), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Delta~SWE~SD~(dam^3))) +
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
       file = "./plots/sd_loss_map_dam3_41x41.png",
       width = 3.5, 
       height = 8)

system("open ./plots/sd_loss_map_dam3_41x41.png")

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
       file = "./plots/cc_map_41x41.png",
       width = 3.5, 
       height = 8)

system("open ./plots/cc_map_55x55.pdf")


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
       file = "./plots/sd_vs_cc_map_dam3_41x41_v2.png",
       width = 9, 
       height = 9,
       dpi = 300)

system("open ./plots/sd_vs_cc_map_dam3_41x41_v2.png")













# loss
loss_sd <-app(loss_mw_41x41, fun = sd_na_rm)
loss_sd_dam3 <-loss_sd/1e3
plot(loss_sd_dam3)
# writeRaster(loss_sd_dam3, "./rasters/dswe_variabilty_analysis/loss_sd_dam3_41x41_v1.tif")

## cc focal
cc_mw <-focal(cc, c(41,41), na.rm=TRUE, fun = "mean")
plot(cc_mw)
# writeRaster(cc_mw, "./rasters/dswe_variabilty_analysis/cc_mw_mean_41x41.tif")

######### cc vs sd scatter plot
# mask out low change values
cc_sd <-c(cc_mw, gain_sd_dam3, loss_sd_dam3)

# convert to df
cc_sd_df <-as.data.frame(cc_sd, xy = TRUE, cells = TRUE)
colnames(cc_sd_df)[4:6] <-c("cc_mean", "gain_sd_dam3","loss_sd_dam3")
head(cc_sd_df)
# data.table::fwrite(cc_sd_df, "./csvs/cc_sd_df_dam3_41x41.csv")

cc_sd_df <-data.table::fread("./csvs/cc_sd_df_dam3_41x41.csv")

# quick hists
hist(cc_sd_df$cc_mean, breaks = 100)
hist(cc_sd_df$gain_sd_dam3, breaks = 100)
hist(cc_sd_df$loss_sd_dam3, breaks = 100)
max(cc_sd_df$gain_sd_dam3, na.rm = TRUE)
max(cc_sd_df$loss_sd_dam3, na.rm = TRUE)

# # filter cc of 0
# df_v2 <-dplyr::filter(cc_sd_df, cc_mean >= 0)
# hist(df_v2$cc_mean, breaks = 100)

# add bins col for box plot
df_v3 <-cc_sd_df %>%
  mutate(bin = cut_width(cc_mean, width = 5, boundary=0))

# cc scale
cc_scale <-colorRampPalette(c("#f7fcf5", "#00441b"))
cc_scale(13)


# starting plot
### gains
gains_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = gain_sd_dam3, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .6, varwidth = TRUE, outlier.size = .001, outlier.shape = 4, 
               outlier.colour = "grey80", outlier.alpha  = .01) +
  xlab("CC (%)") + ylab(expression(SWE~SD~(dam^3)))+ 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,25)) +
  scale_fill_discrete(type = cc_scale(10)) +
  theme_classic(10) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))
gains_plot

# loss
loss_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = loss_sd_dam3, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .6, varwidth = TRUE, outlier.size = .001, outlier.shape = 4,
               outlier.colour = "grey80", outlier.alpha  = .01) +
  xlab("CC (%)") + ylab(expression(SWE~SD~(dam^3)))+ 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_discrete(type = cc_scale(13)) +
  theme_classic(10) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))
loss_plot

# cowplot test
cow <-plot_grid(gains_plot, loss_plot,
                 labels = c("(a) SWE Gain", "(b) SWE Loss"),
                 nrow = 2, 
                 align = "hv",
                 label_size = 15,
                 vjust =  3,
                 hjust = -.7,
                 rel_heights = c(1/2,1/2))
# test save
# make tighter together
ggsave(cow,
       file = "./plots/swe_sd_bp_dam3_41x41_v1.png",
       width = 8, 
       height = 6,
       dpi = 300)

system("open ./plots/swe_sd_bp_dam3_41x41_v1.png")
        
