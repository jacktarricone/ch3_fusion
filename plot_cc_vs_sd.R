# plot cc vs sd

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
colnames(cc_df)[3] <-"cc_mean"
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
p1_df <-as.data.frame(p1_sd, xy = TRUE)
p1_df$pair <-rep("p1", nrow(p1_df))

p2_sd <-app(p2_stack, fun = sd_na_rm)/1e5
p2_df <-as.data.frame(p2_sd, xy = TRUE)
p2_df$pair <-rep("p2", nrow(p2_df))

p3_sd <-app(p3_stack, fun = sd_na_rm)/1e5
p3_df <-as.data.frame(p3_sd, xy = TRUE)
p3_df$pair <-rep("p3", nrow(p3_df))

p4_sd <-app(p4_stack, fun = sd_na_rm)/1e5
p4_df <-as.data.frame(p4_sd, xy = TRUE)
p4_df$pair <-rep("p4", nrow(p4_df))

plotting_df <-rbind(p1_df, p1_df, p3_df, p4_df)

# # filter cc of 0
# df_v2 <-dplyr::filter(cc_sd_df, cc_mean >= 0)
# hist(df_v2$cc_mean, breaks = 100)

# add bins col for box plot
df_v3 <-plotting_df  %>%
  mutate(bin = cut_width(cc_mean, width = 5, boundary=0))

# cc scale
cc_scale <-colorRampPalette(c("#f7fcf5", "#00441b"))
cc_scale(13)


# starting plot
### gains
p1_p <-ggplot(df_v3, mapping = aes(x = cc_mean, y = gain_sd_dam3, fill = as.factor(bin))) +
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
        
