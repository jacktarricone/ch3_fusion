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

# calculate pixelwise standard deviation, add pair row
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

# # filter cc of 0
# df_v2 <-dplyr::filter(cc_sd_df, cc_mean >= 0)
# hist(df_v2$cc_mean, breaks = 100)

# add bins col for box plot
df_v3 <-cc_df  %>%
  mutate(bin = cut_width(cc_mean, width = 5, boundary=0))

# join cc
p1_df2 <-full_join(p1_df, df_v3, by = c("x","y"))
p2_df2 <-full_join(p2_df, df_v3, by = c("x","y"))
p3_df2 <-full_join(p3_df, df_v3, by = c("x","y"))
p4_df2 <-full_join(p4_df, df_v3, by = c("x","y"))

# combine
plotting_df <-rbind(p1_df2, p2_df2, p3_df2, p4_df2)
plotting_df_v2 <-na.omit(plotting_df)
colnames(plotting_df_v2)[3] <-"sd"
head(plotting_df_v2)

# cc scale
cc_scale <-colorRampPalette(c("#f7fcf5", "#00441b"))
cc_scale(13)



# starting plot
### gains
p1_p <-ggplot(plotting_df_v2, mapping = aes(x = cc_mean, y = sd, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .6, varwidth = TRUE, outlier.size = .001, outlier.shape = 4, 
               outlier.colour = "grey80", outlier.alpha  = .01) +
  facet_wrap(~pair, scales = "fixed")+
  xlab("CC (%)") + ylab(expression(Delta~SWE~SD~(m^3~ 10^5)))+ 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,2)) +
  scale_fill_discrete(type = cc_scale(10)) +
  theme_classic(13) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"), 
        strip.text.x = element_blank())

# f_labels <- data.frame(drv = c("p1", "p2" ,"p3", "p4"), label = c("(a) P1", "(b) P2" ,"(c) P3", "(d) P4"))

# fix later!!
# p1_p +  geom_text(x = 10, y = 1.8, aes(label = label), data = f_labels)
p1_p

# test save
# make tighter together
ggsave(p1_p,
       file = "~/ch3_fusion/plots/cc_vs_sd_v2.png",
       width = 8, 
       height = 4,
       dpi = 300)

system("open ~/ch3_fusion/plots/cc_vs_sd_v2.png")
        
