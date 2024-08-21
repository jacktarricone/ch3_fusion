# plot cc vs sd boxplots
# update: feb 23

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

# means
mean(p1_df$lyr.1)
mean(p2_df$lyr.1)
mean(p3_df$lyr.1)
mean(p4_df$lyr.1)

max(p1_df$lyr.1)
max(p2_df$lyr.1)
max(p3_df$lyr.1)
max(p4_df$lyr.1)

# add bins col for box plot
df_v3 <-cc_df  %>%
  mutate(bin = cut(focal_mean, c(0,5,10,15,20,25,30,35,40,45,50,55,60)))

# join cc
p1_df2 <-full_join(p1_df, df_v3, by = c("x","y"))
p2_df2 <-full_join(p2_df, df_v3, by = c("x","y"))
p3_df2 <-full_join(p3_df, df_v3, by = c("x","y"))
p4_df2 <-full_join(p4_df, df_v3, by = c("x","y"))

# combine
plotting_df <-bind_rows(p1_df2, p2_df2, p3_df2, p4_df2)
plotting_df_v2 <-na.omit(plotting_df)
colnames(plotting_df_v2)[3] <-"iqr"
head(plotting_df_v2)
hist(plotting_df_v2$iqr, breaks = 100)

# starting plot
p1_p <-ggplot(plotting_df_v2, mapping = aes(x = focal_mean, y = iqr, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .3, varwidth = TRUE, outlier.size = .001, outlier.shape = 4, 
               outlier.colour = "red", outlier.alpha  = .01, fatten = 2) +
  scale_fill_discrete(type = rep("gray90",12)) +
  facet_wrap(~pair, scales = "fixed", nrow = 4) +
  xlab("CC (%)") + ylab(expression(IQR~(m^3)))+
  scale_x_continuous(limits = c(0,55), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,100)) +
  theme_classic(13) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"), 
        strip.text.x = element_blank())

p1_p

# create labels
f_labels <- data.frame(
  label = c("(a) P1", "(b) P2" ,"(c) P3", "(d) P4"),
  pair = c("(a) P1", "(b) P2" ,"(c) P3", "(d) P4"),
  bin = c(5,5,5,5),
  y = c(110, 110, 110, 110))

# this works!!
p2 <- p1_p + geom_text(data = f_labels, aes(x = bin, y = y, label = label), size = 5) 
p2

# test save
# make tighter together
ggsave(p2,
       file = "~/ch3_fusion/plots/fig8_cc_vs_sd_boxplot_v12.pdf",
       width = 4,
       height = 5.5)

system("open ~/ch3_fusion/plots/fig8_cc_vs_sd_boxplot_v12.pdf")


### numbers for results section
plotting_df

# Group by 'bin' and calculate the median and mean values
result <- plotting_df_v2 %>%
  group_by(pair,bin) %>%
  summarize(med = median(sd), mean = mean(sd))

# Print the result
print(result)
p1 <-filter(result, pair == "P1")
p1

p2 <-filter(result, pair == "P2")
p2

p3 <-filter(result, pair == "P3")
p3

p4 <-filter(result, pair == "P4")
p4
