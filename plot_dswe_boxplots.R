# plot dswe_mw  boxplots

library(terra)
library(tidyverse)
library(dtplyr)
library(scales)
library(cowplot)
library(viridis)
library(data.table)
library(RColorBrewer)

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
plotting_df <-fread("~/ch3_fusion/csvs/dswe_new_41_plotting_v8.csv")
plotting_df$data_set <-factor(plotting_df$data_set, 
                              levels=c("IMS","MODIS","VIIRS","STC","Landsat","FLM"))
head(plotting_df)

# define colors
cols <-brewer_pal(palette = "Spectral")(6)

# plot
p <-ggplot(plotting_df, mapping = aes(x = as.factor(pair), y = dswe_m3, fill = data_set)) +
  geom_boxplot(linewidth = .3, width = .7, 
               outlier.shape = 4, outlier.color = 'gray90', outlier.alpha = .05, outlier.size = .01,
               position = 'dodge') +
  scale_fill_manual(name = "fSCA Data",
                    values = c('IMS' = cols[2], 'FLM' = cols[4], 
                               'STC' = cols[3], 'MODIS' = cols[5],
                               'VIIRS' = cols[1], 'Landsat' = cols[6]))+
  guides(fill = guide_legend(ncol = 3, override.aes = list(order = c(1,2,3,4,5,6)))) +
  xlab("InSAR Pair") + ylab(expression(Delta~SWE~(m^3~10^2))) +
  scale_y_continuous(limits = c(-300,300), breaks = seq(-300,300,100)) +
  theme_classic(25) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = c(.65,.1),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))




ggsave(p,
       file = "./plots/dswe_box_v1.png",
       width = 12, 
       height = 5,
       units = "in",
       dpi = 300) 

system("open ./plots/dswe_box_v1.png")


