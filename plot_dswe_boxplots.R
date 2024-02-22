# plot dswe_mw  boxplots

library(terra)
library(tidyverse)
library(dtplyr)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

setwd("~/ch3_fusion/")

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

theme_set(theme_classic(18))

# read in csv
df <-fread("./csvs/dswe_new_41_plotting_v3.csv")
df$value2 <-df$value/10e4
head(df)

# define colors
cols <-brewer_pal(palette = "Spectral")(6)
df$data_set <-factor(df$data_set, levels = c('IMS','FLM','MODSCAG','MODIS','VIIRS','Landsat')) # conver to facor for plotting
df

# plot
p <-ggplot(df, mapping = aes(x = as.factor(pair), y = value2, fill = data_set)) +
  geom_violin(width=2, position=position_dodge(width=0.5)) +
  scale_fill_manual(name = "fSCA Data",
                    values = c('IMS' = cols[2], 'FLM' = cols[4], 
                               'MODSCAG' = cols[3], 'MODIS' = cols[5],
                               'VIIRS' = cols[1], 'Landsat' = cols[6]),
                    labels = c('IMS','FLM','MODSCAG',
                               'MODIS','VIIRS','Landsat')) +
  guides(fill = guide_legend(ncol = 3, override.aes = list(order = c(1,2,3,4,5,6)))) +
  xlab("InSAR Pair") + ylab(expression(Delta~SWE~(10^4~m^3))) +
  scale_y_continuous(limits = c(-8,6), breaks = seq(-8,6,2)) +
  theme_classic(25) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = c(.65,.1),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))


ggsave(p,
       file = "./plots/dswe_violin_v1.png",
       width = 12, 
       height = 5,
       units = "in",
       dpi = 300) 

system("open ./plots/dswe_violin_v1.png")


