# plot ecdf for all four pairs and all six sensors

library(terra)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(dgof)

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


# load in 41x41 dswe products
p1_stack <-rast("./new_uavsar/p1_phase_stack2.tif")

p2_stack_v1 <-rast(list.files("./dswe_variabilty_analysis/p2", pattern = ".tif", full.names = T))
p2_stack <-c(p2_stack_v1, no_mask_mw[[2]])
names(p2_stack) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS","No Mask")


p3_stack_v1 <-rast(list.files("./dswe_variabilty_analysis/p3", pattern = ".tif", full.names = T))
p3_stack <-c(p3_stack_v1, no_mask_mw[[3]])
names(p3_stack) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS", "No Mask")

p4_stack_v1 <-rast(list.files("./dswe_variabilty_analysis/p4", pattern = ".tif", full.names = T))
p4_stack <-c(p4_stack_v1, no_mask_mw[[4]])
names(p4_stack) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS", "No Mask")


### format data.frames for plotting
p1_df <-as.data.frame(p1_stack, xy = TRUE)
p1_df$pair <-rep("p1", nrow(p1_df))
p1_df_l <-pivot_longer(p1_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS","No Mask"),
                       names_to = c("data_set"))

p2_df <-as.data.frame(p2_stack, xy = TRUE)
p2_df$pair <-rep("p2", nrow(p2_df))
p2_df_l <-pivot_longer(p2_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS","No Mask"),
                       names_to = c("data_set"))

p3_df <-as.data.frame(p3_stack, xy = TRUE)
p3_df$pair <-rep("p3", nrow(p3_df))
p3_df_l <-pivot_longer(p3_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS","No Mask"),
                       names_to = c("data_set"))

p4_df <-as.data.frame(p4_stack, xy = TRUE)
p4_df$pair <-rep("p4", nrow(p4_df))
p4_df_l <-pivot_longer(p4_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS","No Mask"),
                       names_to = c("data_set"))


# bind for plotting
plotting_df <-rbind(p1_df_l,p2_df_l,p3_df_l,p4_df_l)
# data.table::fwrite(plotting_df, "~/ch3_fusion/csvs/dswe_new_41_plotting.csv")
head(plotting_df)


# display.brewer.all()
plot_colors <-brewer.pal(6, "Spectral")
# "#D53E4F" "#FC8D59" "#FEE08B" "#E6F598" "#99D594" "#3288BD"
unique(plotting_df$d)

# plot
ecdf <-ggplot()+
  stat_ecdf(plotting_df, mapping = aes(x=value, color = data_set), linewidth=.5) +
  scale_colour_manual(name = "Snow cover data",
                     # labels = c("IMS","MODSCAG","MODIS","VIIRS","FLM","Landsat", "No Mask"),
                      values = c("IMS" = "#D53E4F",
                                 "MODSCAG" = "#FC8D59",
                                 "MODIS" = '#99D594',
                                 "VIIRS" = '#3288BD',
                                 "FLM" = '#FEE08B',
                                 "Landsat" = 'purple',
                                 "No Mask" = "black"),
                      breaks = c("No Mask","IMS","MODSCAG","MODIS","VIIRS","FLM","Landsat"))+ 
  facet_wrap( ~pair )+
  ylab("Cumulative Distribution") +
  xlab(expression(Delta~SWE~(10^2~ m^3)))+
  scale_x_continuous(expand = c(.01,0)) +
  scale_y_continuous(expand = c(.01,0), limits = c(0,1)) +
  theme(legend.position = c(.38,.25)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_blank(),
        strip.text.x = element_blank())



# create labels
head(plotting_df)
f_labels <- data.frame(
  label = c("(a) P1", "(b) P2" ,"(c) P3", "(d) P4"),
  pair = c("p1", "p2" ,"p3", "p4"),
  x = c(-350,-350,-350,-350),
  y = c(.9,.9,.9,.9))

# this works!!
p2 <- ecdf + geom_text(data = f_labels, aes(x = x, y = y, label = label), size = 6) 

# test save
# make tighter together
ggsave(p2,
       file = "~/ch3_fusion/plots/ecdf_v7.png",
       width = 8, 
       height = 6,
       dpi = 150)
  
system("open ~/ch3_fusion/plots/ecdf_v7.png")
