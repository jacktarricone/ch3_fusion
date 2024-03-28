# make dswe stats and bar plot
# jack tarricone
# jan 25, 2024

library(terra)
library(ggplot2)
library(tidyr)
library(dplyr)

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
setwd("~/ch3_fusion")

# load in 80 m insar dswe products
p1_stack_cm <-rast(list.files("./rasters/new_dswe/p1", full.names = T))
names(p1_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
p2_stack_cm <-rast(list.files("./rasters/new_dswe/p2", full.names = T))
names(p2_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
p3_stack_cm <-rast(list.files("./rasters/new_dswe/p3", full.names = T))
names(p3_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
p4_stack_cm <-rast(list.files("./rasters/new_dswe/p4", full.names = T))
names(p4_stack_cm) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")

# create cell size raster in m^2
cell_size_m2 <- cellSize(p4_stack_cm, unit = "m")

# convert SWE cm to m^3 
p1_stack_m3 <-(p1_stack_cm/100 * cell_size_m2)
p2_stack_m3 <-(p2_stack_cm/100 * cell_size_m2)
p3_stack_m3 <-(p3_stack_cm/100 * cell_size_m2)
p4_stack_m3 <-(p4_stack_cm/100 * cell_size_m2)

### format data.frames for plotting
p1_df <-as.data.frame(p1_stack_m3, xy = TRUE)
p1_df$pair <-rep("(a) P1", nrow(p1_df))
p1_df_l <-pivot_longer(p1_df, 
                       cols = c("IMS","MODIS","VIIRS","STC","Landsat","FLM"),
                       names_to = c("data_set"))

p2_df <-as.data.frame(p2_stack_m3, xy = TRUE)
p2_df$pair <-rep("(b) P2", nrow(p2_df))
p2_df_l <-pivot_longer(p2_df, 
                       cols = c("IMS","MODIS","VIIRS","STC","Landsat","FLM"),
                       names_to = c("data_set"))

p3_df <-as.data.frame(p3_stack_m3, xy = TRUE)
p3_df$pair <-rep("(c) P3", nrow(p3_df))
p3_df_l <-pivot_longer(p3_df, 
                       cols = c("IMS","MODIS","VIIRS","STC","Landsat","FLM"),
                       names_to = c("data_set"))

p4_df <-as.data.frame(p4_stack_m3, xy = TRUE)
p4_df$pair <-rep("(d) P4", nrow(p4_df))
p4_df_l <-pivot_longer(p4_df, 
                       cols = c("IMS","MODIS","VIIRS","STC","Landsat","FLM"),
                       names_to = c("data_set"))


# bind for plotting
plotting_df <-rbind(p1_df_l,p2_df_l,p3_df_l,p4_df_l)
colnames(plotting_df)[5] <-"dswe_m3"
plotting_df <-na.omit(plotting_df)
head(plotting_df)
hist(plotting_df$dswe_m3)

# summarize for bar plots
bar_results <- as.data.frame(plotting_df %>%
  group_by(pair,data_set) %>%
  summarize(
    Loss = as.numeric(sum(dswe_m3[dswe_m3 < 0])*10^-7),
    Gain = as.numeric(sum(dswe_m3[dswe_m3 > 0])*10^-7),
    Net = as.numeric(sum(dswe_m3)*10^-7)))

print(bar_results)

# pivot longer again for plotting
bar_plotting <- bar_results %>%
  pivot_longer(cols = c("Loss", "Gain", "Net"), names_to = "stat", values_to = "swe_change")

bar_plotting$data_set <-factor(bar_plotting$data_set, 
                               levels=c("IMS","MODIS","VIIRS","STC","Landsat","FLM"))

min(bar_plotting$swe_change)

# make group bar plot
p <-ggplot(bar_plotting, aes(fill=stat, x = data_set, y=swe_change)) + 
  geom_bar(position="dodge", stat="identity", color = "black", width = .5)+
  facet_wrap(~pair)+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-2,1.5,.5),limits = c(-2,1.5))+
  scale_fill_manual(values=c('darkblue','darkred','grey90'),name="")+
  ylab(expression(Delta~SWE~(10^7~m^3))) +
  xlab("fSCA Product") +
  theme_classic(15) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "center", 
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank())
p


# saves
# ggsave(p,
#        file = "~/ch3_fusion/plots/fig6_dswe_barplot_v8.pdf",
#        width = 10.5,
#        height = 6,
#        dpi = 300)
# 
# system('open ~/ch3_fusion/plots/fig6_dswe_barplot_v8.pdf')

## numbers for text

mean_changes <- as.data.frame(bar_plotting %>%
                               group_by(pair,stat) %>%
                               summarize(mean = round(mean(swe_change),digits = 2)))

net_mean_changes <-filter(mean_changes, stat == "Net")
net_mean_changes

mult_diff <- as.data.frame(bar_plotting %>%
                                 group_by(pair,stat) %>%
                                 summarize(abs_max = round(max(abs(swe_change)),digits = 2),
                                           abs_min = round(min(abs(swe_change)),digits = 2),
                                           fact = round((abs_max/abs_min), digits = 2)))

net_mult_diff <-filter(mult_diff, stat == "Net")
net_mult_diff

names_df <- as.data.frame(bar_plotting %>%
                             group_by(pair, stat) %>%
                             summarize(
                               max_sensor = data_set[which.max(abs(swe_change))],
                               min_sensor = data_set[which.min(abs(swe_change))]
                             ))
names_df
mult_diff
mean(mult_diff$fact)

gain_mult_diff <-filter(mult_diff, stat == "Gain")
gain_mult_diff

loss_mult_diff <-filter(mult_diff, stat == "Loss")
loss_mult_diff

mean_changes

mean_net <-filter(mean_changes, stat == "Net")
mean_net

mean_gain <-filter(mean_changes, stat == "Gain")
mean_gain

sensor_changes <- as.data.frame(bar_plotting %>%
                                group_by(data_set,stat,pair) %>%
                                summarize(mean = round(mean(abs(swe_change)),digits = 2)))

sensor_net <-filter(sensor_changes, stat == "Net")
sensor_net

mean_net_sensor_changes <- as.data.frame(sensor_net%>%
                                  group_by(data_set) %>%
                                  summarize(mean_net = round(mean(mean),digits = 2)))
mean_net_sensor_changes

