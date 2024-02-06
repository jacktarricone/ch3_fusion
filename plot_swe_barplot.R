# make dswe stats and bar plot
# jack tarricone
# jan 25, 2024

library(terra)
library(ggplot2)
library(tidyr)

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
names(p1_stack_cm) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")
p2_stack_cm <-rast(list.files("./rasters/new_dswe/p2", full.names = T))
names(p2_stack_cm) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")
p3_stack_cm <-rast(list.files("./rasters/new_dswe/p3", full.names = T))
names(p3_stack_cm) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")
p4_stack_cm <-rast(list.files("./rasters/new_dswe/p4", full.names = T))
names(p4_stack_cm) <-c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS")

# create cell size raster in m^2
cell_size_m2 <- cellSize(p4_stack_cm, unit = "m")

# convert SWE cm to m^3 
p1_stack_m3 <-(p1_stack_cm/10 * cell_size_m2)*1e-3
p2_stack_m3 <-(p2_stack_cm/10 * cell_size_m2)*1e-3
p3_stack_m3 <-(p3_stack_cm/10 * cell_size_m2)*1e-3
p4_stack_m3 <-(p4_stack_cm/10 * cell_size_m2)*1e-3


### format data.frames for plotting
p1_df <-as.data.frame(p1_stack_m3, xy = TRUE)
p1_df$pair <-rep("(a) P1", nrow(p1_df))
p1_df_l <-pivot_longer(p1_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p2_df <-as.data.frame(p2_stack_m3, xy = TRUE)
p2_df$pair <-rep("(b) P2", nrow(p2_df))
p2_df_l <-pivot_longer(p2_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p3_df <-as.data.frame(p3_stack_m3, xy = TRUE)
p3_df$pair <-rep("(c) P3", nrow(p3_df))
p3_df_l <-pivot_longer(p3_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))

p4_df <-as.data.frame(p4_stack_m3, xy = TRUE)
p4_df$pair <-rep("(d) P4", nrow(p4_df))
p4_df_l <-pivot_longer(p4_df, 
                       cols = c("FLM","IMS","Landsat","MODIS","MODSCAG","VIIRS"),
                       names_to = c("data_set"))


# bind for plotting
plotting_df <-rbind(p1_df_l,p2_df_l,p3_df_l,p4_df_l)
colnames(plotting_df)[5] <-"dswe_m3"
plotting_df <-na.omit(plotting_df)
head(plotting_df)

# summarize for bar plots
bar_results <- as.data.frame(plotting_df %>%
  group_by(pair,data_set) %>%
  summarize(
    Loss = as.integer(sum(dswe_m3[dswe_m3 < 0])*1e-3),
    Gain = as.integer(sum(dswe_m3[dswe_m3 > 0])*1e-3),
    Net = as.integer(sum(dswe_m3)*1e-3)))

print(bar_results)

# pivot longer again for plotting
bar_plotting <- bar_results %>%
  pivot_longer(cols = c("Loss", "Gain", "Net"), names_to = "stat", values_to = "swe_change")

bar_plotting

# make group bar plot
p <-ggplot(bar_plotting, aes(fill=stat, x = data_set, y=swe_change)) + 
  geom_bar(position="dodge", stat="identity", color = "black", width = .5)+
  # geom_text(aes(label=swe_change), position=position_dodge(width=0.9), vjust=-0.25)+
  facet_wrap(~pair)+
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-350,100,50),limits = c(-350,100))+
  scale_fill_manual(values=c('darkblue','darkred','grey90'),name="")+
  ylab(expression(Delta~SWE~(m^3~10^3))) +
  xlab("fSCA Product") +
  theme_classic(15) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "center", 
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank())

# saves
ggsave(p,
       file = "~/ch3_fusion/plots/dswe_barplot_v3.png",
       width = 10.5,
       height = 6,
       dpi = 300)

system('open ~/ch3_fusion/plots/dswe_barplot_v2.png')


