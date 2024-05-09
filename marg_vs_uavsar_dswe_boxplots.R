# compare marg and insar pair based swe changes

library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)

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

theme_set(theme_classic(14))

setwd("~/ch3_fusion/rasters/")

# load in uavsar
uavsar_list <-list.files("./wus_marg/pairs/", pattern = "uavsar", full.names = T)
uavsar_stack <-rast(uavsar_list)

# load in marg
marg_list <-list.files("./wus_marg/pairs/", pattern = "marg", full.names = T)
marg_stack <-rast(marg_list)*100
plot(marg_stack)

# resample uavsar to 500m marg data
uavsar_500 <-resample(uavsar_stack, marg_stack, method = "bilinear")
uavsar_500
plot(uavsar_500)

# calc diff
diff_stack <-uavsar_500-marg_stack
plot(diff_stack)

# create df with marg, uavsar, and diff
# marg
marg_df <-as.data.frame(marg_stack, xy = T)
colnames(marg_df)[3:6]<-c("P1","P2","P3","P4")
marg_df$data <-rep("WUS-SR",nrow(marg_df))
marg_long <- marg_df %>%
  pivot_longer(cols = starts_with("p"),
               names_to = "pair",
               values_to = "dswe")

# uavsar
uavsar_df <-as.data.frame(uavsar_500, xy = T)
colnames(uavsar_df)[3:6]<-c("P1","P2","P3","P4")
uavsar_df$data <-rep("UAVSAR",nrow(uavsar_df))
uavsar_long <- uavsar_df %>%
  pivot_longer(cols = starts_with("p"),
               names_to = "pair",
               values_to = "dswe")

# uavsar
diff_df <-as.data.frame(diff_stack, xy = T)
colnames(diff_df)[3:6]<-c("P1","P2","P3","P4")
diff_df$data <-rep("Difference",nrow(diff_df))
diff_long <- diff_df %>%
  pivot_longer(cols = starts_with("p"),
               names_to = "pair",
               values_to = "dswe")

# bind together
df1 <-bind_rows(marg_long,uavsar_long)
df <-bind_rows(df1,diff_long)
head(df)
data.table::fwrite(df, "~/ch3_fusion/csvs/uavsar_marg_plotting_df_GOOD_v1.csv")

# calc stats
stats_df <- df %>%
  group_by(data, pair) %>%
  summarize(mean_value = mean(dswe, na.rm = T),
            sd_value = sd(dswe, na.rm = T))

stats_df
# plot
p1 <-ggplot(df1, aes(x = pair, y = dswe, fill = data)) +
  geom_hline(yintercept = 0, col = "gray50", linetype = 3)+
  geom_boxplot(linewidth = .25, width = .4, 
               outlier.shape = 1, outlier.color = 'red', outlier.alpha = .1, outlier.size = .2,
               position = 'dodge') +
  scale_fill_manual(values = c('UAVSAR' = 'goldenrod', 'WUS-SR' = 'purple2'))+
  ylab(expression(Delta~SWE~(cm))) +
  xlab("Pair") + 
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = c(.65,.9),
        legend.direction = 'horizontal',
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))

ggsave(p1,
       file = "~/ch3_fusion/plots/marg_uavsar_diff_box_v2.png",
       width = 6, 
       height = 3,
       units = "in",
       dpi = 500) 

system("open ~/ch3_fusion/plots/marg_uavsar_diff_box_v2.png")

