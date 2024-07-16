# compare marg and insar pair based swe changes

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(viridis)
library(ggpubr)
library(data.table)
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

theme_set(theme_classic(15))

setwd("~/ch3_fusion/")

# read in sierra shp
sierra_v1 <-st_read("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
sierra_sf <-st_geometry(sierra_v1)

# read in df
p_df <-fread("./csvs/uavsar_marg_plotting_df_GOOD_v2.csv")
uavsar_marg_df <-dplyr::filter(p_df, data != "Difference") %>% filter(!is.na(dswe))
diff_df <-dplyr::filter(p_df, data == "Difference")
head(p_df)

# filter for p1
uavsar_p1 <-filter(p_df, data == "WUS-SR" & pair == "P1")
marg_p1 <-filter(p_df, data == "UAVSAR" & pair == "P1")
join <-bind_rows(uavsar_p1, marg_p1)
diff_p1 <-filter(diff_df, pair == "P1")
hist(diff_p1$dswe, breaks = 100)
min(diff_p1$dswe, na.rm = T)
max(diff_p1$dswe, na.rm = T)
median(diff_p1$dswe, na.rm = T)
mean(diff_p1$dswe, na.rm = T)
quantile(diff_p1$dswe, c(.05, .5, .95), na.rm = T) 

# set color scale
swe_scale <-brewer.pal(9, "RdBu")

dswe_both <-ggplot(join) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "gray50", linewidth = .0001, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = dswe)) + 
  facet_wrap(vars(data),scales = "fixed") +
  scale_fill_gradientn(colors = swe_scale, limits = c(-8,8), oob = squish, na.value = "gray50", guide = "none") + 
  labs(fill = expression(atop(Delta~SWE,(cm))))+
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(), 
        legend.box.spacing = unit(0, "pt"), 
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(size = 13, face = "bold")) +
  guides(fill = guide_colorbar(direction = "vertical",
                               label.position = 'right',
                               title.position ='top',
                               title.hjust = .5,
                               title.vjust = 2,
                               barwidth = 1,
                               barheight = 20,
                               frame.colour = "black", 
                               ticks.colour = "black")) 
dswe_both

# ggsave(dswe_both,
#        file = "./plots/dswe_uavsar_marg_v4.pdf",
#        width = 8,
#        height = 12)
# 
# system("open ./plots/dswe_uavsar_marg_v4.pdf")

# plot diff
# set color scale
diff_scale <-rev(brewer.pal(9, "Spectral"))

diff_p <-ggplot(diff_p1) +
  geom_sf(data = sierra_sf, fill = "gray50", color = "gray50", linewidth = .001, inherit.aes = FALSE, alpha = 1) +
  geom_raster(mapping = aes(x,y, fill = dswe)) + 
  # facet_wrap(vars(pair), scales = "fixed") +
  scale_fill_gradientn(colors = diff_scale, limits = c(-10,10), oob = squish, na.value = "gray50", guide = "none") + 
  labs(fill = expression(atop(Delta~SWE~Diff,(cm))), title = 'Difference')+
  theme(panel.border = element_blank(),
        plot.title = element_text(margin=margin(b = 0, unit = "pt"), 
                                  face = "bold", size = 13, hjust = .4),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(), 
        legend.box.spacing = unit(0, "pt"), 
        strip.text.y.left = element_text(angle = 0),
        strip.text.x.top = element_blank(),
        strip.text = element_text(size = 13, face = "bold")) +
  guides(fill = guide_colorbar(direction = "vertical",
                               label.position = 'right',
                               title.position ='top',
                               title.hjust = .5,
                               title.vjust = 2,
                               barwidth = 1,
                               barheight = 20,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# diff_p

full <-plot_grid(dswe_both, diff_p,
                 align = "h", 
                 ncol = 2, 
                 vjust = 10.5,
                 rel_widths = c(.66,.43))

# full
ggsave(full,
       file = "./plots/full_dswe_uavsar_marg_v6.pdf",
       width = 8,
       height = 6)

system("open ./plots/full_dswe_uavsar_marg_v6.pdf")

# ggsave(full,
#        file = "./plots/full_dswe_uavsar_marg_v2.png",
#        width = 8,
#        height = 12,
#        dpi = 500)
# 
# system("open ./plots/full_dswe_uavsar_marg_v2.png")



# pivoted_df <- uavsar_marg_df %>%
#   pivot_wider(names_from = data, values_from = dswe) %>%
#   filter(!is.na(UAVSAR))
# 
# pearson_r <- pivoted_df %>%
#   group_by(pair) %>%
#   summarise(
#     r = cor(`WUS-SR`, UAVSAR, method = "pearson"),
#     rmse = sqrt(mean((`WUS-SR` - UAVSAR)^2)),
#     mae = mean(abs(`WUS-SR` - UAVSAR)),
#     r_squared = cor(`WUS-SR`, UAVSAR)^2)
# 
# ggplot(pivoted_df, aes(x = `WUS-SR`, y = UAVSAR))+
#   geom_point(alpha = .01)