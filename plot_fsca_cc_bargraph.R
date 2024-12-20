# plot updated fsca data
# march 14th, 2024

library(terra)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(viridis)
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

setwd("~/ch3_fusion/rasters")

# load in 80 m insar dswe products
p1_stack1 <-rast(list.files("./new_optical/p1_80m_20200131_20200212", pattern = "_80m", full.names = T))
names(p1_stack1) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
p1_stack <-ifel(is.na(p1_stack1), 0, p1_stack1)
plot(p1_stack[[3]])
freq(p1_stack[[3]])


# p2_stack <-rast(list.files("./new_optical/p2_80m_20200212_20200219/", pattern = "_80m", full.names = T))
# names(p2_stack) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
# p3_stack <-rast(list.files("./new_optical/p3_80m_20200219_20200226/", pattern = "_80m", full.names = T))
# names(p3_stack) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")
# p4_stack <-rast(list.files("./new_optical/p4_80m_20200226_20200311/", pattern = "_80m", full.names = T))
# names(p4_stack) <-c("FLM","IMS","Landsat","MODIS","STC","VIIRS")


# bring in cc data
cc_raw <-rast("./geo_layers/cc_domain.tif")
cc1 <-resample(cc_raw, p1_stack)
cc_sa <-mask(cc1, p1_stack1[[1]]) # before nan remove stack
plot(cc_sa)
names(cc) <-("cc")
cc <-ifel(is.na(cc), 0, cc)
plot(cc)

# calc cell size in km
cell_size <-cellSize(p1_stack, unit = "km")
cell_size
plot(cell_size)

# convert fsca percent to area
fsca_area <-(p1_stack/100)*cell_size
plot(fsca_area[[3]])

# stack with cc
full_stack <-c(cc,fsca_area)
df1 <-as.data.frame(full_stack, xy = T)
head(df1)

# Calculate quantiles for canopy cover
data <- df1 %>%
  mutate(
    cc_group = case_when(
      cc >= 0 & cc <= 15 ~ "0-15",
      cc >= 15 & cc <= 30  ~ "15-30",
      cc >= 30 & cc <= 45 ~ "30-45",
      cc >= 45 & cc <= 60 ~ "45-60",
      cc >= 60 & cc <= 100 ~ "60-100",
    )
  )


# Summarize total area by quantile
summary <- data %>%
  group_by(cc_group) %>%
  summarize(
    FLM = sum(FLM, na.rm = TRUE),
    IMS = sum(IMS, na.rm = TRUE),
    Landsat = sum(Landsat, na.rm = TRUE),
    MODIS = sum(MODIS, na.rm = TRUE),
    STC = sum(STC, na.rm = TRUE),
    VIIRS = sum(VIIRS, na.rm = TRUE)
  )

# Print the result
summary

plot_df1 <-pivot_longer(summary, 
                       cols = c("IMS","MODIS","VIIRS","STC","Landsat","FLM"),
                       names_to = c("dataset"))
###########
### plot for SCA total area
#############
ggplot(plot_df1, aes(fill= dataset, x = cc_group, y=value)) + 
  geom_bar(position="dodge", stat="identity", color = "black", width = .5)+
  scale_y_continuous(expand = c(.004,0), 
                     limits = c(0,700),
                     breaks = c(seq(0,700,100)))+
  scale_fill_viridis_d(option = "turbo")+
  ylab(expression(SCA~(km^2))) +
  xlab("Canopy Cover (%)") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = c(0.70, 0.83),
        legend.text = element_text(size = 8),
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "center", 
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"))

ggsave(file = "~/ch3_fusion/plots/fig8_fsca_cc_bargraph_v4.pdf",
       width = 5.5, 
       height = 3)

system("open ~/ch3_fusion/plots/fig8_fsca_cc_bargraph_v4.pdf") 


# calc cc area for bins
# function

ccArea <-function(cc_rast, lower,upper){
  cc_v1 <-ifel(cc_rast >= upper, NA, cc_rast)
  cc_v2 <-ifel(cc_v1 <= lower, NA, cc_v1)
  df <-expanse(cc_v2, unit = "km")
  cc_area <-df$area[1]
}

# run
cc15 <-ccArea(cc_sa,0,15)
cc30 <-ccArea(cc_sa,15,30)
cc45 <-ccArea(cc_sa,30,45)
cc60 <-ccArea(cc_sa,45,60)
cc100 <-ccArea(cc_sa,60,100)

cc15_perc <-(cc15/area)*100
cc30_perc <-(cc30/area)*100
cc45_perc <-(cc45/area)*100
cc60_perc <-(cc60/area)*100
cc100_perc <-(cc100/area)*100


shp <-vect("/Users/jtarrico/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
area <-expanse(shp, unit = "km")
plot(p1_stack1[[1]])


# make col
just_string <-c(cc15,cc30,cc45,cc60,cc100)

summary$cc_total_area <-just_string
summary

result <- summary %>% 
  group_by(cc_group) %>%
  mutate(
    FLM = (FLM / cc_total_area)*100,
    IMS = (IMS / cc_total_area)*100,
    Landsat = (Landsat / cc_total_area)*100,
    MODIS = (MODIS / cc_total_area)*100,
    STC = (STC / cc_total_area)*100,
    VIIRS = (VIIRS / cc_total_area)*100
  )

result

result2 <- summary %>% 
  group_by(cc_group) %>%
  mutate(
    FLM = (FLM / area)*100,
    IMS = (IMS / area)*100,
    Landsat = (Landsat / area)*100,
    MODIS = (MODIS / area)*100,
    STC = (STC / area)*100,
    VIIRS = (VIIRS / area)*100
  )

result2

# calc area stats
cc15_perc <-(cc15/area)*100

# plotting df
plot_df2 <-pivot_longer(result, 
                        cols = c("IMS","MODIS","VIIRS","STC","Landsat","FLM"),
                        names_to = c("dataset"))

ggplot(plot_df2, aes(fill= dataset, x = cc_group, y=value)) + 
  geom_bar(position="dodge", stat="identity", color = "black", width = .5)+
  scale_y_continuous(expand = c(.004,0), 
                     limits = c(0,100),
                     breaks = c(seq(0,100,20)))+
  scale_fill_viridis_d(option = "turbo")+
  ylab("SCA (%)") +
  xlab("Canopy Cover (%)") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = c(0.3, 0.83),
        legend.text = element_text(size = 8),
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "center", 
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"))

ggsave(file = "~/ch3_fusion/plots/fig8_fsca_cc_bargraph_percent_v2.pdf",
       width = 5.5, 
       height = 3)

system("open ~/ch3_fusion/plots/fig8_fsca_cc_bargraph_percent_v2.pdf") 


