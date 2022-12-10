# fsca mask stats
# jack tarricone
# december 6th, 2022

library(terra)
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

setwd("/Users/jacktarricone/ch3_fusion/rasters/uavsar/feb26_march11_80m/")
list.files()

#### bring in dswe rasters
# modscag
modscag <-rast("./modscag_dswe.tif")
modscag
plot(modscag)

# modis
modis <-rast("./modis_dswe.tif")
modis
plot(modis)

# viirs
viirs <-rast("./viirs_dswe.tif")
viirs
plot(viirs)

# landsat
landsat <-rast("./landsat_dswe.tif")
landsat
plot(landsat)

# flm
flm <-rast("./flm_dswe.tif")
flm
plot(flm)

# define stat generator funciton
# x <-flm

swe_stats <-function(x){
  
  ### swe_gain
  # convert rasters m^3 water
  # assuming 80 m pixel size
  gain_m3 <-(x / 100)*(80^2)
  values(gain_m3)[values(gain_m3) < 0] = NA
  # plot(gain_m3)
  swe_gain_m3 <-as.integer(global(gain_m3, "sum", na.rm = TRUE))
  swe_gain_m3_scaled <-swe_gain_m3 * (1e-6)
  
  ### swe_loss
  loss_m3 <-(x / 100)*(80^2)
  values(loss_m3)[values(loss_m3) > 0] = NA
  # plot(loss_m3)
  swe_loss_m3 <-as.integer(global(loss_m3, "sum", na.rm = TRUE))
  swe_loss_m3_scaled <-swe_loss_m3 * (1e-6)
  
  # net
  swe_net_m3_scaled <- swe_loss_m3_scaled + swe_gain_m3_scaled
  
  string <-c(swe_gain_m3_scaled,swe_loss_m3_scaled,swe_net_m3_scaled)
  return(string)
}

# test <-0.028673721*1e9
# hmmt <-test*1e-7

# calculate stats
modscag_stats <-swe_stats(modscag)
modis_stats <-swe_stats(modis)
viirs_stats <-swe_stats(viirs)
landsat_stats <-swe_stats(landsat)
flm_stas <-swe_stats(flm)

# create df for plotting
swe_change <-c(modscag_stats,modis_stats,viirs_stats,landsat_stats,flm_stas) # bind cols
#swe_change_km3 <-as.numeric(round(swe_change_km3, digits = 3)) # round
sensor <-c(rep("MODSCAG",3), rep("MODIS",3), rep("VIIRS",3), rep("Landsat",3), rep("FLM",3)) # create sensor col
stat <-c("gain","loss","net","gain","loss","net","gain",
         "loss","net","gain","loss","net","gain","loss","net") # create stat col
stats_df <-as.data.frame(cbind(sensor,swe_change,stat)) # bind as df
stats_df$swe_change <-as.numeric(stats_df$swe_change) # convert to numeric
stats_df$sensor <- factor(stats_df$sensor, levels = c('MODSCAG', 'MODIS', 'VIIRS', 'Landsat', 'FLM')) # conver to facor for plotting


# make group bar plot
ggplot(stats_df, aes(fill=stat, x = sensor, y=swe_change)) + 
  geom_bar(position="dodge", stat="identity", color = "black", width = .5)+
  geom_hline(yintercept = 0)+
  #scale_y_continuous(breaks = seq(-.01,.03,.01),limits = c(-.01,.03))+
  scale_y_continuous(breaks = seq(-10,30,10),limits = c(-10,30))+
  scale_fill_manual(values=c('darkblue','darkred','grey90'),name="")+
  ylab(expression(Delta~SWE~(10^6~m^3)))+ 
  xlab("fSCA Product") +
  theme_classic(12) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1))

# saves
# ggsave(file = "/Users/jacktarricone/ch3_fusion/plots/dswe_stats_v7.png",
#        width = 6,
#        height = 3,
#        dpi = 500)

# make table for poster
data <-rbind(modscag_stats,modis_stats,viirs_stats,landsat_stats,flm_stas)
write.csv(data, "/Users/jacktarricone/ch3_fusion/in_situ/dswe_table_v2.csv")


## swe percent stats
