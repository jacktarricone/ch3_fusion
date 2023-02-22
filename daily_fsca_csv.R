# calculate daily % and m^2 fsca mask stats for USJ
# jack tarricone
# december 20th, 2022

library(terra)
library(lubridate)
library(ggplot2)

# setwd
setwd("/Users/jacktarricone/ch3_fusion/rasters/")
list.files()

# usj shp
usj <-vect("/Users/jacktarricone/ch3_fusion/shapefiles/usj.shp")

#### bring in full usj rast: coherence
cor_v1 <-rast("./clips/usj/cor_usj_20200305.tif")
cor <-project(cor_v1, crs('EPSG:4326'))
cor

# bring in formatted fsca data
modis <-rast("./fsca_usj_80m/modis_fsca_usj_80m_stack.tif")
viirs <-rast("./fsca_usj_80m/viirs_fsca_usj_80m_stack.tif")
flm <-rast("./fsca_usj_80m/flm_fsca_usj_80m_stack.tif")

modis
viirs
flm

# calculate area in basin
expanse(modis[[180]], unit = "km")
usj_area <-expanse(cor, unit = "km")

# define function for calculating percent fsca area
# at given fsca threshold
fsca_percent_calc <-function(fsca_rast, total_area, threshold){
  
  # fsca_rast: rast object
  # threshold: number above which pixel is considered snow covered
  
  test <-fsca_rast # create dummy stack
  values(test)[values(test) < threshold] = NA # mask for threshold
  fsca_area <-expanse(test, unit = "km") # calculate fsca area
  fsca_percent <-(fsca_area/total_area)*100 # calculate percent of basin covered
  return(fsca_percent)
  
}


### run function on stack for different thresholds
# modis
modis_15_raw <-fsca_percent_calc(modis, total_area = usj_area, threshold = 15)
modis_15 <-c(NA, modis_15_raw)

# viirs
viirs_15 <-fsca_percent_calc(viirs, total_area = usj_area, threshold = 15)

# flm
flm_15 <-fsca_percent_calc(flm, total_area = usj_area, threshold = 15)
flm_15_v2 <-c(flm_15, rep(NA,110)) # add NAs at end

# make df
dowy <-seq(1,366,1) # dowy column
df <-as.data.frame(cbind(dowy))
df$date <-seq(ymd('2019-10-01'),ymd('2020-09-30'), by = '1 day') # make date col
df <-as.data.frame(cbind(df,flm_15_v2,modis_15,viirs_15)) # bind
# write.csv(df, "/Users/jacktarricone/ch3_fusion/csvs/modis_fsca_percent_rough.csv", row.names = FALSE)

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


# test plot
ggplot(df) +
  geom_line(aes(x = date, y = flm_15_v2), col = 'red', alpha = .5, size = .4) +
  geom_line(aes(x = date, y = modis_15), col = 'darkblue', alpha = .5, size = .4) +
  geom_line(aes(x = date, y = viirs_15), col = 'darkgreen', alpha = .5, size = .4) +
  scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
  ylab("Basin Snow Cover (%)")+ 
  xlab("Date") +
  theme_classic(12) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1))

# saves
ggsave(file = "/Users/jacktarricone/ch3_fusion/plots/fsca_15_rough_v3.png",
       width = 6,
       height = 3,
       dpi = 500)


