# testing sen1_nisar and fused landsat/modis comparisons
# oct 27th, 2022

library(terra)
library(ggmap)
library(ggplot2);theme_set(theme_minimal(12))
library(RColorBrewer)

# setwd
setwd('/Users/jacktarricone/ch3_sierra_data/sen1_nisar_sim/jan29-feb4_2020/')
list.files()

# # load in fsca data
# fsca_files <-list.files("./optical", full.names = TRUE)
# fsca_stack <-rast(fsca_files)
# 
# # replace 0 with NaN
# values(fsca_stack)[values(fsca_stack) == 0] = NA
# 
# # reproject
# fsca_stack <-project(fsca_stack, 'EPSG:4326')
# 
# # check extent
# ext(fsca_stack)
# 
# # test plot
# plot(fsca_stack[[4]])

# save
#writeRaster(fsca_stack, "./optical/jan29-feb4_stack.tif")

## read in optical stack
# fsca_stack <-rast("./optical/jan29-feb4_stack.tif")

# # extend so sen1 isn't chopped off in the resample
# fsca_stack_expanded <- extend(fsca_stack, c(0,2500))
# plot(fsca_stack_expanded)
# 
# writeRaster(fsca_stack_expanded, "./optical/jan29-feb4_stack_expanded.tif")


# read in expanded stack
fsca_stack_expanded <-rast("./optical/jan29-feb4_stack_expanded.tif")

# list insar data
insar_files <-list.files("./insar", full.names = TRUE, pattern = '.tif')
insar_stack <-rast(insar_files) # rasterize
insar_stack <-project(insar_stack, 'EPSG:4326') # project

# inspect both stacks
insar_stack
fsca_stack_expanded

# resample insar down to fsca from expanded data
insar_resamp <-resample(insar_stack, fsca_stack_expanded, method = 'bilinear')
insar <-crop(insar_resamp, ext(insar_stack))
insar # check
plot(insar[[2]])

# crop and mask with coherence rasters
fsca_crop <-crop(fsca_stack_expanded, ext(insar))
fsca <-mask(fsca_crop, insar[[2]], maskvalue = NA)

# check they have the same spatial parameters
insar
fsca

# test plot
# test plot fsca on top of insar
plot(insar[[2]], col = heat.colors(100))
plot(fsca[[1]], add = TRUE, col = gray.colors(100))

# conver to df for plotting
cor_df <-as.data.frame(insar[[2]], xy = TRUE)
fsca_df <-as.data.frame(fsca[[1]], xy = TRUE)

# rename
names(cor_df)[3] <-"cor"
names(fsca_df)[3] <-"fsca"


# set map bounding box
loc <-c(-121,36,-116.7,38.5)

# download google sat
myMap <- get_map(location=loc,
                 source="google", 
                 maptype="satellite", 
                 crop=TRUE)

# see map, looks good
ggmap(myMap)

# set scale
greys_scale <-rev(brewer.pal(9, 'Greys'))
vid_scale <-viridis::

hist(fsca_df$fsca, breaks = 20)

# single plot test before making gif
ggmap(myMap) +
  coord_equal()+
  geom_raster(cor_df, mapping = aes(x,y, fill = cor)) +
  viridis::scale_fill_viridis(option="magma", limits = c(0,1), na.value="transparent") +
  geom_tile(fsca_df, mapping = aes(x,y,color = fsca), fill = "transparent")) +
  scale_fill_gradientn(colors = greys_scale, limits = c(0,100), na.value="transparent") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(title = "", fill = "Sen-1 NISAR Simulated w/ FLM fSCA",
       x="Lon (deg)", y="Lat (deg)")


################################################## 
################# pixel analysis ################# 
##################################################

# generate pixel counts
fsca_pixel_count <-terra::freq(fsca[[7]])
insar_pixel_count <-terra::freq(insar[[2]], digits = 1)

# calc sum
fsca_sum <-sum(fsca_pixel_count$count)
insar_sum <-sum(insar_pixel_count$count)

# calc percent
fsca_percent <-(fsca_sum/insar_sum)*100
print(fsca_percent)

## %31.4 percent of the scene

#######
# testing to makes sure this is correct
#######

fsca_area <-fsca[[1]]
insar_area <-insar[[2]]

# make binary
values(fsca_area)[values(fsca_area) > 0] = 1
values(insar_area)[values(insar_area) > 0] = 1

# test plot with some color
plot(insar_area, col = 'red')
plot(fsca_area, add = TRUE, col = 'black')

# generate pixel counts
fsca_pixel_count_bin <-terra::freq(fsca_area)
insar_pixel_count_bin <-terra::freq(insar_area)

# calc sum
fsca_sum_bin <-sum(fsca_pixel_count_bin$count)
insar_sum_bin <-sum(insar_pixel_count_bin$count)

# calc percent
fsca_percent_bin <-(fsca_sum_bin/insar_sum_bin)*100
print(fsca_percent_bin)







# create shape file from cor data
rast_to_shp <-function(file){

  rast <-rast(file) #import raster
  name <-names(rast) # extract raster name
  rast[rast > 0] <- 1   # convert all values to 1
  rast_shp_file <-as.polygons(rast) # convert to vector data
  
  ## aggregate polyongs up to just data extent
  rast_shp <- aggregate(rast_shp_file, dissolve = TRUE, fun = "mean", cores = 10)
  
  setwd("/Users/jacktarricone/ch3_sierra_data/uavsar_shape_files") # setwd
  writeVector(rast_shp, paste(name,".shp")) # save with correct name
  return(rast_shp) # return
}

# apply our funciton to the list of rasters
lapply(list_coherence_files, rast_to_shp)


