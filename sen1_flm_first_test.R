# testing sen1_nisar and fused landsat/modis comparisons
# oct 27th, 2022

library(terra)

# setwd
setwd('/Users/jacktarricone/ch3_sierra_data/sen1_nisar_sim/jan29-feb4_2020/')
list.files()

# load in fsca data
fsca_files <-list.files("./optical", full.names = TRUE)
fsca_stack <-rast(fsca_files)

# replace 0 with NaN
values(fsca_stack)[values(fsca_stack) == 0] = NA

# reproject
fsca_stack <-project(fsca_stack, 'EPSG:4326')

# check extent
ext(fsca_stack)

# test plot
plot(fsca_stack[[4]])

# save
#writeRaster(fsca_stack, "./optical/jan29-feb4_stack.tif")
fsca_stack <-rast("./optical/jan29-feb4_stack.tif")

# list insar data
insar_files <-list.files("./insar", full.names = TRUE, pattern = '.tif')
insar_stack <-rast(insar_files)
insar_stack <-project(insar_stack, 'EPSG:4326')

# check both stacks
insar_stack
fsca_stack

plot(insar_stack[[4]], col = rev(heat.colors(100)))
plot(fsca_stack[[7]], add = TRUE, col = gray.colors(100))

# resample insar down to fsca
#### need to change the resampling so it doesn't cut off the edge of the nisar data ###
insar_resamp <-resample(insar_stack, fsca_stack, method = 'bilinear')
insar <-crop(insar_resamp, ext(insar_stack))
insar # check

# crop and mask with coherence rasters
fsca_crop <-crop(fsca_stack, ext(insar))
fsca <-mask(fsca_crop, insar[[2]], maskvalue = NA)

# check they have the same spatial parameters
insar
fsca

# test plot
plot(insar[[2]])
plot(fsca[[1]], add = TRUE, col = gray.colors(100))

# generate pixel counts
fsca_pixel_count <-terra::freq(fsca[[7]])
insar_pixel_count <-terra::freq(insar[[2]], digits = 1)

# calc sum
fsca_sum <-sum(fsca_pixel_count$count)
insar_sum <-sum(insar_pixel_count$count)

# calc percent
fsca_percent <-(fsca_sum/insar_sum)*100
print(fsca_percent)

## %33.22 percent of the scene


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


