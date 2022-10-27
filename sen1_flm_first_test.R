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
insar_stack
plot(insar_stack[[4]], col = heat.colors(100))
plot(fsca_stack[[7]], add = TRUE, col = gray.colors(100))


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


