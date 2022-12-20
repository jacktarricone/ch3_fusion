# convert uavsar coherence data to shape files for isce_sat2 group work
# july 13th, 2022
# we have failed the group. we are using R

library(terra)

list_coherence_files <-list.files("/Users/jacktarricone/ch3_fusion/rasters/uavsar/sierra_35402_20006-003_20009-024_0007d_s01_L090_01_int_grd/", full.names = TRUE)
print(list_coherence_files)

# create shape file from cor data
rast_to_shp <-function(file){

  rast <-rast(file) #import raster
  name <-names(rast) # extract raster name
  rast[rast > 0] <- 1   # convert all values to 1
  rast_shp_file <-as.polygons(rast) # convert to vector data
  
  ## aggregate polyongs up to just data extent
  rast_shp <- aggregate(rast_shp_file, dissolve = TRUE, fun = "mean", cores = 10)
  
  setwd("/Users/jacktarricone/ch3_fusion/uavsar_shape_files") # setwd
  writeVector(rast_shp, paste(name,".shp")) # save with correct name
  return(rast_shp) # return
}

# apply our funciton to the list of rasters
lapply(list_coherence_files[2], rast_to_shp)


