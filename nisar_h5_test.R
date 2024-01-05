library(terra)
library(rhdf5)

# path to h5
h5_path <-list.files("~/nisar_dat", pattern = "*.h5", full.names = TRUE)

# load in data
gunw <-h5read(h5_path, "/science/LSAR/GUNW/grids/")

# pull out projection
nisar_proj <-gunw$frequencyA$unwrappedInterferogram$projection

# pull out coords to set bounding box
y_coords <-gunw$frequencyA$unwrappedInterferogram$yCoordinates
x_coords <-gunw$frequencyA$unwrappedInterferogram$xCoordinates

# find max and mins
ymin <-min(y_coords)
ymax <-max(y_coords)
xmin <-min(x_coords)
xmax <-max(x_coords)

# pull out spatial data in matrix form
cor <-gunw$frequencyA$unwrappedInterferogram$HH$coherenceMagnitude
unw <-gunw$frequencyA$unwrappedInterferogram$HH$unwrappedPhase
conncomp <-gunw$frequencyA$unwrappedInterferogram$HH$connectedComponents
ion <-gunw$frequencyA$unwrappedInterferogram$HH$ionospherePhaseScreen
ion_uncert <-gunw$frequencyA$unwrappedInterferogram$HH$ionospherePhaseScreenUncertainty

h5closeAll()  
print("c1 read into memory")
  
  ## calculate pixel-wise max
  # returns max value in mm per pixel in the given year
  max_c1 <-as.matrix(apply(c1, c(1,2), snow_metric_function)) 
  print("c1 max calculated")
  rm(c1) # clean up
  
  ## same for south half of data
  c2 <-h5read(swe_list, "/SWE", index = list(3301:6601,1:5701,1:nday))
  print("c2 read into memory")
  max_c2 <-as.matrix(apply(c2, c(1,2), snow_metric_function))
  print("c2 max calculated")
  rm(c2)
  h5closeAll()
  
  #bind chunks together
  full_max <-rbind(max_c1,max_c2)
  r <-rast(full_max) # convert from matrix to raster
  rm(full_max) # trash array
  values(r)[values(r) == -32768] <- NA # change no data to NA
  print("-32768 converted to NA")
  
  # georeference
  ext(r) <-c(-123.3,-117.6,35.4,42) # set extent
  crs(r) <-crs(dem) # set crs from DEM raster
  
  # name formatting
  name <- gsub(".h5", "", basename(swe_list))
  good_name <- gsub("SN_SWE", snow_metric_name, name)
  
  # set saving director to correct folder
  # doesn't need to change for each metric bc include at top of script
  saving_location <-list.files("./rasters/snow_metrics/",
                               pattern = paste0("*",snow_metric_name,"$"), 
                               full.names = TRUE)
  # save
  setwd(saving_location)
  writeRaster(r, paste0(good_name, ".tif"))
  
  # thank you!
  print(paste0(good_name," has been generated!"))
}
