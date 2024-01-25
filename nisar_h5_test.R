library(terra)
library(rhdf5)

# path to h5
h5_path <-list.files("~/nisar_dat", pattern = "*.h5", full.names = TRUE)

# function for reading in nisar level-2 Geocoded Unwrapped Interferogram (GUNW) data
# included 5 layers: unwrapped phase (unw: radians)
#                    coherence (cor: -)
#                    connected components (conncomp: -)
#                    ionospheric phase screen (ion: -)
#                    ionospheric phase screen  uncertainty (ion_uncert: -)

read_nisar_gunw <-function(h5_path){
  
  # read file
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
  
  # pull out spatial data, flip, and convert to non georeferenced raster
  unw <-rast(t(gunw$frequencyA$unwrappedInterferogram$HH$unwrappedPhase))
  cor <-rast(t(gunw$frequencyA$unwrappedInterferogram$HH$coherenceMagnitude))
  conncomp <-rast(t(gunw$frequencyA$unwrappedInterferogram$HH$connectedComponents))
  ion <-rast(t(gunw$frequencyA$unwrappedInterferogram$HH$ionospherePhaseScreen))
  ion_uncert <-rast(t(gunw$frequencyA$unwrappedInterferogram$HH$ionospherePhaseScreenUncertainty))
  
  # stack and rename layers
  nisar_stack <-c(unw,cor,conncomp,ion,ion_uncert)
  names(nisar_stack) <-c("unw","cor","conncomp","ion","ion_uncert")
  
  # set extent, res, and project
  crs(nisar_stack) <-paste0("EPSG:",nisar_proj)
  ext(nisar_stack) <-c(xmin,xmax,ymin,ymax)
  
  # inspect
  return(nisar_stack)

}

# read data
nisar_stack <-read_nisar_gunw(h5_path)
nisar_stack

# save
writeRaster(nisar_stack[[1]], "~/nisar_dat/unw.tif")
writeRaster(nisar_stack[[2]], "~/nisar_dat/cor.tif")
writeRaster(nisar_stack[[3]], "~/nisar_dat/concomm.tif")
writeRaster(nisar_stack[[4]], "~/nisar_dat/ion.tif")
writeRaster(nisar_stack[[5]], "~/nisar_dat/ion_uncert.tif")

