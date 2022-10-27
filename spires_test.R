library(R.matlab)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")
library(rhdf5)
browseVignettes("rhdf5")
library(raveio)
library(terra)


hdf_file <-'/Users/jacktarricone/ch3_sierra_data/LC08_L2SP_043034_20190325_20200829_02_T1.mat'
test <-read_mat(hdf_file, ram = FALSE)
fsca <-rast(test$fsca[])
plot(fsca)


# oct 12
# keeping data in a matrix instead of raster for processing
# this way we can avoid geolocation issues

#set path and file name for hdf5 SWE file
hdf_name <'LC08_L2SP_043034_20190325_20200829_02_T1.mat'
hdf_path <- '/Users/jacktarricone/ch3_sierra_data/' #create path
hdf_file <-'/Users/jacktarricone/ch3_sierra_data/LC08_L2SP_043034_20190325_20200829_02_T1.mat'

h5readAttributes(hdf_file) #$units = mm



  
test <-h5read(hdf_file) #load in 

  c1[ c1[] == -32768 ] <- NA #remove NA
  max_c1 <-as.matrix(apply(c1, c(1,2), max)) #create matrix with max value on z axis
  
  #bind chunks together
  rast <-raster(full_max, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
  plot(rast)
  hist(rast)
  
  name <- gsub(".h5", "", hdf_name)
  good_name <- gsub("SN_SWE_", "max_swe_", name)
  
  setwd("/Volumes/jt/projects/margulis/max_rasters/")
  writeRaster(rast, paste0(good_name, ".tif"))
  return(rast)
}