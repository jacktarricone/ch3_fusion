# format ims data
# june 16th

library(terra)
library(ncdf4)

setwd("~/ch3_fusion")

# list the raw netcdf files
nc_list <-list.files('./rasters/ims/raw/', full.names = TRUE)

# format_ims_data <-function(x){

  # path to file
  nc_path <-x
  nc_in <-nc_open(nc_path) # open

  # pull out needed info
  sc_mat <-ncvar_get(nc_in,"IMS_Surface_Values") # read in

  # make test rast from one day
  flip_mat <-apply(t(sc_mat),2,rev) # rotate matrix 90 deg counter clockwise
  sc_rast <-rast(flip_mat)

  # georefernce
  crs(sc_rast) <-"+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6356257 +units=m +no_defs"
  ext(sc_rast) <-ext(-12288000.0, 12288000.0, -12288000.0, 12288000.0)

  # rough crop so reproject is faster
  rough_crop <-ext(-5008000.0,0,-5088000.0,0)
  crop <-crop(sc_rast, rough_crop)

  # reproject using near neighbor bc categorical data
  reproj <-project(crop, "epsg:4326", method = "near")
  
  # crop and mask to snsr
  snsr <-vect('~/ch1_margulis/vectors/snsr_shp.gpkg')
  ct <-crop(reproj ,ext(snsr))
  final <-mask(ct, snsr)
  plot(final)
  
  # format name
  raw_name <-basename(x)
  name2 <-paste0("snsr_",raw_name)
  name3 <-gsub(".nc",".tif",name2)
  
  # save
  writeRaster(final, paste0("./rasters/ims/snsr_tifs/",name3))
  print(paste0(name3," is done!"))
}

format_ims_data_v2 <-function(x){
  
  # path to file
  nc_path <-x
  nc_in <-nc_open(nc_path) # open
  
  # pull out needed info
  sc_mat <-ncvar_get(nc_in,"IMS_Surface_Values") # read in
  
  # make test rast from one day
  flip_mat <-apply(t(sc_mat),2,rev) # rotate matrix 90 deg counter clockwise
  sc_rast <-rast(flip_mat)
  
  # georefernce
  crs(sc_rast) <-"+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6356257 +units=m +no_defs"
  ext(sc_rast) <-ext(-12288000.0, 12288000.0, -12288000.0, 12288000.0)
  
  # rough crop so reproject is faster
  rough_crop <-ext(-5008000.0,0,-5088000.0,0)
  crop <-crop(sc_rast, rough_crop)
  
  # reproject using near neighbor bc categorical data
  reproj <-project(crop, "epsg:4326", method = "near")
  
  # crop and mask to snsr
  snsr <-vect('~/ch1_margulis/vectors/snsr_shp.gpkg')
  final <-crop(reproj ,ext(snsr))
  # final <-mask(ct, snsr)
  plot(final)
  
  # format name
  raw_name <-basename(x)
  name2 <-paste0("v2_snsr_",raw_name)
  name3 <-gsub(".nc",".tif",name2)
  
  # save
  writeRaster(final, paste0("./rasters/ims/snsr_tifs/",name3))
  print(paste0(name3," is done!"))
}

lapply(nc_list[[157]], format_ims_data_v2)

# stack
tifs_list <-list.files("./rasters/ims/snsr_tifs/", full.names = TRUE, pattern = ".tif")
stack <-rast(tifs_list)
stack
writeRaster(stack, "./rasters/ims/ims_snsr_stack.tif")
