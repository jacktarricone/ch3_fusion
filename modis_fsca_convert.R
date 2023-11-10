# convert and reproject raw gap filled modis ndsi to fsca stack
# jack tarricone
# november 16th, 2022

# data downloaded using this script
# nsidc-download_MOD10A1F.061_2022-11-15.py 

library(terra)
library(stringr)
library(lubridate)

setwd("/Users/jtarrico/ch3_fusion")

# list hdf files
date_list <-list.files("/Users/jtarrico/proposal_drafts/disseration/ppt/modis", pattern = "\\.hdf$", full.names = TRUE)
head(date_list)

# for extent cropping
flm_raw <-rast("./rasters/flm/raw/SSN.downscaled.20200401.v4.3e+05.tif")
flm <-project(flm_raw, 'EPSG:4326')
rm(flm_raw)
flm
plot(flm)

# function for concerting, cropping, and saving
# MODIS ndsi to fsca
ndsi_to_fsca <-function(x){
    
    # read in raw raster
    ndsi_rast <-rast(x)
    
    # convert to fsca using equation presented in stillinger et al. 2022
    # fsca = −0.01 + (1.45 × ndsi)
    fsca_rast <-ndsi_rast
    fsca_rast[[1]] <- -.01 + (1.45*ndsi_rast[[1]])
    
    # reproject to geographic coords
    fsca_reproj <-project(fsca_rast, 'EPSG:4326')

    # crop down to flm ext
    fsca <-crop(fsca_reproj, ext(flm))
    
    # pull out file name
    file_name <-basename(x)
    
    # file naming for both 2019 and 2020 files
    wy2020 <-grepl("A2020", file_name, fixed = TRUE)
    
    if (wy2020 == TRUE){
      
      doy_raw <-str_sub(file_name,15) # remove first 15 characters
      doy <-as.numeric(str_sub(doy_raw, end=-30)) # move last 30, leaving just doy
      date <-as.Date(doy, origin = "2020-01-01") # convert doy to date using correct origin
      date_v2<-format(date, "%Y%m%d") # reformat for saving 
      name_v1 <-paste0("modis_fsca_", date_v2, ".tif") # create file name
      
      } else{
        
        doy_raw <-str_sub(file_name,15)
        doy <-as.numeric(str_sub(doy_raw, end=-30))
        date <-as.Date(doy, origin = "2019-01-01")
        date_v2<-format(date, "%Y%m%d")
        name_v1 <-paste0("modis_fsca_", date_v2, ".tif")
      }
    
    # save
    saving_path <-file.path("./rasters/MOD10A1F_wy2020/fsca/")
    writeRaster(fsca, paste0(saving_path, name_v1))
}

# lapp
lapply(date_list, 
       ndsi_to_fsca)



