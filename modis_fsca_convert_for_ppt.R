# convert and reproject raw gap filled modis ndsi to fsca stack
# jack tarricone
# november 16th, 2022

# data downloaded using this script
# nsidc-download_MOD10A1F.061_2022-11-15.py 

library(terra)
library(stringr)
library(lubridate)

setwd("/Users/jacktarricone/ch3_fusion")

# list hdf files
date_list <-list.files("/Users/jacktarricone/proposal_drafts/disseration/ppt/modis", pattern = "\\.hdf$", full.names = TRUE)
head(date_list)

# for extent cropping
flm_raw <-rast("./rasters/flm/raw/SSN.downscaled.20191001.v4.3e+05.tif")
flm <-project(flm_raw, 'EPSG:4326')
rm(flm_raw)
flm

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
    
    # save
    saving_path <-file.path("/Users/jacktarricone/proposal_drafts/disseration/ppt/")
    writeRaster(fsca[[1]], paste0(saving_path, file_name,".tif"))
}


# lapp
lapply(date_list, 
       ndsi_to_fsca)



