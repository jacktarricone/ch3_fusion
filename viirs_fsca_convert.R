# convert and reproject raw gap filled viirs ndsi to fsca stack
# jack tarricone
# november 17th, 2022

# data downloaded using this script
# nsidc-download_VNP10A1F.001_2022-11-16.py
# https://nsidc.org/data/vnp10a1f/versions/1

library(terra)
library(stringr)
library(lubridate)
library(XML)

setwd("/Users/jacktarricone/ch3_fusion")

# list hdf files
h5_list <-list.files("./rasters/VNP10A1F_wy2020/raw", pattern = "\\.h5$", full.names = TRUE)
xml_list <-list.files("./rasters/VNP10A1F_wy2020/raw", pattern = "\\.xml$", full.names = TRUE)
head(xml_list)

# pull out data for crs
xml_parse <-xmlParse(xml_list[1])
xml <-xmlToList(xml_parse)
coords <-xml[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["GPolygon"]][["Boundary"]]
coords_df <-as.data.frame(coords)

# pull out extent box corners
xmn <-as.numeric(coords_df$Point.PointLongitude.3)
xmx <-as.numeric(coords_df$Point.PointLongitude)
ymn <-as.numeric(coords_df$Point.PointLatitude.3)
ymx <-as.numeric(coords_df$Point.PointLatitude)

## projection info
viirs_proj <-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +no_defs"
# viirs_ext <-ext(xmx,xmn,ymn,ymx) # length=4; order= xmin, xmax, ymin, ymax)
viirs_ext_m <-ext(-11119505, -10007555, 3335852, 4447802) # from modis

# for extent cropping
flm_raw <-rast("./rasters/flm/SSN.downscaled.20200403.v4.3e+05.tif")
flm <-project(flm_raw, 'EPSG:4326')
rm(flm_raw)
flm

# function for concerting, cropping, and saving
# VIIRS ndsi to fsca

viirs_ndsi_to_fsca <-function(x){
    
    # read in raw raster
    ndsi_rast <-rast(x)
    
    # project
    crs(ndsi_rast) <-viirs_proj
    ext(ndsi_rast) <-viirs_ext_m
    
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
    file_name
    
    # file naming for both 2019 and 2020 files
    wy2020 <-grepl("A2020", file_name, fixed = TRUE)
    
    if (wy2020 == TRUE){
      
      doy_raw <-str_sub(file_name,15) # remove first 15 characters
      doy <-as.numeric(str_sub(doy_raw, end=-29)) # move last 30, leaving just doy
      date <-as.Date(doy, origin = "2020-01-01") # convert doy to date using correct origin
      date_v2<-format(date, "%Y%m%d") # reformat for saving 
      name_v1 <-paste0("viirs_fsca_", date_v2, ".tif") # create file name
      
      } else{
        
        doy_raw <-str_sub(file_name,15)
        doy <-as.numeric(str_sub(doy_raw, end=-29))
        date <-as.Date(doy, origin = "2019-01-01")
        date_v2<-format(date, "%Y%m%d")
        name_v1 <-paste0("viirs_fsca_", date_v2, ".tif")
      }
    
    # save
    saving_path <-file.path("./rasters/VNP10A1F_wy2020/fsca/")
    writeRaster(fsca, paste0(saving_path, name_v1))
}

# lapp
lapply(h5_list, 
       viirs_ndsi_to_fsca)



