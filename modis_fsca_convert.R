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
data_list <-list.files("./rasters/MOD10A1F_wy2020/raw", pattern = "\\.hdf$", full.names = TRUE)
head(data_list)

# for extent cropping
flm_raw <-rast("./rasters/flm/SSN.downscaled.20200403.v4.3e+05.tif")
flm <-project(flm_raw, 'EPSG:4326')
flm

ndsi_to_fsca <-function(x){
  
  
    x <-data_list[51]
    x
    
    # read in raw raster
    ndsi_rast <-rast(x)
    plot(ndsi_rast[[1]])
    
    # convert to fsca using equation presented in stillinger et al. 2022
    # fsca = −0.01 + (1.45 × ndsi)
    fsca_rast <-ndsi_rast
    fsca_rast[[1]] <- -.01 + (1.45*ndsi_rast[[1]])
    fsca_rast
    
    # reproject to geographic coords
    fsca_reproj <-project(fsca_rast, 'EPSG:4326')
    fsca_reproj
    plot(fsca_reproj)

    # crop down to flm ext
    fsca <-crop(fsca_reproj, ext(flm))
    plot(fsca[[1]])
    hist(fsca[[1]])
    
    # pull out file name
    file_name <-basename(x)
    file_name
    
    # file naming for both 2019 and 2020 files
    wy2020 <-grepl("A20200", file_name, fixed = TRUE)
    
    if (wy2020 == TRUE){
      
      doy_raw <-str_sub(file_name,15)
      doy <-as.numeric(str_sub(doy_raw, end=-30))
      date <-as.Date(doy, origin = "2020-01-01")
      date_v2<-format(date, "%Y%m%d")
      name_v1 <-paste0("modis_fsca_", date_v2, ".tif")
      
      }else{
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
# crop modis ndsi down 
ndsi_crop <-crop(ndsi, ext(flm))

# set values above 100, which are data flags, to NA, and 0
values(ndsi_crop[[1]])[values(ndsi_crop[[1]]) > 100] <- NA
values(ndsi_crop[[1]])[values(ndsi_crop[[1]]) == 0] <- NA

# plot both
plot(flm, col = viridis::viridis(100, option = "plasma"))
plot(ndsi_crop[[1]], col = viridis::viridis(100, option = "plasma"))

plot(ndsi_crop[[1]], col = viridis::viridis(20, option = "plasma"))
plot(flm, col = viridis::viridis(100, option = "viridis"), add = TRUE)

# save rasters for plotting in Q
# writeRaster(flm, "./rasters/for_Q/flm_april_3_2020.tif")

writeRaster(ndsi_crop[[1]], "./rasters/for_Q/modis_ndsi_feb1_1_2020.tif")

# convert to fsca
modis_fsca <-(-.01 + (1.45*ndsi_crop))
modis_fsca
plot(ndsi_crop)
hist(ndsi_crop)
writeRaster(modis_fsca, "./rasters/for_Q/modis_fsca_feb1_1_2020.tif")

### load in insar data
# setwd
setwd('./sen1_nisar_sim/jan29-feb4_2020/')
list.files()

# list insar data
insar_files <-list.files("./insar", full.names = TRUE, pattern = '.tif')
insar_stack <-rast(insar_files) # rasterize
insar_stack <-project(insar_stack, 'EPSG:4326') # project

# inspect both stacks
insar_stack

# plot
grey_ramp <-RColorBrewer::brewer.pal(9, "Greys") # set color ramp
plot(insar_stack[[2]], col = grey_ramp)
plot(ndsi_crop[[1]], col = viridis::viridis(20, option = "plasma"), add = TRUE)
plot(flm, col = viridis::viridis(100, option = "viridis"), add = TRUE)

 