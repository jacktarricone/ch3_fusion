# formatting landsat fsca data for usj analysis
# jack tarricone
# november 29th, 2022

library(terra)
library(stringr)

setwd("/Users/jacktarricone/ch3_fusion/rasters/")

# create dates list
file_name <-list.files("./landsat_fsca/h3_v10_2020")
chop_front <-str_sub(file_name, 16) # remove first 16 characters
dates_list_v10 <-str_sub(chop_front, end=-18) # move last 18, leaving just doy

file_name1 <-list.files("./landsat_fsca/h3_v09_2020")
chop_front1 <-str_sub(file_name, 16) # remove first 16 characters
dates_list_v09 <-str_sub(chop_front, end=-18) # move last 18, leaving just doy

# check if the same, yes
identical(dates_list_v10, dates_list_v09)

# path to h3_v10 folder
v10 <-list.dirs("./landsat_fsca/h3_v10_2020", full.names = TRUE)
head(v10)

# path to h3_v09 folder
v09 <-list.files("./landsat_fsca/h3_v09_2020", full.names = TRUE)
head(v09)

# list tifs
v10_tifs <-list.files(v10,  pattern = ".TIF", full.names = TRUE) 
v09_tifs <-list.files(v09,  pattern = ".TIF", full.names = TRUE) 
head(v10_tifs)

stack_rep <-seq(1,670,5)

# landsat
# bring in other tile so it covers full basin
landsat_10 <-rast("./landsat_fsca/h3_v10_2020/LC08_CU_003010_20200304_20210504_02_SNOW/LC08_CU_003010_20200304_20210504_02_GROUND_SNOW.TIF")
landsat_9 <-rast("./landsat_fsca/h3_v9_2020/LC08_CU_003009_20200304_20210504_02_SNOW/LC08_CU_003009_20200304_20210504_02_GROUND_SNOW.TIF")

# merge and reproject
landsat_r <-merge(landsat_10, landsat_9)
plot(landsat_r)

# reproj
landsat <-project(landsat_r, "EPSG:4326", method = "bilinear")
landsat
plot(landsat)
plot(usj, add = TRUE)

# stack
stack <-rast(tifs)
stack

# reproj
system.time(stack_v2 <-project(stack, "EPSG:4326", method = "bilinear"))
plot(stack_v2[[5]])
stack_v2[[5]]


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


