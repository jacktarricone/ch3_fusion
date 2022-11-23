library(terra)
setwd("/Users/jacktarricone")

# path to h3_v8 folder
folders_list <-list.files("./ch3_fusion/landsat_fsca/h3_v8", full.names = TRUE) 
tifs <-list.files(folders_list,  pattern = ".TIF", full.names = TRUE) 

# stack
stack <-rast(tifs)
stack

# reproj
system.time(stack_v2 <-project(stack, "EPSG:4326", method = "bilinear"))
plot(stack_v2[[5]])
stack_v2[[5]]


