setwd("/Users/jacktarricone/ch3_fusion")

# list hdf files
h5_list <-list.files("./rasters/VNP10A1F_wy2020/fsca", pattern = ".tif", full.names = TRUE)
viirs_fsca_stack <-rast(h5_list)
viirs_fsca_stack

snow <-seq(3,1828,5)
snow_stack <-viirs_fsca_stack[[snow]]
snow_stack

cuts=c(0,10,20,30,40,50,60,70,80,90,100) #set breaks
pal <- colorRampPalette(c("blue","orange"))
plot(snow_stack[[c(66:70)]],breaks=cuts, col = pal(10))
