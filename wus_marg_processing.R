# processing wus margulis data for carries analysis

library(terra)
library(ncdf4)
require(XML)
# install.packages("remotes")
remotes::install_github("USGS-R/EflowStats")

# set working directory
setwd("~/ch3_fusion/")

# file path for 2017 swe and sca ncdf4 in the sierras
t1 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W119_0_agg_16_WY2019_20_SWE_SCA_POST.nc"
x1 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W119_0_agg_16_WY2019_20_SWE_SCA_POST.nc.xml"
t2 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W120_0_agg_16_WY2019_20_SWE_SCA_POST.nc"
x2 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W120_0_agg_16_WY2019_20_SWE_SCA_POST.nc.xml"


ncdf_to_tiff <-function(nc_path, xml_path, dowy){
  
  ncin <- nc_open(nc_path)
  dname <- "SWE_Post"  # define variable name
  # print(ncin) # print netcdf contents

  # pull out SWE variable
  swe_array <- ncvar_get(ncin,dname)
  # dim(swe_array) # check dims

  # gives us 4 dimensions here (nrow,ncol,statitics,time)
  ## data organized in order of the 5 statistics for the 50 model ensemble 
  # stat_1 = mean
  # stat_2 = standard deviation
  # stat_3 = median
  # stat_4 = 25th
  # stat_5 = 75th

  # create array for just mean_swe
  mean_swe_array <-swe_array[,,1,]

  # convert to raster
  mean_swe_rast <-rast(mean_swe_array)

  ### geolocate and project
  # !!!!this is not the right way to do this given the variable pixel size

  # parse xml file
  data <- xmlParse(xml_path)
  xml_data <- xmlToList(data)

  # bounding box for geolocation
  xmin <-as.numeric(xml_data[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["BoundingRectangle"]][["WestBoundingCoordinate"]])
  xmax <-as.numeric(xml_data[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["BoundingRectangle"]][["EastBoundingCoordinate"]])
  ymin <-as.numeric(xml_data[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["BoundingRectangle"]][["SouthBoundingCoordinate"]])
  ymax <-as.numeric(xml_data[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["BoundingRectangle"]][["NorthBoundingCoordinate"]])

  # set crs
  crs(mean_swe_rast) <-"epsg:4326"
  # set extent
  ext(mean_swe_rast)<- ext(xmin,xmax,ymin,ymax)
  mean_swe_rast
  dowy_rast <-mean_swe_rast[[dowy]]
  return(dowy_rast)

}
rast1 <-ncdf_to_tiff(nc_path = t1, xml_path = x1, dowy = 180)
rast2 <-ncdf_to_tiff(nc_path = t2, xml_path = x2, dowy = 180)
plot(rast2)

full_rast <-merge(rast1,rast2)
plot(full_rast)
??terra::merge
writeRaster(mean_swe_rast[[180]], "./rasters/wus_marg/wus_test5.tif")

# test plot
plot(mean_swe_rast[[160]]-mean_swe_rast[[153]])
diff <-mean_swe_rast[[160]]-mean_swe_rast[[153]]

writeRaster(diff, "swe_diff_160_153.tif")




