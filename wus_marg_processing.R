# processing wus margulis data for carries analysis

library(terra)
library(ncdf4)
require(XML)
library(EflowStats)

# set working directory
setwd("~/ch3_fusion/")

# file paths
t1 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W119_0_agg_16_WY2019_20_SWE_SCA_POST.nc"
x1 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W119_0_agg_16_WY2019_20_SWE_SCA_POST.nc.xml"
t2 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W120_0_agg_16_WY2019_20_SWE_SCA_POST.nc"
x2 <-"~/ch3_fusion/rasters/wus_marg/WUS_UCLA_SR_v01_N37_0W120_0_agg_16_WY2019_20_SWE_SCA_POST.nc.xml"

# function to read in nc, geolocate, convert to rast for 1 tile
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
  xmin2 <-round(xmin, 0)
  print(xmin2)
  xmax <-as.numeric(xml_data[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["BoundingRectangle"]][["EastBoundingCoordinate"]])
  xmax2 <-round(xmax, 0)
  print(xmax2)
  ymin <-as.numeric(xml_data[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["BoundingRectangle"]][["SouthBoundingCoordinate"]])
  print(ymin)
  ymax <-as.numeric(xml_data[["GranuleURMetaData"]][["SpatialDomainContainer"]][["HorizontalSpatialDomainContainer"]][["BoundingRectangle"]][["NorthBoundingCoordinate"]])
  print(ymax)
  
  # set crs
  crs(mean_swe_rast) <-"epsg:4326"
  # set extent
  ext(mean_swe_rast)<- ext(xmin2,xmax2,ymin,ymax)
  mean_swe_rast
  dowy_rast <-mean_swe_rast[[dowy]]
  return(dowy_rast)
}

# convert date to day of water year
dates <-as.Date(c("2020-01-31","2020-02-12","2020-02-19","2020-02-26","2020-03-04"))
dowy <- get_waterYearDay(dates, 10L)

# fun function on all 5 dates
rast1 <-ncdf_to_tiff(nc_path = t1, xml_path = x1, dowy = dowy)
rast2 <-ncdf_to_tiff(nc_path = t2, xml_path = x2, dowy = dowy)

# merge tiles
full_rast <-merge(rast1,rast2)
plot(full_rast)

for (i in 1:length(dowy)){
  
  dataset <-dowy[i]
  writeRaster(full_rast[[i]], paste0("~/ch3_fusion/rasters/wus_marg/uavsar_dates_swe/swe_dowy_",dataset,"_v1.tif"))
  
}

### create pairs by differencing
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
study_area <-crop(mask(full_rast,sierra),ext(sierra))
# study_area2 <-ifel(study_area < .005,NA,study_area)
p1 <-study_area[[2]]-study_area[[1]]
p2 <-study_area[[3]]-study_area[[2]]
p3 <-study_area[[4]]-study_area[[3]]
p4 <-study_area[[5]]-study_area[[4]]

# bring in viirs data
v_p1 <-rast("./rasters/new_optical/p1_80m_20200131_20200212/viirs_0212_80m.tif")
v_p2 <-rast("./rasters/new_optical/p2_80m_20200212_20200219/viirs_0219_80m.tif")
v_p3 <-rast("./rasters/new_optical/p3_80m_20200219_20200226/viirs_0226_80m.tif")
v_p4 <-rast("./rasters/new_optical/p4_80m_20200226_20200311/viirs_0311_80m.tif")

# stack and resampe
v_stack <-c(v_p1,v_p2,v_p3,v_p4)
v_resamp <-resample(v_stack, p1)
plot(v_resamp)

# bring in viirs data
stack <-c(p1,p2,p3,p4)
masked_stack <-mask(stack, v_resamp)
plot(masked_stack)
hist(stack,breaks=100)

# # save
writeRaster(masked_stack[[1]], "./rasters/wus_marg/pairs/p1_marg_dswe_v5.tif")
writeRaster(masked_stack[[2]], "./rasters/wus_marg/pairs/p2_marg_dswe_v5.tif")
writeRaster(masked_stack[[3]], "./rasters/wus_marg/pairs/p3_marg_dswe_v5.tif")
writeRaster(masked_stack[[4]], "./rasters/wus_marg/pairs/p4_marg_dswe_v5.tif")

