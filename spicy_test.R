library(terra)
library(ncdf4)


banner <-rast("~/spicy/SnowEx-Data/Banner_2021-03-15.nc")

# path to file
nc_path <-x
nc_in <-nc_open("~/spicy/SnowEx-Data/Banner_2021-03-15.nc") # open
nc_in

# pull out needed info
projection <-ncvar_get(nc_in,"crs_wkt")
sd_mat <-rast(ncvar_get(nc_in,"snow_depth")) # read in
sd_mat 
sd_proj <- wkt("GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AXIS["Latitude",NORTH],AXIS["Longitude",EAST],AUTHORITY["EPSG","4326"]]"

# make test rast from one day
flip_mat <-apply(t(sc_mat),2,rev) # rotate matrix 90 deg counter clockwise
sc_rast <-rast(flip_mat)
