# create new incidence angle data from cop dem and geocoded lvk from SLC data
# jack tarricone

####
# using the function cgrad, from the insol package
# calculate the 3d (x,y,z) unit vector for a valle grande crop of the cop dem
# then add? not sur yet, to make surface normal vector

# set path to '/jemez_lband_swe_code_data' that was downloaded and unzipped from zenodo
# all other file paths are relative
setwd("~/ch3_fusion")
list.files() #pwd

library(terra)
library(raster)
library(insol) # https://rdrr.io/cran/insol/man/cgrad.html


# notes from meeting with HP October 25th

### creating new incidence angle raster from CZO cop data
# will be used for SWE inversion of UAVSAR data

## steps
# 1.create slope, aspect rasters from filtered DEM
# 2.reproject and resample cop data products to UAVSAR projection (wsg-84, lat/lon)
# 3.use these resampled products to create new .inc file

# function to calculate "gradient" of cop raster (vector)
# three component vector (easting, northing, 3 component vector)
# calculate dot product and calculate the angle
# dot product of gradient from cop raster and path length vector (n1*n2+e1*e2+up1*up2)
# cos^-1((n1*n2+e1*e2+up1*up2)/(distance calc through atm for each vector))

# packages in r or python for calculating gradients and surface normals


#########################################################################

# bring in cop dem with raster not terra
# switched back to using the raster package bc cgrad can injest only rasters not SpatRasters!
# cop_dem <-raster("/Users/jacktarricone/ch1_jemez_data/jemez_cop/valles_elev_filt.img")
# plot(cop_dem, col = terrain.colors(3000)) # test plot
# 
# # crop down
# crop_ext <-extent(359000, 374000, 3965000, 3980000) # set vg UTM extent for raster
# crop_ext_sr <-ext(359000, 374000, 3965000, 3980000) # for spatrast
# cop_crop <-crop(cop_dem, crop_ext)
# writeRaster(cop_crop, "/Users/jacktarricone/ch1_jemez_data/jemez_cop/jemez_cop_crop.tif")

# bring in cop crop
## use raster not rast
cop_dem <-raster("./rasters/cop_dem_30m/output_COP30.tif")
plot(cop_dem, col = terrain.colors(3000)) # test, good
cop_dem

########
# calculate the gradient in all three dimensions
# this function create a matrix for the x,y,z competent of a unit vector
########

grad_mat <-cgrad(cop_dem, 1, 1, cArea = FALSE)

# make individual raster layer for each competent
# and geocode back to original crop extent
# switch back to terra
cop_rast <-rast(cop_dem)
sierra_shp <-vect("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
crop_ext_sr <-ext(sierra_shp)

## x
x_comp <-rast(grad_mat[,,1], crs = crs(cop_rast))
ext(x_comp) <-ext(cop_rast)
plot(x_comp)

## y
y_comp <-rast(grad_mat[,,2], crs = crs(cop_rast))
ext(y_comp) <-ext(cop_rast)
plot(y_comp)

## z
z_comp <-rast(grad_mat[,,3], crs = crs(cop_rast))
ext(z_comp) <-ext(cop_rast)
plot(z_comp)

rm(grad_mat)

#################################
# bring in look vector data #####
#################################

# read in lvk file we made before
lvk_km <-rast("./rasters/lvk/lkv_km.tif")
lvk_m <-lvk_km*1000
plot(lvk_km)

# cop latlon
cop_ll <-project(cop_crop_spat, crs(lvk_km))
plot(cop_ll)

# east
radar_east_v1 <-rast("./rasters/lvk/alamos_35915_01_BU_s1_2x8.lkv.y.tif")
radar_east <-resample(radar_east_v1, lvk_m)

# north
radar_north_v1 <-rast("./rasters/lvk/alamos_35915_01_BU_s1_2x8.lkv.x.tif")
radar_north <-resample(radar_north_v1, lvk_m)

# up
radar_up_v1 <-rast("./rasters/lvk/alamos_35915_01_BU_s1_2x8.lkv.z.tif")
radar_up <-resample(radar_up_v1, lvk_m)

######
# resample to uavsar grid
######
# x
x_rs_v1 <-project(x_comp, radar_east, method = "bilinear")
x_rs <-resample(x_rs_v1, radar_east, method = "bilinear")
x_rs
radar_east

# y
y_rs_v1 <-project(y_comp, radar_north, method = "bilinear")
y_rs <-resample(y_rs_v1, radar_north, method = "bilinear")
y_rs
radar_north

# z
z_rs_v1 <-project(z_comp, radar_up, method = "bilinear")
z_rs <-resample(z_rs_v1, radar_up, method = "bilinear")
z_rs
radar_up

### dot product formula
# cos^-1((y_rs*radar_north+x_rs*radar_east+z_rs*radar_up)/(distance calc through atm for each vector))

# calculate surface normal
dot_prod <-(y_rs*radar_north + x_rs*radar_east+ z_rs*radar_up)
plot(dot_prod)

# compute the dot product to get a inc. angle in radians
# make sure to put the negative sign!
## rad
inc_ang_rad <-(acos)(-dot_prod/(lvk_m))
plot(inc_ang_rad)

# deg
inc_ang_deg <-inc_ang_rad*(180/pi)
plot(inc_ang_deg)

## save
# writeRaster(inc_ang_deg, "./rasters/incidence_angle/cop_inc_deg.tif")
# writeRaster(inc_ang_rad, "./rasters/incidence_angle/cop_inc_rad.tif")

















