# crop, mask, and resample non-landsat optical data to the four dates
# jack tarricone

library(terra)

setwd("~/ch3_fusion/rasters")

# bring in sim nisar
# bring in coh from masking and cropping
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp_v4.gpkg")
coh_80m <-rast("./new_uavsar/p1_80m/p1_14d_VV_coh_80m.tif")
coh_reproj <-project(coh_80m, 'EPSG:32611', method = 'bilinear')
plot(coh_80m)
plot(sierra, add = T)

################
##### flm ######
################

# p1: jan 21 -- feb 12
flm_0131 <-rast("./flm/raw/SSN.downscaled.20200131.v4.3e+05.tif")
flm_0212 <-rast("./flm/raw/SSN.downscaled.20200212.v4.3e+05.tif")

# p2
flm_0219 <-rast("./flm/raw/SSN.downscaled.20200219.v4.3e+05.tif")

# p3
flm_0226 <-rast("./flm/raw/SSN.downscaled.20200226.v4.3e+05.tif")

# p4
flm_0311 <-rast("./flm/raw/SSN.downscaled.20200311.v4.3e+05.tif")

# stack 
flm_stack_v1 <-c(flm_0131,flm_0212,flm_0219,flm_0226,flm_0311)

# reformat
flm_stack_v2 <-crop(flm_stack_v1, ext(coh_reproj))
flm_stack_v3 <-mask(resample(project(flm_stack_v2, coh_80m, method = 'bilinear'),coh_80m),coh_80m)
flm_stack_v3
plot(flm_stack_v3)

# save
# writeRaster(flm_stack_v3[[2]], "./new_optical/p1_80m_20200131_20200212/flm_0212_80m.tif")
# writeRaster(flm_stack_v3[[3]], "./new_optical/p2_80m_20200212_20200219/flm_0219_80m.tif")
# writeRaster(flm_stack_v3[[4]], "./new_optical/p3_80m_20200219_20200226/flm_0226_80m.tif")
# writeRaster(flm_stack_v3[[5]], "./new_optical/p4_80m_20200226_20200311/flm_0311_80m.tif")


################
##### modis ####
################

# p1: jan 21 -- feb 12
modis_0131 <-rast("./MOD10A1F_wy2020/fsca_v2/modis_fsca_20200131.tif")
modis_0212 <-rast("./MOD10A1F_wy2020/fsca_v2/modis_fsca_20200212.tif")

# p2
modis_0219 <-rast("./MOD10A1F_wy2020/fsca_v2/modis_fsca_20200219.tif")

# p3
modis_0226 <-rast("./MOD10A1F_wy2020/fsca_v2/modis_fsca_20200226.tif")

# p4
modis_0311 <-rast("./MOD10A1F_wy2020/fsca_v2/modis_fsca_20200311.tif")

# stack 
modis_stack_v1 <-c(modis_0131,modis_0212,modis_0219,modis_0226,modis_0311)

# reformat
modis_stack_v2 <-crop(modis_stack_v1, ext(coh_80m))
modis_stack_v3 <-mask(resample(modis_stack_v2, coh_80m, method = 'bilinear'),coh_80m)
plot(modis_stack_v3)

# save
# writeRaster(modis_stack_v3[[2]], "./new_optical/p1_80m_20200131_20200212/modis_0212_80m.tif")
# writeRaster(modis_stack_v3[[3]], "./new_optical/p2_80m_20200212_20200219/modis_0219_80m.tif")
# writeRaster(modis_stack_v3[[4]], "./new_optical/p3_80m_20200219_20200226/modis_0226_80m.tif")
# writeRaster(modis_stack_v3[[5]], "./new_optical/p4_80m_20200226_20200311/modis_0311_80m.tif")
# 

################
##### ims ######
################

# p1: jan 21 -- feb 12
ims_0131 <-rast("./ims/snsr_tifs/snsr_ims2020031_1km_v1.3.tif")
ims_0212 <-rast("./ims/snsr_tifs/snsr_ims2020043_1km_v1.3.tif")

# p2
ims_0219 <-rast("./ims/snsr_tifs/snsr_ims2020050_1km_v1.3.tif")

# p3
ims_0226 <-rast("./ims/snsr_tifs/snsr_ims2020057_1km_v1.3.tif")

# p4
ims_0311 <-rast("./ims/snsr_tifs/snsr_ims2020071_1km_v1.3.tif")

# stack 
ims_stack_v1 <-c(ims_0131,ims_0212,ims_0219,ims_0226,ims_0311)

# reformat
ims_stack_v2 <-crop(ims_stack_v1, ext(coh_80m))
ims_stack_v3 <-mask(resample(ims_stack_v2, coh_80m, method = 'near'), coh_80m) # categorigcal, so near
plot(ims_stack_v3)

# convert all categorical values to percents
ims_stack_v4 <-ifel(ims_stack_v3 == 4, 100, ims_stack_v3)
ims_stack_v5 <-ifel(ims_stack_v4 == 2, NA, ims_stack_v4)
plot(ims_stack_v5)

# save
# writeRaster(ims_stack_v5[[2]], "./new_optical/p1_80m_20200131_20200212/ims_0212_80m.tif", overwrite = T)
# writeRaster(ims_stack_v5[[3]], "./new_optical/p2_80m_20200212_20200219/ims_0219_80m.tif", overwrite = T)
# writeRaster(ims_stack_v5[[4]], "./new_optical/p3_80m_20200219_20200226/ims_0226_80m.tif", overwrite = T)
# writeRaster(ims_stack_v5[[5]], "./new_optical/p4_80m_20200226_20200311/ims_0311_80m.tif", overwrite = T)

################
#### viirs #####
################

# p1: jan 21 -- feb 12
viirs_0131 <-rast("./VNP10A1F_wy2020/fsca_v2/viirs_fsca_20200131.tif")
viirs_0212 <-rast("./VNP10A1F_wy2020/fsca_v2/viirs_fsca_20200212.tif")

# p2
viirs_0219 <-rast("./VNP10A1F_wy2020/fsca_v2/viirs_fsca_20200219.tif")

# p3
viirs_0226 <-rast("./VNP10A1F_wy2020/fsca_v2/viirs_fsca_20200226.tif")

# p4
viirs_0311 <-rast("./VNP10A1F_wy2020/fsca_v2/viirs_fsca_20200311.tif")

# stack 
viirs_stack_v1 <-c(viirs_0131,viirs_0212,viirs_0219,viirs_0226,viirs_0311)

# reformat
viirs_stack_v2 <-crop(viirs_stack_v1, ext(coh_80m))
viirs_stack_v3 <-mask(resample(viirs_stack_v2, coh_80m, method = 'bilinear'),coh_80m) # categorigcal, so near

# save
# writeRaster(viirs_stack_v3[[2]], "./new_optical/p1_80m_20200131_20200212/viirs_0212_80m.tif")
# writeRaster(viirs_stack_v3[[3]], "./new_optical/p2_80m_20200212_20200219/viirs_0219_80m.tif")
# writeRaster(viirs_stack_v3[[4]], "./new_optical/p3_80m_20200219_20200226/viirs_0226_80m.tif")
# writeRaster(viirs_stack_v3[[5]], "./new_optical/p4_80m_20200226_20200311/viirs_0311_80m.tif")

################
#### modscag #####
################

# p1: jan 21 -- feb 12
modscag_0131 <-rast("./modscag/sierra/modscag_reproj_20200131.tif")
modscag_0212 <-rast("./modscag/sierra/modscag_reproj_20200212.tif")

# p2
modscag_0219 <-rast("./modscag/sierra/modscag_reproj_20200219.tif")

# p3
modscag_0226 <-rast("./modscag/sierra/modscag_reproj_20200226.tif")

# p4
modscag_0311 <-rast("./modscag/sierra/modscag_reproj_20200311.tif")

# stack 
modscag_stack_v1 <-c(modscag_0131,modscag_0212,modscag_0219,modscag_0226,modscag_0311)

# reformat
modscag_stack_v2 <-crop(modscag_stack_v1, ext(coh_80m))
modscag_stack_v3 <-mask(resample(modscag_stack_v2, coh_80m, method = 'bilinear'),coh_80m) # categorigcal, so near
plot(modscag_stack_v3)

# save
# writeRaster(modscag_stack_v3[[2]], "./new_optical/p1_80m_20200131_20200212/modscag_0212_80m.tif")
# writeRaster(modscag_stack_v3[[3]], "./new_optical/p2_80m_20200212_20200219/modscag_0219_80m.tif")
# writeRaster(modscag_stack_v3[[4]], "./new_optical/p3_80m_20200219_20200226/modscag_0226_80m.tif")
# writeRaster(modscag_stack_v3[[5]], "./new_optical/p4_80m_20200226_20200311/modscag_0311_80m.tif")

