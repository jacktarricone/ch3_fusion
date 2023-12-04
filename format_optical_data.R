# crop, mask, and resample non-landsat optical data to the four dates
# jack tarricone

library(terra)

setwd("~/ch3_fusion/rasters")

# bring in sim nisar
# bring in coh from masking and cropping
sierra <-vect("~/ch3_fusion/shapefiles/sierra_multiseg_shp.gpkg")
coh_80m <-rast("./new_uavsar/p1_80m/p1_14d_VV_coh_80m.tif")
coh_reproj <-project(coh_80m, 'EPSG:32611', method = 'bilinear')
plot(coh_reproj)

#### flm
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
flm_stack_v2 <-mask(resample(project(flm_stack_v2, coh_80m, method = 'bilinear'),coh_80m), coh_80m)
flm_stack_v2
plot(flm_stack_v2)

# save
writeRaster(flm_stack_v2[[2]], "./new_optical/p1_80m_20200131_20200212/flm_0212_80m.tif")
writeRaster(flm_stack_v2[[3]], "./new_optical/p2_80m_20200212_20200219/flm_0219_80m.tif")
writeRaster(flm_stack_v2[[4]], "./new_optical/p3_80m_20200219_20200226/flm_0226_80m.tif")
writeRaster(flm_stack_v2[[5]], "./new_optical/p4_80m_20200226_20200311/flm_0311_80m.tif")
