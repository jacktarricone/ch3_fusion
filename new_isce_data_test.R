# resamp fresh uavsar data
# nov 14, 2023

library(terra)

setwd('~/sierra_isce/geocode_test/')

# unwrapped phase
unw <-rast('./20200226T2253_20200311T1852.unw_snaphu.unw.tif.unw.tif')
unw
plot(unw)

filt <- focal(unw,w=3,fun="mean")
writeRaster(filt, "./filt.tif")

# coherence
coh <-rast('./20200226T2253_20200311T1852.coh.tif.coh.tif')
coh
plot(coh)

# concomp
concomp <-rast('./20200226T2253_20200311T1852.unw_snaphu.unw.conncomp.tif.conncomp.tif')
concomp
plot(concomp)
hist(concomp)

mask_test <-mask(filt, concomp, maskvalues = 1, inverse = TRUE)
plot(mask_test)
writeRaster(mask_test, "./filt_masked.tif")

resamp_rast <-rast("~/ch3_fusion/rasters/uavsar/feb26_march11_80m/cor_80m.tif")

isce_resamp <-resample(mask_test, resamp_rast, method = 'bilinear')
isce_resamp
plot(isce_resamp)
writeRaster(isce_resamp, "./filt_masked_80m.tif")

??resample


