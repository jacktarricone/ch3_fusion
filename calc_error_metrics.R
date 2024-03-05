# calc error metrics

#### calc error metics
sar_dpo_vals <-terra::extract(dswe, dpo_cells)
sar_dpo_mean <-mean(colMeans(sar_dpo_vals, na.rm = TRUE), na.rm = TRUE)

sar_mhp_vals <-terra::extract(dswe, mhp_cells)
sar_mhp_mean <-mean(colMeans(sar_mhp_vals, na.rm = TRUE), na.rm = TRUE)

sar_ubc_vals <-terra::extract(dswe, ubc_cells)
sar_ubc_mean <-mean(colMeans(sar_ubc_vals,  na.rm = TRUE), na.rm = TRUE)

sar_vlc_vals <-terra::extract(dswe, vlc_cells)
sar_vlc_mean <-mean(colMeans(sar_vlc_vals, na.rm = TRUE), na.rm = TRUE)

sar_swe <-c(sar_dpo_mean,sar_mhp_mean,sar_ubc_mean,sar_vlc_mean)

# calc error metrics
mae <-Metrics::mae(station_dswe$dswe_cm,sar_swe)
rmse <-Metrics::rmse(station_dswe$dswe_cm,sar_swe)