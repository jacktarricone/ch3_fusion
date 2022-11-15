
library(aws.s3)
library(terra)


get_bucket(bucket = "usgs-landsat-level-3", check_region = FALSE, url_style = "path", verbose = TRUE)


r <- s3read_using(FUN = rast, object = s3_url, bucket = "usgs-landsat-level-3")

s3_url = 's3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/009/LC08_CU_003009_20200912_20210504_02_SNOW/LC08_CU_003009_20200912_20210504_02_GROUND_SNOW.TIF'
save_object(s3_url)
test <-rast(s3_url)

