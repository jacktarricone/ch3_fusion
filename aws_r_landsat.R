library(aws.s3)
library(terra)

# this works
get_bucket(
  prefix = "collection02/SNOW/",
  bucket = "usgs-landsat-level-3", 
  headers = list('x-amz-request-payer' = "requester"),
  verbose = TRUE
  )

# system('aws s3 ls --request-payer requester s3://usgs-landsat-level-3/collection02/')
# 
# setwd('/Users/jacktarricone/ch3_fusion/landsat_fsca/h3_v8/')
# list.files()
# system('aws s3 ls --request-payer requester s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/')
# 
# system('aws s3 cp s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/ . --recursive --request-payer') 
