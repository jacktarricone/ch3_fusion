library(aws.s3)
library(terra)

# this works, but no raster interface for direct s3 reading, sad!
buck <-get_bucket_df(
  prefix = "collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/",
  bucket = "usgs-landsat-level-3", 
  headers = list('x-amz-request-payer' = "requester"),
  verbose = TRUE
  )

#### therefore we'll download using aws cli call
# starting with ARD horizontal box 3
# and vertical 8, 9, and 10

## 2020

## h3_v8
# setwd("/Users/jacktarricone/ch3_fusion/landsat_fsca/h3_v8/")
# system('aws s3 cp s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/009/ . --recursive --request-payer') 
list.files()

## h3_v9
setwd("/Users/jacktarricone/ch3_fusion/landsat_fsca/h3_v9/")
# system('aws s3 cp s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/009/ . --recursive --request-payer') 

## h3_v10
setwd("/Users/jacktarricone/ch3_fusion/landsat_fsca/h3_v10/")
# system('aws s3 cp s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/010/ . --recursive --request-payer') 

