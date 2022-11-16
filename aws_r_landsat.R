library(aws.s3)
library(terra)

# this works
buck <-get_bucket_df(
  prefix = "collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/",
  bucket = "usgs-landsat-level-3", 
  headers = list('x-amz-request-payer' = "requester"),
  verbose = TRUE
  )

buck

url <- "s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/LC08_CU_003008_20200107_20210504_02_GROUND_SNOW.TIF"
test <-rast(url, drivers = 'COG') 

test <- s3read_using(FUN = terra::rast, 
                   object = "s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/LC08_CU_003008_20200107_20210504_02_GROUND_SNOW.TIF", 
                   # prefix = "collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/",
                   # bucket = "usgs-landsat-level-3", 
                   headers = list('x-amz-request-payer' = "requester"),
                   verbose = TRUE)


{
s3load(buck$Key[1], 
              prefix = "collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/",
              bucket = "usgs-landsat-level-3", 
              headers = list('x-amz-request-payer' = "requester"),
              verbose = TRUE,
              envir=(temp_env <- new.env()))
  
as.list.environment(temp_env)
}

# return object using 'S3 URI' syntax, with progress bar
get_object(object = 's3://usgs-landsat-level-3/collection02/SNOW/2019/CU/003/010/LC08_CU_003010_20191231_20210504_02_SNOW/',
           #prefix = "collection02/SNOW/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/",
           #bucket = "usgs-landsat-level-3", 
           headers = list('x-amz-request-payer' = "requester"),
           verbose = TRUE,
           show_progress = TRUE)

s3_url <- 's3://usgs-landsat-level-3/collection02/SNOW/2019/CU/003/010/LC08_CU_003010_20191231_20210504_02_SNOW/LC08_CU_003010_20191231_20210504_02_GROUND_SNOW.TIF'
test <-rast(s3_url)

## comand line calls
# system('aws s3 ls --request-payer requester s3://usgs-landsat-level-3/collection02/')
# system('aws s3 ls --request-payer requester s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/008/LC08_CU_003008_20200107_20210504_02_SNOW/')

setwd("/Users/jacktarricone/ch3_fusion/landsat_fsca/h3_v8/")
system('aws s3 cp s3://usgs-landsat-level-3/collection02/SNOW/2020/CU/003/008/ . --recursive --request-payer') 

setwd("/Users/jacktarricone/ch3_fusion/landsat_fsca/h3_v8/")
list.files()
