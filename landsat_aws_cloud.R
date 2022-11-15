library(aws.s3)
library(terra)

Sys.setenv ("AWS_ACCESS_KEY_ID" = "AKIATNEUCCTMPQSQV7HG",
            "AWS_SECRET_ACCESS_KEY" = "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",
            "AWS_DEFAULT_REGION" = "us-west-2")

get_bucket(bucket = "sentinel-cogs", check_region = FALSE, url_style = "path", verbose = TRUE)

cog <- 's3://usgs-landsat/collection02/level-2/standard/oli-tirs/2020/026/027/LC08_L2SP_026027_20200827_20200906_02_T1/LC08_L2SP_026027_20200827_20200906_02_T1_SR_B2.TIF'
test <-rast(cog)
