require(raster)
require(sp)
require(sf)
require(lubridate)
require(tidyverse)

source("scripts/googledrive_read_write_functions.R")

#Function to get snowdepth raster for particular day ##########
# Get data frame of googledrive ids for all the snowdepth rasters
  snodas_gd_depth <- drive_ls(as_id("1_IxGme096iUx6JJQY0nhONKSzBWaiI3k"))
      
#JMT extent  
  jmt_clipper <- extent(-120, -118, 36, 38)
    
# function to get snowdepth geotiff on particular day
  get_snodas_tif <- function(date){
      snodas_id <- snodas_gd_depth %>% 
        slice(grep(date, name)) %>% 
        pull(id)
    
      snodas_data <- load_geotiff_from_googledrive(snodas_id[1])
  
      return(snodas_data)
  }

# Function to download snowdepth raster and clip it
  snodas_jmt_clip <- function(date){
    to_clip <- get_snodas_tif(date)
    
    clipped <- crop(to_clip, jmt_clipper)
    
    return(clipped)
  }
  
# 2015 dates snow depth  
  dates_2015 <- seq(ymd("2015-01-01"), ymd("2015-12-31"), "days")
  
  snow_depth_2015_jmt <- lapply(dates_2015, snodas_jmt_clip)
  
  saveRDS(snow_depth_2015_jmt, "scripts/Shiny_App/Data/snow_depth_2015.rds")