
require(leaflet)
require(sp)
require(sf)
require(raster)
require(htmltools)
require(lubridate)
require(tidyverse)

source("scripts/googledrive_read_write_functions.R")

# Get vector data from google drive to plot ###############
  # Actual JMT trail vector and entry trails
    jmt_trail <- load_rgdal_from_googledrive("1RJPvOwVY1mcfjKh1eVybZHfWuIX1jE23", "Trail Edges")
      jmt_trail <- spTransform(jmt_trail, "+proj=longlat +ellps=GRS80 +no_defs") %>% st_as_sf() %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
  #Subset trails to main JMT or access trails    
    jmt_access <- jmt_trail[which(jmt_trail$Type == "Access"),]
      saveRDS(jmt_access, "scripts/Shiny_App/Data/jmt_access_trails.rds") 
      
    jmt_main <- jmt_trail[which(jmt_trail$Type == "Main"),]
      saveRDS(jmt_main, "scripts/Shiny_App/Data/jmt_main_trail.rds")  
    
  # JMT Stream crossing points are processed in crossings_add_field.R

  # Watersheds upstream of stream crossings  
    jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")
      jmt_watersheds <- spTransform(jmt_watersheds, "+proj=longlat +ellps=GRS80 +no_defs") %>% st_as_sf() %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      saveRDS(jmt_watersheds, "scripts/Shiny_App/Data/jmt_watersheds.rds")  
  

#Download and process snowdepth rasters ##########
# Get data frame of googledrive ids for all the snowdepth rasters
  snodas_gd_depth <- drive_ls(as_id("1_IxGme096iUx6JJQY0nhONKSzBWaiI3k"))
      
#JMT extent  
  jmt_clipper <- extent(-120, -118, 36, 38)
      
#function to get snowdepth geotiff on particular day
get_snodas_tif <- function(date){
    snodas_id <- snodas_gd_depth %>% 
      slice(grep(date, name)) %>% 
      pull(id)
  
    snodas_data <- load_geotiff_from_googledrive(snodas_id)

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
      
#Get precip data ########
# Get data frame of googledrive ids for all the snowdepth rasters
  gd_precip <- drive_ls(as_id("16TsvJGV4YNxG6EER2RokJTOZMFurhsX4"))
      
# Function to download snowdepth raster and clip it
  precip_jmt_clip <- function(date){
    #Convert date input to character with no dashes
    date_char <- gsub("-", "", as.character(date))
    
    #Find folder within googledrive that contains date
    bil_id <- gd_precip$id[grepl(date_char, gd_precip$name)]
    
    print(c(date_char, bil_id))
    
    #Download raster from .bil in above folder
    to_clip <- load_raster_from_bil_googledrive(bil_id)
    
    #clip raster and return
    clipped <- crop(to_clip, jmt_clipper)
    
    return(clipped)
  }
  
# 2015 precipitation
  precip_2015_jmt <- lapply(dates_2015, precip_jmt_clip)
  
  saveRDS(precip_2015_jmt, "scripts/Shiny_App/Data/precip_2015.rds")    
