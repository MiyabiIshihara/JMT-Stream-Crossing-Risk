
require(leaflet)
require(sp)
require(sf)
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
  

#Function to get snowdepth raster for particular day ##########
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
      
# Make icon for stream crossings ############      
crossingIcon <- makeIcon(
  iconUrl = "River_Icon/Artboard 1.png",
  iconRetinaUrl = "River_Icon/Artboard 1@2x.png",
  iconHeight = 35, iconWidth = 20 
)

#Get precip data ########
  #temp <- tempfile(fileext = ".Rdata")
  #dl <- drive_download(as_id("1vkoGl-35k4BDk-9lM7_rw7sSXXX_8k9b"), path = temp, overwrite = TRUE)
  #load(temp)
  #unlink(temp)