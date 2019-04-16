source("scripts/googledrive_read_write_functions.R")

require(leaflet)
require(sp)
require(sf)
require(htmltools)
require(tidyverse)

# Get trail-related vector data from google drive to plot ###############
  # Actual JMT trail vector and entry trails
    jmt_trail <- load_rgdal_from_googledrive("1RJPvOwVY1mcfjKh1eVybZHfWuIX1jE23", "Trail Edges")
      jmt_trail <- spTransform(jmt_trail, "+proj=longlat +ellps=GRS80 +no_defs") %>% st_as_sf() %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
  #Subset trails to main JMT or access trails    
    jmt_access <- jmt_trail[which(jmt_trail$Type == "Access"),]
    jmt_main <- jmt_trail[which(jmt_trail$Type == "Main"),]
    
  # JMT Stream crossing points 
    jmt_crossings <- load_rgdal_from_googledrive("1klB4m5GQVIv7sVaZnZzbbomnkqpfDih2")
      jmt_crossings_simplify <- data.frame(lon = jmt_crossings@coords[,1], 
                                           lat = jmt_crossings@coords[,2], 
                                           Crossing = as.character(jmt_crossings$JMT_Cross),
                                           popup_field = as.character(jmt_crossings$Down_Name)) %>% 
        st_as_sf(coords = c("lon", "lat")) %>% 
        st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
  # Watersheds upstream of stream crossings  
    jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")
      jmt_watersheds <- spTransform(jmt_watersheds, "+proj=longlat +ellps=GRS80 +no_defs") %>% st_as_sf() %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Make icon for stream crossings ############      
crossingIcon <- makeIcon(
  iconUrl = "Plots_Viz/River_Icon/Artboard 1.png",
  iconRetinaUrl = "Plots_Viz/River_Icon/Artboard 1@2x.png",
  iconHeight = 35, iconWidth = 20 
)

#Get precip data ########
  temp <- tempfile(fileext = ".Rdata")
  dl <- drive_download(as_id("1vkoGl-35k4BDk-9lM7_rw7sSXXX_8k9b"), path = temp, overwrite = TRUE)
  load(temp)
  unlink(temp)
      
#Get Snodas data ###########
  
#Save entire workspace to Rdata object   
save.image(file = "scripts/Shiny_App/shiny_data.Rdata")  