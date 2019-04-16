source("scripts/googledrive_read_write_functions.R")

require(leaflet)
require(sp)
require(htmltools)

# Get vector data from google drive to plot ###############
  # Actual JMT trail vector and entry trails
    jmt_trail <- load_rgdal_from_googledrive("1RJPvOwVY1mcfjKh1eVybZHfWuIX1jE23", "Trail Edges")
      jmt_trail <- spTransform(jmt_trail, "+proj=longlat +ellps=GRS80 +no_defs")
      
  #Subset trails to main JMT or access trails    
    jmt_access <- jmt_trail[which(jmt_trail$Type == "Access"),]
    jmt_main <- jmt_trail[which(jmt_trail$Type == "Main"),]
    
  # JMT Stream crossing points 
    jmt_crossings <- load_rgdal_from_googledrive("1klB4m5GQVIv7sVaZnZzbbomnkqpfDih2")
      jmt_crossings_simplify <- data.frame(lon = jmt_crossings@coords[,1], 
                                           lat = jmt_crossings@coords[,2], 
                                           Crossing = as.character(jmt_crossings$JMT_Cross))

  # Stream network
    
    
  # Watersheds upstream of stream crossings  
    jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")
      jmt_watersheds <- spTransform(jmt_watersheds, "+proj=longlat +ellps=GRS80 +no_defs")

# Make icon for stream crossings ############      
crossingIcon <- makeIcon(
  iconUrl = "Plots_Viz/River_Icon/Artboard 1.png",
  iconRetinaUrl = "Plots_Viz/River_Icon/Artboard 1@2x.png",
  iconHeight = 35, iconWidth = 20 
)

# Leaflet object #################
  leaflet() %>% 
    setView(lng = -118.869194, lat = 37.235921, zoom = 8) %>% 
    addProviderTiles(provider = "Esri.WorldTopoMap") %>% 
    addPolylines(data = jmt_access, color = "green", label = ~htmlEscape(Name),
                 weight = 2, opacity = 0.75) %>% 
    addPolylines(data = jmt_main, color = "brown", label = ~htmlEscape(Name),
                 weight = 4, opacity = 0.9) %>%
    addMarkers(~lon, ~lat, label = ~htmlEscape(Crossing), icon = ~crossingIcon,
               data = jmt_crossings_simplify) %>% 
    #addPopups(~lon, ~lat, popup = ~htmlEscape(Crossing),
    #           data = jmt_crossings_simplify) %>%    
    addPolygons(data = jmt_watersheds, color = "blue",
                opacity = 0.9, weight = 1, fillOpacity = 0.2)
  
# todos:
  # Add popup info for stream crossings
  # Color crossings based on risk?
    # Categorize risk based on??