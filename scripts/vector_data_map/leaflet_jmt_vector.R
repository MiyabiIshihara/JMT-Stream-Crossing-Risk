source("scripts/googledrive_read_write_functions.R")

require(leaflet)
require(sp)
require(htmltools)

#First source all the vector data that's located on googledrive
  # Actual JMT trail vector and entry trails
    jmt_trail <- load_rgdal_from_googledrive("1RJPvOwVY1mcfjKh1eVybZHfWuIX1jE23", "Trail Edges")
      jmt_trail <- spTransform(jmt_trail, "+proj=longlat +ellps=GRS80 +no_defs")
  # JMT Stream crossing points 
    jmt_crossings <- load_rgdal_from_googledrive("1klB4m5GQVIv7sVaZnZzbbomnkqpfDih2")
      jmt_crossings_simplify <- data.frame(lon = jmt_crossings@coords[,1], 
                                           lat = jmt_crossings@coords[,2], 
                                           Crossing = as.character(jmt_crossings$JMT_Cross))

  # Stream network
    
    
  # Watersheds upstream of stream crossings  
    jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")
      jmt_watersheds <- spTransform(jmt_watersheds, "+proj=longlat +ellps=GRS80 +no_defs")

#Leaflet object
  leaflet() %>% 
    setView(lng = -118.869194, lat = 37.235921, zoom = 10) %>% 
    addProviderTiles(provider = "Esri.WorldTopoMap") %>% 
    addPolylines(data = jmt_trail, color = "red") %>% 
    addMarkers(~lon, ~lat, label = ~htmlEscape(Crossing), data = jmt_crossings_simplify) %>% 
    addPolygons(data = jmt_watersheds, color = "blue")
  
# todos:
  # Change icon for stream crossings
  # Add markers/labels to watersheds
  # Color crossings based on risk
    # Categorize risk based on??
  # Color Trail edges based on main JMT vs entry/resupply/auxiliary trails