require(tidyverse)
require(sf)

source("scripts/googledrive_read_write_functions.R")

# JMT Stream crossing points 
  jmt_crossings <- load_rgdal_from_googledrive("1klB4m5GQVIv7sVaZnZzbbomnkqpfDih2")
  
  crossings_df <- jmt_crossings@data
  
  crossings_df <- crossings_df %>% 
    mutate(description = case_when(JMT_Cross == "Arctic Lake outlet" ~ "Can be a wade at peak flows, hop across on rocks at low flows",
                                   JMT_Cross == "Wallace Creek" ~ "Relatively long and deep, usually a wade",
                                   JMT_Cross == "Wright Creek" ~ "Broad and deep, usually a wade",
                                   JMT_Cross == "Tyndall Creek" ~ "Crossing can be hazardous at peak flows, take care and search upstream for easier crossing points if needed",
                                   JMT_Cross == "Bubbs Creek" ~ "Can be swift and moderately deep, take care",
                                   JMT_Cross == "Rae Lakes outlet" ~ "Deep, but slow moving wade across large boulders along the bottom",
                                   JMT_Cross == "Arrowhead lake outlet" ~ "Deep and can be moderately siwft with run out. Avoid by going west of the lake and crossing at the south side",
                                   JMT_Cross == "White Fork" ~ "Can be swift and moderately deep at peak flows, take care",
                                   JMT_Cross == "South Fork Kings River" ~ "Extremely hazardous and impassable at peak flows. Look downstream for crossing points across islands where the channel divides or avoid by remaining on the east side of the river and crossing further north in the Upper Basin",
                                   JMT_Cross == "Helen Lake outlet" ~ "Possible wade across boulders at peak flows",
                                   JMT_Cross == "Wanda Lake outlet" ~ "Broad and shallow, but can be a wade at peak flows",
                                   JMT_Cross == "Evolution Lake inlet" ~ "Long crossing on large boulders that are only submerged under very high flows",
                                   JMT_Cross == "Evolution Creek" ~ "Can be very hazardous at high flows, can opt for the meadow crossing which is always a wade, but slow-moving",
                                   JMT_Cross == "Senger Creek" ~ "Possible wade at high flows, otherwise cross on rocks",
                                   JMT_Cross == "Sallie Keyes Lakes outlet" ~ "Possible log crossings, otherwise find diffuse channels just downstream to cross",
                                   JMT_Cross == "Rosemarie Meadow" ~ "Possible wade at high flows",
                                   JMT_Cross == "Bear Creek" ~ "Deep and fast moving, very hazardous at peak flows. Search for diffuse channels or a large log downstream to cross on",
                                   JMT_Cross == "Hilgard Branch" ~ "Can be a challenging wade at peak flows",
                                   JMT_Cross == "Silver Pass Creek lower" ~ "Can be a wade at peak flows, hop across on rocks at low flows",
                                   JMT_Cross == "Silver Pass Creek middle" ~ "Infamous waterfall crossing. Not particularly challenging, but take care due to steep cascades below",
                                   JMT_Cross == "Silver Pass Creek upper" ~ "Can be a wade at peak flows, hop across on rocks at low flows",
                                   JMT_Cross == "Mott Creek" ~ "Can be a challenging wade at peak flows",
                                   JMT_Cross == "Deer Creek" ~ "Possible log crossing, non-challenging wade otherwise",
                                   JMT_Cross == "Lyell Fork" ~ "Cross on large intentionally-placed boulders. Can be disconcerting if boulders submerged, long steps across and long crossing",
                                   JMT_Cross == "Ireland Creek" ~ "Can be a wade, but typically cross on logs and rocks (can be slippery)"))
  
  jmt_crossings@data <- crossings_df
    jmt_crossings_simplify <- data.frame(lon = jmt_crossings@coords[,1], 
                                         lat = jmt_crossings@coords[,2], 
                                         Crossing = as.character(jmt_crossings$JMT_Cross),
                                         popup_field = as.character(jmt_crossings$description)) %>% 
      st_as_sf(coords = c("lon", "lat"))

  saveRDS(jmt_crossings_simplify, file = "scripts/Shiny_App/Data/crossing_points.rds")  
  