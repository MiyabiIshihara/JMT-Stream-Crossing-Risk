# Script to extract snow depth characteristics in each watershed upstream of stream crossings for all of 2015

source("scripts/googledrive_read_write_functions.R")

require(sf)
require(lubridate)
require(raster)
require(mapview)
require(tidyverse)

# Get watersheds and dem and assign same coordinate system
jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")

jmt_dem <- load_geotiff_from_googledrive("1mp-x1b5BtJ_hUXfPEcP_3pPhiliHMU6I")

jmt_watersheds_sf <- st_as_sf(jmt_watersheds) %>% 
  st_transform(st_crs(jmt_dem))

crop_dem_to_watershed <- function(dem, watershed){
  init_crop <- raster::crop(dem, watershed)
  real_crop <- raster::mask(init_crop, watershed)
}

watershed_elevation_pct <- function(watershed_name, elevation){
  #Get watershed shapefile from watershed name  
    watershed_shape <- jmt_watersheds_sf %>% filter(crossing == watershed_name)
  
  #Crop raster to watershed
    cropped_raster <- crop_dem_to_watershed(jmt_dem, watershed_shape)
  
  #Return percent of watershed above the elevation threshold
    sum(cropped_raster[] > elevation, na.rm = T) / sum(cropped_raster[] > 0, na.rm = T)
}

watersheds_elevations <- expand.grid(watershed_name = unique(jmt_watersheds_sf$crossing),
                                     elevation = seq(2500, 4500, by = 25)) %>% 
  left_join(jmt_watersheds_sf %>% st_set_geometry(NULL), by = c("watershed_name" = "crossing")) %>% 
  mutate(elevation_ft = elevation * 3.28084,
         pct_above = map2_dbl(watershed_name, elevation, watershed_elevation_pct) * 100,
         area_km2 = area*1e-6)


watersheds_elevations %>% 
  mutate(Crossing = watershed_name) %>% 
  filter(area_km2 > 10 & 
           watershed_name != "Rosemarie Meadow" &
           watershed_name != "Evolution Lake inlet" &
           watershed_name != "Arrowhead Lake outlet") %>% 
  ggplot(aes(x = elevation_ft, y = pct_above, col = Crossing, size = area_km2)) + 
    geom_line() +
    scale_color_brewer(palette = "Paired") +
    labs(x = "Elevation (ft)",
         y = "Percent of watershed above elevation",
         title = "Elevation profiles of JMT watersheds",
         subtitle = "above key crossing points") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.title = element_text(14),
          axis.text = element_text(10)) +
    guides(size = guide_legend(title = expression("Watershed\nArea"~(km^2))))

ggsave("Plots_Viz/elevation_watershed_area.png",
       height = 6, width = 10, units = "in", dpi = 300)    
