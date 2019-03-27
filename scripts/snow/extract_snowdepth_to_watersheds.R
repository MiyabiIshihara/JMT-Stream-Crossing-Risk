# Script to extract snow depth characteristics in each watershed upstream of stream crossings for all of 2015

source("scripts/load_data_from_googledrive.R")

require(sf)
require(tidyverse)

# Get watersheds
jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")

# Get data frame of googledrive ids for all the snowdepth rasters
snodas_gd <- drive_ls(as_id("1_IxGme096iUx6JJQY0nhONKSzBWaiI3k"))

#function to extract sum of snowdepth in each watershed on a particular day
get_snowdepth_day <- function(date){
  snodas_id <- snodas_gd %>% 
    slice(grep(date, name)) %>% 
    pull(id)
  
  snodas_data <- load_geotiff_from_googledrive(snodas_id)
  
  
}
test_grid <- load_geotiff_from_googledrive("1qtyTTdCHCd3ZJROVFW9712dPX1QGzVaN")
