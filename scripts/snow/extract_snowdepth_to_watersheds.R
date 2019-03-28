# Script to extract snow depth characteristics in each watershed upstream of stream crossings for all of 2015

source("scripts/load_data_from_googledrive.R")

require(sf)
require(tidyverse)

# Get watersheds
jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")

# Get data frame of googledrive ids for all the snowdepth rasters
snodas_gd_depth <- drive_ls(as_id("1_IxGme096iUx6JJQY0nhONKSzBWaiI3k"))
snodas_gd_swe <- drive_ls(as_id("1JPXf6Pq9Ki9zjTSctvO2UxfRn2KTUiq4"))

# 2015 dates
dates_2015 <- seq(ymd("2015-01-01"), ymd("2015-12-31"), "days")

#function to extract sum of snowdepth in each watershed on a particular day
get_snodas_day <- function(date, variable, summary_fun){
  if(variable == "SWE"){
    snodas_id <- snodas_gd_swe %>% 
      slice(grep(date, name)) %>% 
      pull(id)
    
    snodas_data <- load_geotiff_from_googledrive(snodas_id)

      return(t(raster::extract(snodas_data, jmt_watersheds, fun = summary_fun, na.rm = T)))
    
  } else if(variable == "snowDepth"){
    snodas_id <- snodas_gd_depth %>% 
      slice(grep(date, name)) %>% 
      pull(id)
  
    snodas_data <- load_geotiff_from_googledrive(snodas_id)

      return(t(raster::extract(snodas_data, jmt_watersheds, fun = summary_fun, na.rm = T)))

  } else {
    return("Variable must be SWE or snowDepth")
  }
  
}

#Data frame to fill with total snow water equivalent estimates over time
  jmt_swe_2015 <- as.data.frame(matrix(nrow = length(dates_2015),
                                       ncol = nrow(jmt_watersheds)+1))
    jmt_swe_2015[,1] <- as.character(dates_2015)

  jmt_fill <- t(sapply(dates_2015, get_snodas_day, variable = "SWE", summary_fun = sum))
  
  jmt_swe_2015[,2:26] <- jmt_fill

  colnames(jmt_swe_2015) <- c("Date", as.character(jmt_watersheds$crossing))
  
# Visualization of total SWE in each watershed over time
  jmt_swe_2015 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = ymd(Date), y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom")
  
# Visualization of SWE melt in each watershed over time
  jmt_swe_2015 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
    ggplot(aes(x = ymd(Date), y = SWE_melt, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom")
  