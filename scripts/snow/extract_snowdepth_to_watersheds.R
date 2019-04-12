# Script to extract snow depth characteristics in each watershed upstream of stream crossings for all of 2015

source("scripts/googledrive_read_write_functions.R")

require(sf)
require(lubridate)
require(raster)
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
snodas_watershed_year <- function(dates, varble){
  jmt_fill <- as.data.frame(matrix(nrow = length(dates),
                                   ncol = nrow(jmt_watersheds)+1))
  
  jmt_fill[,1] <- as.character(dates)
  
    jmt_watersheds_swe <- t(sapply(dates, 
                                   get_snodas_day, 
                                   variable = varble, summary_fun = sum))
  
  jmt_fill[,2:ncol(jmt_fill)] <- jmt_watersheds_swe
  
  colnames(jmt_fill) <- c("Date", as.character(jmt_watersheds$crossing))
  
  jmt_fill <- jmt_fill %>% mutate(Date = ymd(Date))
  
  return(jmt_fill)
}

#Get 2015 data (year we have survey data) ##############
jmt_swe_2015 <- snodas_watershed_year(dates_2015, "SWE")  

# Visualization of total SWE in each watershed over time for 2015 
  jmt_swe_2015 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom")
  
# Visualization of SWE melt in each watershed over time
  jmt_swe_2015 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
    ggplot(aes(x = Date, y = SWE_melt, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom")
  
#Save two datasets
  #SWE in each watershed over time in wide format
    write_csv_to_googledrive(jmt_swe_2015, "jmt_watersheds_SWE_2015", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")
  
  #SWE and SWE melt in each watershed over time in long format
  jmt_swe_long <- jmt_swe_2015 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
      select(-last_swe)
  
    write_csv_to_googledrive(jmt_swe_long, "jmt_watersheds_SWE_2015_long", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")

#Get and save data from 2016 ###############
  jmt_swe_2016 <- snodas_watershed_year(seq(ymd("2016-01-01"), ymd("2016-12-31"), "days"), "SWE")  
    
# Visualization of total SWE in each watershed over time for 2015 
  jmt_swe_2016 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Snow water equivalent in JMT watersheds, 2016")
    
    
  #Save 
  #SWE in each watershed over time in wide format
    write_csv_to_googledrive(jmt_swe_2016, "jmt_watersheds_SWE_2016", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")
  
  #SWE and SWE melt in each watershed over time in long format
  jmt_swe_long <- jmt_swe_2016 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
      select(-last_swe)
  
    write_csv_to_googledrive(jmt_swe_long, "jmt_watersheds_SWE_2016_long", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")

#Get and save data from 2017 ##############
  jmt_swe_2017 <- snodas_watershed_year(seq(ymd("2017-01-01"), ymd("2017-12-31"), "days"), "SWE")  
    
# Visualization of total SWE in each watershed over time for 2015 
  jmt_swe_2017 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Snow water equivalent in JMT watersheds, 2017")

  #Save
  #SWE in each watershed over time in wide format
    write_csv_to_googledrive(jmt_swe_2017, "jmt_watersheds_SWE_2017", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")
  
  #SWE and SWE melt in each watershed over time in long format
  jmt_swe_long <- jmt_swe_2017 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
      select(-last_swe)
  
    write_csv_to_googledrive(jmt_swe_long, "jmt_watersheds_SWE_2017_long", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")

#Get and save data from 2018 ###################    
  jmt_swe_2018 <- snodas_watershed_year(seq(ymd("2018-01-01"), ymd("2018-12-31"), "days"), "SWE")  
    
# Visualization of total SWE in each watershed over time for 2015 
  jmt_swe_2018 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Snow water equivalent in JMT watersheds, 2018")
  
  #Save
  #SWE in each watershed over time in wide format
    write_csv_to_googledrive(jmt_swe_2018, "jmt_watersheds_SWE_2018", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")
  
  #SWE and SWE melt in each watershed over time in long format
  jmt_swe_long <- jmt_swe_2018 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
      select(-last_swe)
  
    write_csv_to_googledrive(jmt_swe_long, "jmt_watersheds_SWE_2018_long", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")
