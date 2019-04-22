# Import PRISM temperature data 
# Data obtained from: http://prism.oregonstate.edu/recent/

library(prism)
library(raster)
library(lubridate)
library(tidyverse)

source("scripts/googledrive_read_write_functions.R")


#JMT extent  
  jmt_clipper <- extent(-120, -118, 36, 38)

#PRISM options and path  
options(prism.path = "scripts/temperature/")

get_prism_temp <- function(date){
  get_prism_dailys(type = "tmean", dates = date, keepZip = F)
  
  temp_rast <- raster(paste0("scripts/temperature/PRISM_tmean_stable_4kmD1_", 
                             gsub("-", "", as.character(date)), 
                             "_bil", "/PRISM_tmean_stable_4kmD1_", 
                             gsub("-", "", as.character(date)), "_bil.bil"))
  
  clipped_temp_rast <- crop(temp_rast, jmt_clipper)
  
  return(clipped_temp_rast)
  
  file.remove(file.path("scripts/temperature/", list.files("scripts/temperature/")))
}


#2015 PRISM TEMP ###########

# download daily 2015 temperature data from PRISM website (couldn't get Jan 1 to work for some reason)
prism_jmt_temp_2015 <- lapply(seq.Date(ymd("2015-01-02"), ymd("2015-12-31"), "day"),
                              get_prism_temp)

saveRDS(prism_jmt_temp_2015, file = "scripts/Shiny_App/Data/prism_temp_jmt_clip_2015.rds")

# Get watersheds
jmt_watersheds <- load_rgdal_from_googledrive("1yB7ww8YgWCAOHjeuCa4Xu6vIZPthO3aD")

#function to extract mean temperature in each watershed on a particular day
get_watersheds_temp_day <- function(date){
    temp_rast <- prism_jmt_temp_2015[[yday(as.Date(date))-1]]

      return(t(raster::extract(temp_rast, jmt_watersheds, fun = mean, na.rm = T)))
    
}

dates_2015 <- seq.Date(ymd("2015-01-02"), ymd("2015-12-31"), "day")

  watersheds_2015_mean_temp <- as.data.frame(matrix(nrow = length(dates_2015),
                                                    ncol = nrow(jmt_watersheds)+1))
  
  watersheds_2015_mean_temp[,1] <- as.character(dates_2015)
  
  watersheds_2015_mean_temp_filler <- t(sapply(dates_2015, 
                                               get_watersheds_temp_day))
  
  watersheds_2015_mean_temp[,2:ncol(watersheds_2015_mean_temp)] <- watersheds_2015_mean_temp_filler
  
  colnames(watersheds_2015_mean_temp) <- c("Date", as.character(jmt_watersheds$crossing))
  
  watersheds_2015_mean_temp <- watersheds_2015_mean_temp %>% mutate(Date = ymd(Date))

#Save two datasets
  #temperature in each watershed over time in wide format
    write_csv_to_googledrive(watersheds_2015_mean_temp, "jmt_watersheds_mean_temp_2015", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")
  
  #SWE and SWE melt in each watershed over time in long format
  watersheds_2015_mean_temp_long <- watersheds_2015_mean_temp %>% 
    gather("watershed", "Mean_Temp", -Date)
  
    write_csv_to_googledrive(watersheds_2015_mean_temp_long, "jmt_watersheds_mean_temp_2015_long", 
                             folder_id = "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV")

  