# Import PRISM precipitation data 
# Data obtained from: http://prism.oregonstate.edu/recent/

library(prism)
library(raster)
library(rgdal)
library(tidyverse)
library(plyr)
require(readr)



# download daily precipitation data from PRISM website
# get_prism_dailys(type = "ppt", minDate = "2015-01-01", maxDate = "2015-12-31", keepZip = FALSE)

# create a vector of dates 
dates_as_dates <- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = 1)
dates <- gsub("-", "", dates_as_dates)
length(dates)

# import daily precipitation data as a list
data <- precip_data <- list()
ptm <- proc.time()
for(i in 1:length(dates)){
  date <- dates[i]
  # import daily precipitation data
  data[[date]] <- raster(paste0("../../data/PRISM/PRISM_ppt_stable_4kmD2_", 
                date, "_bil", "/PRISM_ppt_stable_4kmD2_", date, "_bil.bil"))
  
  # convert coordinate system to WGS84
  crs(data[[date]]) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

  # crop to spatial extent of interest
  data[[date]] <- crop(data[[date]], extent(-120, -118, 36, 38))
  
  # convert raster data to data frame
  precip_data[[date]] <- as.data.frame(data[[date]], xy = TRUE) %>% 
    mutate(date = as.Date(date, "%Y%m%d"))
  names(precip_data[[date]]) <- c("lat", "lon", "precip", "date")
  
}
proc.time() - ptm


# save(precip_data, file = "data/precip_data.RData")

precip_2015_jmt <- data








