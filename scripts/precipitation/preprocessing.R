# Subset precipitation data in JMT region 

# ----------------- # 
#      set up      # 
# ----------------- # 
# TO DO: Import directly from the source data 
# set working directory to project directory
library(tidyverse)
library(ggplot2)
library(maps)
library(dplyr)
library(RColorBrewer)
library(plotly)

# ----------------- # 
#   Import data     # 
# ----------------- # 
# TO DO: Import directly from the source data 
load("data/GHCN/precip_allStations.RData") # 50K days x 20K stations
load("data/GHCN/CONUS_allStationsDF.RData") # 20K stations x 10 location info
load("data/GHCN/timeDF.RData") # 50K days x 4 time 


# ----------------- # 
#      Make DF      # 
# ----------------- # 
year <- 2015
location <- "CA"
 
# filter precipitation data by year and location
precip_data <- precip_allStations[timeDF$season.year == year, ]
station_ind <- which(CONUS_allStationsDF$state == "CA")
precip_data <- precip_data[, station_ind]


# bind precip and time data 
time_data <- timeDF[timeDF$season.year == year, ]
precip_data <- cbind(time_data, precip_data)
rownames(precip_data) <- NULL
colnames(precip_data)[-(1:4)] <- CONUS_allStationsDF$stationID[station_ind]


# assign season 
precip_data <- precip_data %>% 
  mutate(season = ifelse(raw.month %in% c(12, 1, 2), "winter", 
                  ifelse(raw.month %in% c(3, 4, 5), "spring", 
                  ifelse(raw.month %in% c(6, 7, 8), "summer", 
                  ifelse(raw.month %in% c(9, 10, 11), "fall", NA)))))

# gather stationID
m <- length(station_ind); m 
first_station <- CONUS_allStationsDF$stationID[station_ind][1]
last_station <- CONUS_allStationsDF$stationID[station_ind][m]

precip_gather <- precip_data %>% 
  gather(stationID, precipitation, first_station:last_station) 


# assign station location
precip <- inner_join(precip_gather, CONUS_allStationsDF, by = "stationID")

# remove stations with no measurements 
stations_to_keep <- precip %>% 
  group_by(stationID) %>% 
  dplyr::summarise(n = n(), 
                   num_missing = sum(is.na(precipitation))) %>% 
  dplyr::filter(num_missing < 365) %>% 
  select(stationID)

precip <- precip %>% 
  dplyr::filter(stationID %in% stations_to_keep$stationID)


# compute seasonal mean per station
seasonal_df <- precip %>% 
  group_by(season.year, season, stationID) %>% 
  dplyr::summarise(
    mean = mean(precipitation, na.rm = T)
  )


# convert NaN and Inf to NA
seasonal_df$mean[is.nan(seasonal_df$mean)] <- NA


# 
seasonal_df <- inner_join(seasonal_df, precip, by = c("stationID", "season.year", "season"))

seasonal_df$time <- as.Date(as.character(seasonal_df$time))





