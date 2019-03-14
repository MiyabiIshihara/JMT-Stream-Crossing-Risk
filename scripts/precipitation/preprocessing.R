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
dim(precip_data)
station_ind <- which(CONUS_allStationsDF$state == "CA")
precip_data <- precip_data[, station_ind]
dim(precip_data)


# bind precip and time data 
time_data <- timeDF[timeDF$season.year == year, ]
precip_data <- cbind(time_data, precip_data)
rownames(precip_data) <- NULL
colnames(precip_data)[-(1:4)] <- CONUS_allStationsDF$stationID[station_ind]
dim(precip_data)
precip_data[1:3, 1:5]


# assign season 
precip_data <- precip_data %>% 
  mutate(season = ifelse(raw.month %in% c(12, 1, 2), "winter", 
                  ifelse(raw.month %in% c(3, 4, 5), "spring", 
                  ifelse(raw.month %in% c(6, 7, 8), "summer", 
                  ifelse(raw.month %in% c(9, 10, 11), "fall", NA)))))

# gather stationID
m <- length(stations_of_interest); m
first_station <- stations_of_interest[1]
last_station <- stations_of_interest[m]

precip_gather <- precip_data %>% 
  gather(stationID, precipitation, first_station:last_station) 

dim(precip_gather) 
head(precip_gather)


# assign station location
precip <- inner_join(precip_gather, CONUS_allStationsDF, by = "stationID")
head(precip)
dim(precip)
colnames(precip)


# remove stations with no measurements 
stations_to_keep <- precip %>% 
  group_by(stationID) %>% 
  dplyr::summarise(n = n(), 
                   num_missing = sum(is.na(precipitation))) %>% 
  dplyr::filter(num_missing < 365) %>% 
  select(stationID)

precip <- precip %>% 
  dplyr::filter(stationID %in% stations_to_keep$stationID)

head(precip)
dim(precip)
length(unique(precip$stationID))


# compute seasonal mean per station
seasonal_df <- precip %>% 
  group_by(season.year, season, stationID) %>% 
  dplyr::summarise(
    mean = mean(precipitation, na.rm = T)
  )

head(seasonal_df)
dim(seasonal_df)

# convert NaN and Inf to NA
seasonal_df$mean[is.nan(seasonal_df$mean)] <- NA


# 
seasonal_df <- inner_join(seasonal_df, precip, by = c("stationID", "season.year", "season"))
head(seasonal_df)
dim(seasonal_df)
length(table(seasonal_df$stationID))





