# static visualization of precipitation 


# 
library(usmap)
library(ggplot2)

state_df <- map_data("state", region = "California")
ggplot(data = seasonal_df, aes(x = longitude, y = latitude)) + 
  geom_point(aes(stationID = stationID), size = 1) + 
  geom_polygon(data = state_df, aes(x = long, y = lat, group = group), 
               fill = NA, color = "grey50")

ggplot(data = seasonal_df, aes(x = longitude, y = latitude)) + 
  geom_point(aes(stationID = stationID, col = mean), size = 1) + 
  geom_polygon(data = state_df, aes(x = long, y = lat, group = group), 
               fill = NA, color = "grey50") + 
  facet_grid(. ~ season)


head(seasonal_df)