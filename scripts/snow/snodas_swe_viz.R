source("scripts/googledrive_read_write_functions.R")

require(sf)
require(lubridate)
require(raster)
require(tidyverse)

#Get time series of snodas data in JMT watersheds from GoogleDrive
jmt_swe_2015 <- load_csv_from_googledrive("1Py8svcq-YMBUlhKYJR_HnfVGf41dkFyp")
jmt_swe_2016 <- load_csv_from_googledrive("15vpYxV75PpQWGEmhEoTkSvkx07vp5H5P")
jmt_swe_2017 <- load_csv_from_googledrive("1bjlEhCZ9ghEoLtUXmjiOJQKkxKDOk4zS")
jmt_swe_2018 <- load_csv_from_googledrive("1OwX0bljESiE6UCDO3m6J_NlRGlkPpQ9a")

#2015 #########
# Visualization of total SWE in each watershed over time for 2015 
  jmt_swe_2015 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Snow water equivalent in JMT watersheds, 2015")
  
ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_time_series_2015.png", width = 10, height = 6)

# Visualization of SWE melt in each watershed over time
  jmt_swe_2015 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
    ggplot(aes(x = Date, y = SWE_melt, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Daily Change in SWE in JMT watersheds, 2015")

ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_melt_time_series_2015.png", width = 10, height = 6)
  
#2016 ######################  
# Visualization of total SWE in each watershed over time for 2016 
  jmt_swe_2016 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Snow water equivalent in JMT watersheds, 2016")
  
ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_time_series_2016.png", width = 10, height = 6)

# Visualization of SWE melt in each watershed over time
  jmt_swe_2016 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
    ggplot(aes(x = Date, y = SWE_melt, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Daily Change in SWE in JMT watersheds, 2016")
  
ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_melt_time_series_2016.png", width = 10, height = 6)
  
# 2017 #############
# Visualization of total SWE in each watershed over time for 2015 
  jmt_swe_2017 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Snow water equivalent in JMT watersheds, 2017")
  
ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_time_series_2017.png", width = 10, height = 6)

# Visualization of SWE melt in each watershed over time
  jmt_swe_2017 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
    ggplot(aes(x = Date, y = SWE_melt, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Daily Change in SWE in JMT watersheds, 2017")
  
ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_melt_time_series_2017.png", width = 10, height = 6)

# 2018 ##############  
# Visualization of total SWE in each watershed over time for 2015 
  jmt_swe_2018 %>% 
    gather("watershed", "SWE", -Date) %>% 
    ggplot(aes(x = Date, y = SWE, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Snow water equivalent in JMT watersheds, 2018")

ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_time_series_2018.png", width = 10, height = 6)

# Visualization of SWE melt in each watershed over time
  jmt_swe_2018 %>% 
    gather("watershed", "SWE", -Date) %>% 
    group_by(watershed) %>% 
    mutate(last_swe = dplyr::lag(SWE, order_by = watershed),
           SWE_melt = -(SWE - last_swe)) %>% 
    ggplot(aes(x = Date, y = SWE_melt, col = watershed)) +
      geom_line(size = 1.2) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(title = "Daily Change in SWE in JMT watersheds, 2018")
  
ggsave(filename = "Plots_Viz/SWE_Time_Series/SWE_melt_time_series_2018.png", width = 10, height = 6)
  