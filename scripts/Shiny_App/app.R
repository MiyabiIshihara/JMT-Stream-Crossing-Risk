library(shiny)
library(usmap)
library(ggplot2)
library(leaflet)
library(raster)
library(rgdal)
library(viridis)
library(lubridate)
library(tidyverse)
library(htmltools)
library(dplyr)

# Load data ###########
jmt_crossings_simplify <- readRDS("Data/crossing_points.rds")
jmt_main <- readRDS("Data/jmt_main_trail.rds")
jmt_access <- readRDS("Data/jmt_access_trails.rds")
jmt_watersheds <- readRDS("Data/jmt_watersheds.rds")

snow_depth_2015_jmt <- readRDS("Data/snow_depth_2015.rds")

# Combine multiple data into a list
data <- list("snow_depth" = snow_depth_2015_jmt, 
             "precip" = precip_2015_jmt)


# Make icon for stream crossings ############      
crossingIcon <- makeIcon(
  iconUrl = "River_Icon/Artboard 1.png",
  iconRetinaUrl = "River_Icon/Artboard 1@2x.png",
  iconHeight = 35, iconWidth = 20 
)

# ----------------- # 
ui <- pageWithSidebar(
  headerPanel('Snow Depth'),
  sidebarPanel(
    radioButtons(inputId = "variable", 
                  label = "Variables to show", 
                  choices = c("Precipitation" = "precip", 
                              "Snow Depth" = "snow_depth"), 
                  selected = "precip"), 
    sliderInput(inputId = 'time', 
                label = 'Time Range', 
                min = as.Date("2015-01-01"), 
                max = as.Date("2015-12-31"), 
                value = as.Date(c("2015-01-01")), 
                timeFormat = '%Y-%m-%d')
  ),
  mainPanel(
    leafletOutput('plot1')
  )
)


# ----------------- # 

server <- function(input, output, session) {
  
  # subset data 
  selectedData <- reactive({
    selected_variable <- input$variable 
    selected_time <- input$time
      
    selectedData <- data[[paste0(selected_variable)]]
    selectedData <- selectedData[[yday(selected_time)]]
    selectedData
  })
  
  output$plot1 <- renderLeaflet({
    pal <- colorNumeric("viridis", domain = c(0,2000),
                        na.color = "transparent")
    leaflet() %>% 
      setView(lng = -118.869194, lat = 37.235921, zoom = 8) %>% 
      addProviderTiles(provider = "Esri.WorldTopoMap") %>% 
      addRasterImage(selectedData(), 
                     colors = pal, 
                     opacity = 0.4,
                     maxBytes = 10 * 1024 * 1024,
                     group = "Snow Depth") %>% 
      addLegend(pal = pal, 
                values = values(selectedData()), 
                title = "Snow Depth (mm)") %>% ## CHANGE
      addPolylines(data = jmt_access, 
                   color = "green", 
                   label = ~htmlEscape(Name),
                   weight = 2, 
                   opacity = 0.75, 
                   group = "JMT Access Trails") %>% 
      addPolylines(data = jmt_main, 
                   color = "brown", 
                   label = ~htmlEscape(Name),
                   weight = 4, 
                   opacity = 0.9, 
                   group = "JMT Main Trail") %>%
      addMarkers(data = jmt_crossings_simplify, 
                 label = ~htmlEscape(Crossing), 
                 icon = ~crossingIcon,
                 popup = ~htmlEscape(popup_field),
                 group = "JMT Main Stream Crossings") %>% 
      addPolygons(data = jmt_watersheds, 
                  color = "blue",
                  opacity = 0.9, 
                  weight = 1, 
                  fillOpacity = 0.2,
                  group = "Main Crossing Watersheds") %>% 
      addLayersControl(overlayGroups = c("Snow Depth",
                                         "JMT Access Trails",
                                         "JMT Main Trail",
                                         "JMT Main Stream Crossings",
                                         "Main Crossing Watersheds"))
  })
  
  
}

shinyApp(ui, server)