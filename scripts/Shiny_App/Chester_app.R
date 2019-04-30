
## app.R ##
library(shinydashboard)
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


# ------------------------------- # 
#         import data             # 
# ------------------------------- # 
# Load data ###########
jmt_crossings_simplify <- readRDS("Data/crossing_points.rds")
jmt_all <- readRDS("Data/jmt_all_trail.rds")
jmt_main <- jmt_all %>% filter(Type == 'Main')
jmt_access <- jmt_all %>% filter(Type == 'Access')
# jmt_main <- readRDS("Data/jmt_main_trail.rds")
# jmt_access <- readRDS("Data/jmt_access_trails.rds")
jmt_watersheds <- readRDS("Data/jmt_watersheds.rds")
snow_depth_2015_jmt <- readRDS("Data/snow_depth_2015.rds")
precip_2015_jmt <- readRDS("Data/prism_ppt_jmt_clip_2015.rds")
route_info <- readRDS("Data/route_info.rds")

# Combine multiple data into a list
data <- list("snow_depth" = snow_depth_2015_jmt, 
             "precip" = precip_2015_jmt)


# Make icon for stream crossings ############      
crossingIcon <- makeIcon(
  iconUrl = "River_Icon/Artboard 1.png",
  iconRetinaUrl = "River_Icon/Artboard 1@2x.png",
  # iconHeight = 35, iconWidth = 20 
  iconHeight = 17, iconWidth = 10
)


# ------------------------------- # 
#                ui               # 
# ------------------------------- # 
#### Header content #### 
header <- dashboardHeader(title = "John Muir Trail Hazard")


#### Sidebar content #### 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Planning Your Stream Crossings", 
             tabName = "planning"), # icon = icon("dashboard")
    menuItem("What Causes Risk?", 
             tabName = "risk_cause"), # icon = icon("th") 
    menuItem("Current Conditions", 
             tabName = "current_conditions"), 
    menuItem("About Us", 
             tabName = "about_us")
  )
)


#### Body content #### 
body <- dashboardBody(
  tabItems(
    # 1st tab content -- Planning Crossings 
    tabItem(tabName = "planning",
            fluidRow(
              box(leafletOutput("plot1", height = 500)),
              
              box(
                title = "Select a date",
                sliderInput(inputId = 'time', 
                            label = '', 
                            min = as.Date("2015-01-01"), 
                            max = as.Date("2015-12-31"), 
                            value = as.Date(c("2015-05-20")), 
                            timeFormat = '%Y-%m-%d')
              ), 
              
              box(title = "Select ", 
                  radioButtons(inputId = "variable", 
                               label = "",
                               choices = c("Snow Depth" = "snow_depth", 
                                           "Precipitation" = "precip"), 
                               selected = "snow_depth")),
              
              box(title = "Enter Trip Information ", 
                  selectInput(inputId = "start_th", 
                               label = "Starting Trailhead:",
                               choices = sort(unique(route_info$`entry trailhead`)), 
                               selected = "Happy Isles Trailhead"),
                  
                  selectInput(inputId = "end_th", 
                              label = "Ending Trailhead:",
                              choices = sort(unique(route_info$`exit trailhead`)), 
                              selected = "Whitney Portal")
                  
                  )
            )
    ), 
    
    # 2nd tab content -- What causes risk? 
    tabItem(tabName = "risk_cause", 
            includeMarkdown("docs/risk_cause.md")), 
    
    # 3rd tab content -- Current conditions
    tabItem(tabName = "current_conditions"), 
    
    # 4th tab content -- About Us
    tabItem(tabName = "about_us",
                includeMarkdown("docs/about_us.md"))
    )
)


ui <- dashboardPage(
  title = "JMT WebApp", 
  skin = "blue", 
  header, 
  sidebar, 
  body)



# ------------------------------- # 
#              server             # 
# ------------------------------- # 

server <- function(input, output) {
  
  # subset data
  selectedData <- reactive({
    selectedData <- data[[paste0(input$variable)]]
    selectedData <- selectedData[[yday(input$time)]]
    selectedData
  })
  
  compileRoute <- function(start, end) {
    # Get routes with that start and end
    routes <- route_info %>% filter(`entry trailhead`==start & `exit trailhead`==end)
    # Identify the shortest of these routes
    route <- routes %>% filter(length == min(length))
    # Get the segment indices associated with this route
    segment_ids <- route$edge_ids
    segment_ids <- substr(segment_ids, 2, nchar(segment_ids)-1)
    segment_ids <- as.integer(strsplit(segment_ids, ", ")[[1]])
    # Add one to account for R indices starting at 1 instead of 0
    segment_ids <- segment_ids + 1
    # Select the segments involves with this route
    segments = jmt_all  %>% slice(segment_ids)
    # Get the crossing indices associated with this route
    crossing_ids <- route$streams_crossed
    crossing_ids <- substr(crossing_ids, 2, nchar(crossing_ids)-1)
    crossing_ids <- as.integer(strsplit(crossing_ids, ", ")[[1]])
    # Add one to account for R indices starting at 1 instead of 0
    crossing_ids <- crossing_ids + 1
    # Select the crossings involved with this route
    crossings <- jmt_crossings_simplify %>% slice(crossing_ids)
    # Return just the segments associated with the route
    return(list('segments'=segments, 'crossings'=crossings))
  }
  route = reactive({compileRoute(input$start_th, input$end_th)})

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
      
      # Map selected route
      addPolylines(data = route()$segments,
                   color = "blue",
                   label = ~htmlEscape(Name),
                   weight = 4,
                   opacity = 0.9,
                   group = "Selected Route") %>%
      
      addLayersControl(overlayGroups = c("Snow Depth",
                                         "JMT Access Trails",
                                         "JMT Main Trail",
                                         "JMT Main Stream Crossings",
                                         "Main Crossing Watersheds",
                                         "Selected Route"))
  }) # end of leaflet function
}


shinyApp(ui, server)

