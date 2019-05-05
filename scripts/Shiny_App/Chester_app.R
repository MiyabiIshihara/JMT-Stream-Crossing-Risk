
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
library(markdown)
library(sf)
library(shinyWidgets)

# ------------------------------- # 
#         import data             # 
# ------------------------------- # 
# Load data
jmt_crossings_simplify <- readRDS("Data/crossing_points.rds")
jmt_all <- readRDS("Data/jmt_all_trails.rds")
jmt_main <- jmt_all %>% filter(Type == 'Main')
jmt_access <- jmt_all %>% filter(Type == 'Access')
jmt_watersheds <- readRDS("Data/jmt_watersheds.rds")
snow_depth_2015_jmt <- readRDS("Data/snow_depth_2015.rds")
precip_2015_jmt <- readRDS("Data/prism_ppt_jmt_clip_2015.rds")
route_info <- readRDS("Data/route_info.rds")
crossing_positions <- readRDS("Data/crossing_positions.rds")

# Combine multiple data into a list
data <- list("snow_depth" = snow_depth_2015_jmt, 
             "precip" = precip_2015_jmt)



# ------------------------------- # 
#         ui components           # 
# ------------------------------- # 

# date_selector <- function(){
#   sliderInput(
#     inputId = 'time', 
#     label = '', 
#     min = as.Date("2015-01-01"), 
#     max = as.Date("2015-12-31"), 
#     value = as.Date(c("2015-05-20")), 
#     timeFormat = '%Y-%m-%d'
#   )
# }

trip_selector <- function(){
  fluidRow(
    column(
      8,
      style='padding:0px;',
      selectInput(
        inputId = "start_th",
        label = "Starting Trailhead:",
        choices = sort(unique(route_info$`entry trailhead`)),
        selected = "Happy Isles Trailhead",
      ),
      selectInput(
        inputId="end_th",
        label="Ending Trailhead:",
        choices=sort(unique(route_info$`exit trailhead`)),
        selected="Whitney Portal",
      )
    ),
    column(
      4,
      style='padding:0px;',
      dateInput(
        inputId="start_date",
        label="Start Date:",
        value=today("PMT"),
        format = "M dd, yyyy"
      ),
      dateInput(
        inputId="end_date",
        label="End Date:",
        value=(today("PMT") + 21),
        format = "M dd, yyyy"
      )
    )
  )
}

raster_selector <- function(){
  radioButtons(
    inputId = "variable", 
    label = "",
    choices = c(
      "Snow Depth" = "snow_depth", 
      "Precipitation" = "precip"
    ), 
    selected = "snow_depth"
  )
}

# Define Icons
icon <- function(url, retinaUrl) {
  return(
    makeIcon(
      iconUrl = url,
      # iconRetinaUrl = retinaUrl,
      shadowUrl = "www/icon_shadow.png",
      iconHeight = 35,
      iconWidth = 25,
      shadowHeight = 35,
      shadowWidth = 35,
      iconAnchorX = 12,
      iconAnchorY = 35,
      shadowAnchorX = 6,
      shadowAnchorY = 32,
      popupAnchorX = 1,
      popupAnchorY = -35
    )
  )
}
blueIcon <- icon("www/icon.png", "www/icon2x.png")
redIcon <- icon("www/icon_red.png", "www/icon2x_red.png")
yellowIcon <- icon("www/icon_yellow.png", "www/icon2x_yellow.png")
greenIcon <- icon("www/icon_green.png", "www/icon2x_green.png")
blueIconSemi <- icon("www/icon_semi.png", "www/icon2x_semi.png")
redIconSemi <- icon("www/icon_red_semi.png", "www/icon2x_red_semi.png")
yellowIconSemi <- icon("www/icon_yellow_semi.png", "www/icon2x_yellow_semi.png")
greenIconSemi <- icon("www/icon_green_semi.png", "www/icon2x_green_semi.png")

# ------------------------------- # 
#                ui               # 
# ------------------------------- # 
#### Header content #### 
header <- dashboardHeader(
  title = tags$a(
    tags$img(
      src="Logo.svg",
      # height = "50",
      width = "50"
    ), 
    'JMT Stream Crossing Planner'),
  titleWidth = 400)


#### Sidebar content #### 
sidebar <- dashboardSidebar(
  width = 400,

  # Navigation
  column(
    12,
    style = "background-color:#4372a7;",
    
    # Header
    HTML("<h1>PAGES</h1>"),
    
    # Menu Items
    sidebarMenu(
      menuItem(
        "Stream Crossing Map", 
        tabName="planning"#,
        # icon=icon("dashboard")
      ),
      menuItem(
        "Historical Stream Flow", 
        tabName="risk_cause"#,
        # icon=icon("th")
      ),
      menuItem(
        "Forecasted Stream Flow", 
        tabName="current_conditions"
      ),
      menuItem(
        "Stream Crossing Information", 
        tabName="about_us"
      ),
      menuItem(
        "About This Project", 
        tabName="about_us"
      )
    )
  ),
  
  # Spacing the same color as the Navigation
  column(
    12,
    style = "background-color:#4372a7;",
    HTML("<br>")
  ),
  
  # Spacing without a color
  column(
    12,
    HTML("<br>")
  ),
  
  # Trip Planner
  column(
    12,
    style = "background-color:#4372a7;",
    HTML("<h1>TRIP PLANNER</h1>"),
    # date_selector(),
    trip_selector(),
    uiOutput("trip_day_selector"),
    # trip_day_selector(),
    raster_selector()
  )
)

main_map <- function(){
  leafletOutput(
    "main_map"
  )
}

#### Body content ####
body <- dashboardBody(
  
  # Load CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # Setup Tabs
  tabItems(
    # 1st tab content -- Planning Crossings
    tabItem(
      tabName = "planning",
      main_map()
    ),
    
    # 2nd tab content -- What causes risk?
    tabItem(
      tabName="risk_cause",
      includeMarkdown("docs/risk_cause.md")
    ),
    
    # 3rd tab content -- Current conditions
    tabItem(
      tabName = "current_conditions"
    ),
    
    # 4th tab content -- About Us
    tabItem(
      tabName="about_us",
      includeMarkdown("docs/about_us.md")
    )
  )
)

ui <- dashboardPage(
  title = "JMT WebApp", 
  skin = "blue", 
  header, 
  sidebar, 
  body
)



# ------------------------------- # 
#              server             # 
# ------------------------------- # 

server <- function(input, output) {
  
  # Compile the route between start and end trailheads
  compileRoute <- function(start, end) {
    # Get shortest routes with that start and end
    route <- route_info %>%
      filter(`entry trailhead`==start & `exit trailhead`==end) %>%
      filter(length == min(length))
    print(route$segment_ids)
    print(route$crossing_ids)
    # Select the segments involved with this route
    segment_ids <- route$segment_ids[[1]] + 1
    segments = jmt_all %>% slice(segment_ids)
    # Select the crossings involved with this route
    crossing_ids <- route$crossing_ids[[1]] + 1
    crossings <- jmt_crossings_simplify %>% slice(crossing_ids)
    
    return(
      list(
        'segment_geoms'=segments,
        'crossing_geoms'=crossings,
        'crossing_ids'=crossing_ids,
        'crossing_names'=crossings$Crossing,
        'crossing_dists'=route$crossing_positions[[1]],
        'bounds'=st_bbox(segments),
        'id'=route$route_id
      )
    )
  }
  route <- reactive({compileRoute(input$start_th, input$end_th)})
  
  # reactive({print(route()$crossing_geoms$Crossing)})
  
  # Prepare a custom day selector UI based on the number of trip days
  output$trip_day_selector <- renderUI({
    fluidRow(
      sliderTextInput(
        inputId = 'trip_day',
        label = "Select Crossing:",
        choices = route()$crossing_names,
        selected = route()$crossing_names[c(1)],
        width = '100%',
        grid = TRUE
      )
    )
  })
    
  # Select raster data based on trip date
  selectedData <- reactive({
    map_date = input$start_date
    selectedData <- data[[paste0(input$variable)]]
    selectedData <- selectedData[[yday(map_date)]]
    selectedData
  })
    
  # trip_days <- as.numeric(difftime(input$end_date, input$start_date, units = "days")) + 1
  # fluidRow(
  #   sliderInput(
  #     inputId = 'trip_day',
  #     label = 'Trip Day:',
  #     min = 1,
  #     max = trip_days,
  #     value = 1,
  #     width = "100%",
  #     step = 1
  #   )
  # )
  # })
  
  # # Compile the crossings associated with that route
  # compileCrossings <- function(route_id) {
  #   crossings <- crossing_positions %>%
  #     filter(route_id==route_id) %>%
  #     arrange(crossing_position)
  #   crossing_geoms <- jmt_crossings_simplify %>% 
  #     slice(crossings$crossing_id + 1)
  #   return(
  #     list(
  #       'ids' = crossings$crossing_id,
  #       'lin_refs' = crossings$crossing_position,
  #       'geoms' = crossing_geoms
  #     )
  #   )
  # }
  # crossings <- reactive(compileCrossings(route()$id))
  
  output$main_map <- renderLeaflet({
    pal <- colorNumeric(
      # "viridis",
      palette = colorRamp(c("#FFFFFF", "#014175"), interpolate = "spline"),
      domain=c(0,3000),
      na.color="transparent"
    )
    
    leaflet() %>%
      # Fit to the bounds of the selected route
      fitBounds(
        as.numeric(route()$bounds$xmin), 
        as.numeric(route()$bounds$ymin), 
        as.numeric(route()$bounds$xmax), 
        as.numeric(route()$bounds$ymax)) %>%
      
      ### Make basemap selector
      
      # # Load basemap
      # addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      # Load basemap
      addProviderTiles(provider = providers$OpenStreetMap.Mapnik) %>%
      # Load raster layers
      addRasterImage(
        selectedData(),
        # colors = pal,
        colors = pal,
        opacity = 0.5,
        maxBytes = 10 * 1024 * 1024,
        group = "Snow Depth",
      ) %>%
      
      addLegend(
        pal = pal,
        values = values(selectedData()),
        title = "Snow Depth (mm)"
      ) %>% ## CHANGE
      
      # Map access trails
      addPolylines(
        data = jmt_access,
        color = "#202020",
        label = ~htmlEscape(Name),
        weight = 1,
        opacity = 0.9,
        group = "JMT Access Trails"
      ) %>%
      
      # Map main JMT trail
      addPolylines(
        data = jmt_main,
        color = "#202020",
        label = ~htmlEscape(Name),
        weight = 3,
        opacity = 0.9,
        group = "JMT Main Trail"
      ) %>%
      
      # Map stream Crossings
      addMarkers(
        data = route()$crossing_geoms,
        label = ~htmlEscape(Crossing),
        icon = ~blueIcon,
        popup = ~htmlEscape(popup_field),
        group = "JMT Main Stream Crossings"
      ) %>%
      
      # Map watersheds
      addPolygons(
        data = jmt_watersheds,
        color = "#4372a7",
        opacity = 0.9,
        weight = 1,
        fillOpacity = 0.2,
        group = "Main Crossing Watersheds"
      ) %>%
      
      # Map selected route
      addPolylines(
        data = route()$segment_geoms,
        color = "#126b20",
        label = ~htmlEscape(Name),
        weight = 6,
        opacity = 1,
        group = "Selected Route"
      ) %>%
      
      addLayersControl(overlayGroups = c("Snow Depth",
                                         "JMT Access Trails",
                                         "JMT Main Trail",
                                         "JMT Main Stream Crossings",
                                         "Main Crossing Watersheds",
                                         "Selected Route"))
  }) # end of leaflet function
}


shinyApp(ui, server)

