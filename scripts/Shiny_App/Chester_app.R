
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
swe_risk_2015_2018 <- readRDS("Data/swe_risk_2015_2018.rds")

# Combine multiple data into a list
data <- list(
  "snow_depth" = snow_depth_2015_jmt, 
  "precip" = precip_2015_jmt
)

# ------------------------------- # 
#         ui components           # 
# ------------------------------- # 

trip_selector <- function(){
  fluidRow(
    column(
      8,
      style='padding:0px;',
      selectInput(
        inputId = "start_th",
        label = "Starting Trailhead",
        choices = sort(unique(route_info$`entry trailhead`)),
        selected = "Happy Isles Trailhead",
      ),
      selectInput(
        inputId="end_th",
        label="Ending Trailhead",
        choices=sort(unique(route_info$`exit trailhead`)),
        selected="Whitney Portal",
      )
    ),
    column(
      4,
      style='padding:0px;',
      dateInput(
        inputId="start_date",
        label="Start Date",
        value=today("PMT"),
        format = "M dd, yyyy"
      ),
      dateInput(
        inputId="end_date",
        label="End Date",
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
      
      verbatimTextOutput("test"), ###########
      
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
    trip_selector(),
    uiOutput("crossing_selector"),
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
      includeMarkdown("docs/risk_cause.md"),
      plotOutput("time_series")
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
    # Select the segments involved with this route
    segment_ids <- route$segment_ids[[1]] + 1
    segments = jmt_all %>% slice(segment_ids)
    # Select the crossings involved with this route
    # Create a copy of the crossings table specific to this route; include row numbers
    route_crossings <- jmt_crossings_simplify %>% mutate(id = row_number())

    # Restrict to crossings along this route
    crossing_ids <- route$crossing_ids[[1]] + 1
    route_crossings <- route_crossings %>% slice(crossing_ids)
    
    # Match the dataframe order to the predefined crossing order
    route_crossings <- route_crossings[match(route$crossing_ids[[1]] + 1, route_crossings$id),]
    
    # Add crossing distances
    route_crossings$crossing_dist <- route$crossing_positions[[1]]
    
    # Calculate crossing days
    crossing_day <- function(dist) {
      start_day <- input$start_date
      end_day <- input$end_date
      days <- end_day - start_day + 1
      crossing_days <- floor(dist / route$length * days)
      crossing_date <- start_day + crossing_days
      return(crossing_date)
    }
    route_crossings$crossing_date <- route_crossings$crossing_dist %>% lapply(crossing_day)
    
    # Calculate crossing day of year
    day_of_year <- function(date) {
      day <- strftime(date, format = "%j")
      day <- as.numeric(day)
      return(day)
    }
    route_crossings$crossing_day_of_year <- route_crossings$crossing_date %>% lapply(day_of_year)
    
    return(
      list(
        'segment_geoms'=segments,
        'crossings'=route_crossings,
        'bounds'=st_bbox(segments),
        'length'=route$length,
        'start'=route$`entry trailhead`,
        'end'=route$`exit trailhead`,
        'id'=route$route_id
      )
    )
  }
  route <- reactive({compileRoute(input$start_th, input$end_th)})
  print(route()$crossings$crossing_day_of_year)
  
  # Prepare a custom crossing selector for chosing crossings along a given route
  output$crossing_selector <- renderUI({
      # slider_values <- c(
      #   route()$start,
      #   as.character(route()$crossing$Crossing),
      #   route()$end
      # )
      slider_values <- route()$crossing$Crossing
      fluidRow(
        sliderTextInput(
          inputId = 'selected_crossing',
          label = "Scroll between crossings along your trip:",
          choices = slider_values,
          selected = slider_values[c(1)],
          width = '100%',
          force_edges = TRUE,
          hide_min_max = TRUE
        )
      )
    })
  
  select_crossing <- function(selected_crossing) {
    # For some reason, leaflet only wants to display a sliced dataframe, not a filtered one
      # So first we get the row number, and then slice by it
      row_number <- which(jmt_crossings_simplify$Crossing == selected_crossing)
      crossing <- jmt_crossings_simplify %>% slice(row_number)
    return(crossing)
  }
  crossing <- reactive({select_crossing(input$selected_crossing)})

  # output$test <- renderText({as.Date(crossing()$crossing_date, format = "%m / %d / %Y")})
  
  # Select raster data based on trip date
  selectedData <- reactive({
    map_date = input$start_date
    # map_date = crossing()$crossing_date[[1]]
    selectedData <- data[[paste0(input$variable)]]
    selectedData <- selectedData[[yday(map_date)]]
    selectedData
  })
  
  # Prepare the main map  
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
        data = route()$crossings,
        label = ~htmlEscape(Crossing),
        icon = ~blueIcon,
        popup = ~htmlEscape(popup_field),
        group = "JMT Main Stream Crossings"
      ) %>%
      
      # Map selected Crossings
      addMarkers(
        data = crossing(),
        label = ~htmlEscape(Crossing),
        icon = ~redIcon,
        popup = ~htmlEscape(popup_field),
        group = "Selected Crossings"
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

#Plot to show historical data at selected crossing(s) ####### 
# Select date range based on inputs
  
# date_range <- reactive({
#   date_range <- list("start_day" = yday(input$start_date), 
#                      "end_day" = yday(input$end_date))
#   
#   date_range
# })
# 
#   output$time_series <- renderPlot({
#     swe_risk_2015_2018 %>% 
#       mutate(Year = as.factor(Year)) %>% 
#       filter(watershed %in% route()$crossing_names) %>% # Filter crossings here, currently based on crossings generated in route selection
#       gather("variable", "value", SWE, melt_risk) %>% 
#       ggplot(aes(x = year_day, y = value, lty = Year, col = watershed)) + # Currently symbolizing year with linetype and crossing with color. If we get to a point where only selecting one crossing, should switch year to color and get rid of linetype (lty) argument
#       annotate("rect", xmin = date_range()$start_day, xmax = date_range()$end_day,
#                ymin = 0, ymax = Inf,
#                fill = "grey20", alpha = 0.25) +
#       geom_line() + 
#       facet_grid(variable~., scales = "free_y") +
#       theme_classic() +
#       theme(legend.position = "bottom") +
#       labs(x = "Day of the year", 
#            title = "Snow Water Equivalent and Associated Risk",
#            subtitle = "2015-2018 historical data")
#     
#   })
}

shinyApp(ui, server)

