
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
library(tidytidbits)

# source("functions.R")

# UI component to select trip by start and end point/date
tripSelector <- function(route_info){
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

# Dynamic UI component to select crossing of interest based on previously selected route
crossingSelector <- function(route) {
  return(
    renderUI({
      # slider_values <- c(
      #   route()$start,
      #   as.character(route()$crossing$Crossing),
      #   route()$end
      # )
      slider_values <- route()$crossing$Crossing
      fluidRow(
        sliderTextInput(
          inputId = 'selected_crossing',
          label = "Scroll through stream crossings along your trip:",
          choices = slider_values,
          selected = slider_values[c(1)],
          width = '100%',
          force_edges = TRUE,
          hide_min_max = TRUE
        )
      )
    })
  )
}

# Filter the crossings table to get only the selected crossing
selectCrossingFromTable <- function(selected_crossing, crossings) {
  return(crossings %>% filter(Crossing==selected_crossing) %>% as_tibble())
}

# Filter crossings based on their risk
filterCrossingRisk <- function(crossings, risk) {
  return(crossings %>% filter(risk_score==risk) %>% as_tibble())
}

# Select the raster to display based on the selector
selectRaster <- function(crossing, raster_data, raster_selection) {
  selected_day = crossing$crossing_day_of_year
  selected_raster <- raster_data[[paste0(raster_selection)]]
  selected_raster <- selected_raster[[selected_day]]
  return(selected_raster)
}

# UI Component to select the snow depth vs precip raster
rasterSelector <- function(){
  fluidRow(
    radioButtons(
      inputId = "raster_selection",
      label = "Chose reference layer:",
      choices = c(
        "Snow Depth" = "snow_depth",
        "Precipitation" = "precip"
      ),
      selected = "snow_depth",
      width = 320
    )
  )
}

# Define a leaflet icon
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

small_icon <- function(url, retinaUrl) {
  return(
    makeIcon(
      iconUrl = url,
      # iconRetinaUrl = retinaUrl,
      shadowUrl = "www/icon_shadow.png",
      iconHeight = 18,
      iconWidth = 13,
      shadowHeight = 18,
      shadowWidth = 18,
      iconAnchorX = 6,
      iconAnchorY = 18,
      shadowAnchorX = 3,
      shadowAnchorY = 16,
      popupAnchorX = 1,
      popupAnchorY = -18
    )
  )
}

# Set up a number of leaflet icons
blueIcon <- icon("www/icon.png", "www/icon2x.png")
blueIconSmall <- small_icon("www/icon_small.png", "www/icon2x.png")
redIcon <- icon("www/icon_red.png", "www/icon2x_red.png")
redIconSmall <- small_icon("www/icon_red_small.png", "www/icon2x_red.png")
yellowIcon <- icon("www/icon_yellow.png", "www/icon2x_yellow.png")
yellowIconSmall <- small_icon("www/icon_yellow_small.png", "www/icon2x_yellow.png")
greenIcon <- icon("www/icon_green.png", "www/icon2x_green.png")
greenIconSmall <- small_icon("www/icon_green_small.png", "www/icon2x_green.png")
blueIconSemi <- icon("www/icon_semi.png", "www/icon2x_semi.png")
redIconSemi <- icon("www/icon_red_semi.png", "www/icon2x_red_semi.png")
yellowIconSemi <- icon("www/icon_yellow_semi.png", "www/icon2x_yellow_semi.png")
greenIconSemi <- icon("www/icon_green_semi.png", "www/icon2x_green_semi.png")


# Compile the route between start and end trailheads
compileRoute <- function(start_th, end_th, start_date, end_date, jmt_crossings, jmt_all, swe_risk_2015_2018, route_info) {
  # Get shortest routes with that start and end
  route <- route_info %>%
    filter(`entry trailhead`==start_th & `exit trailhead`==end_th) %>%
    filter(length == min(length))
  # Select the segments involved with this route
  segment_ids <- route$segment_ids[[1]] + 1
  segments = jmt_all %>% slice(segment_ids)
  # Select the crossings involved with this route
  # Create a copy of the crossings table specific to this route; include row numbers
  crossings <- jmt_crossings %>% mutate(id = row_number())

  # Restrict to crossings along this route
  crossing_ids <- route$crossing_ids[[1]] + 1
  crossings <- crossings %>% slice(crossing_ids)

  # Match the dataframe order to the predefined crossing order
  crossings <- crossings[match(route$crossing_ids[[1]] + 1, crossings$id),]

  # Add crossing distances
  crossings$crossing_dist <- route$crossing_positions[[1]]

  # Calculate crossing days
  crossing_day <- function(dist) {
    days <- end_date - start_date + 1
    crossing_days <- floor(dist / route$length * days)
    crossing_date <- start_date + crossing_days
    return(crossing_date)
  }
  crossings$crossing_date <- crossings$crossing_dist %>% sapply(crossing_day) %>% as.Date(origin=origin)

  # Calculate the crossing's day of the year
  day_of_year <- function(date) {
    day <- yday(date)
    return(day)
  }
  crossings$crossing_day_of_year <- crossings$crossing_date %>% sapply(day_of_year)

  # Retrieve crossing risk scores based on day of year and crossing ids
  get_risk_score <- function(id, crossing_day_of_year) {
    # Retrieve risk scores for this crossing on this day of the year
    crossing_scores <- swe_risk_2015_2018 %>% filter(crossing_id==id & year_day==crossing_day_of_year)
    # Take the max acrossa available years
    return(max(crossing_scores$risk_score))
  }
  crossings$risk_score <- map2_dbl(crossings$id, crossings$crossing_day_of_year, get_risk_score)

  return(
    list(
      'segment_geoms'=segments,
      'crossings'=crossings,
      'bounds'=st_bbox(segments),
      'length'=route$length,
      'start'=route$`entry trailhead`,
      'end'=route$`exit trailhead`,
      'id'=route$route_id
    )
  )
}


# ------------------------------- # 
#         import data             # 
# ------------------------------- # 

jmt_crossings <- readRDS("Data/crossing_points.rds")
jmt_all <- readRDS("Data/jmt_all_trails.rds")
jmt_main <- jmt_all %>% filter(Type == 'Main')
jmt_access <- jmt_all %>% filter(Type == 'Access')
jmt_watersheds <- readRDS("Data/jmt_watersheds.rds")
snow_depth_2015_jmt <- readRDS("Data/snow_depth_2015.rds")
snow_depth_2017_jmt <- readRDS("Data/snow_depth_2017.rds")
precip_2015_jmt <- readRDS("Data/prism_ppt_jmt_clip_2015.rds")
route_info <- readRDS("Data/route_info.rds")
swe_risk_2015_2018 <- readRDS("Data/swe_risk_2015_2018.rds")


# Combine multiple data into a list
raster_data <- list(
  "snow_depth" = snow_depth_2015_jmt, 
  "precip" = precip_2015_jmt
)

# ------------------------------- # 
#         ui components           # 
# ------------------------------- # 


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
  titleWidth = 420)


#### Sidebar content #### 
sidebar <- dashboardSidebar(
  width = 420,

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
        "Stream Flow & Hazard Timeseries", 
        tabName="risk_cause"#,
        # icon=icon("th")
      ),
      # menuItem(
      #   "Forecasted Stream Flow", 
      #   tabName="current_conditions"
      # ),
      # menuItem(
      #   "Stream Crossing Information", 
      #   tabName="about_us"
      # ),
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
    tripSelector(route_info),
    uiOutput("crossing_selector"),
    rasterSelector()
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
      HTML("<h2>SNOW WATER EQUIVALENT & ASSOCIATED RISK</h2>"),
      HTML("<h3>2015-2018 Historical Data</h3>"),
      # includeMarkdown("docs/risk_cause.md"),
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
  
  # Compile route info based on the entered start and end th/date
  route <- reactive({compileRoute(input$start_th, input$end_th, input$start_date, input$end_date, jmt_crossings, jmt_all, swe_risk_2015_2018, route_info)})
  
  # Built a dynamic slider to select the crossing of interest based on the input route
  output$crossing_selector <- crossingSelector(route)
  
  # Get the data for that selected crossing
  selected_crossing <- reactive({selectCrossingFromTable(input$selected_crossing, route()$crossings)})

  # Get raster data based on raster type selection and trip date
  selectedRaster <- reactive({selectRaster(selected_crossing(), raster_data, input$raster_selection)})
  
  # Filter crossings based their risk ratings
  highRiskCrossings <- reactive({filterCrossingRisk(route()$crossings, 3)})
  mediumRiskCrossings <- reactive({filterCrossingRisk(route()$crossings, 2)})
  lowRiskCrossings <- reactive({filterCrossingRisk(route()$crossings, 1)})
  highRiskCrossingsSelected <- reactive({filterCrossingRisk(selected_crossing(), 3)})
  mediumRiskCrossingsSelected <- reactive({filterCrossingRisk(selected_crossing(), 2)})
  lowRiskCrossingsSelected <- reactive({filterCrossingRisk(selected_crossing(), 1)})

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
        selectedRaster(),
        # colors = pal,
        colors = pal,
        opacity = 0.5,
        maxBytes = 10 * 1024 * 1024,
        group = "Snow Depth"
      ) %>%
      
      addLegend(
        pal = pal,
        values = values(selectedRaster()),
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
      
      # Map high risk crossings, not selected
      execute_if(
        (nrow(highRiskCrossings()) > 0),
        addMarkers(
          data = highRiskCrossings(),
          label = ~htmlEscape(Crossing),
          icon = ~redIconSmall,
          popup = ~htmlEscape(popup_field),
          group = "JMT Main Stream Crossings"
        )
      ) %>%
      
      # Map medium risk crossings, not selected
      execute_if(
        (nrow(mediumRiskCrossings()) > 0),
        addMarkers(
          data = mediumRiskCrossings(),
          label = ~htmlEscape(Crossing),
          icon = ~yellowIconSmall,
          popup = ~htmlEscape(popup_field),
          group = "JMT Main Stream Crossings"
        )
      ) %>%
      
      # Map low risk crossings, not selected
      execute_if(
        (nrow(lowRiskCrossings()) > 0),
        addMarkers(
          data = lowRiskCrossings(),
          label = ~htmlEscape(Crossing),
          icon = ~greenIconSmall,
          popup = ~htmlEscape(popup_field),
          group = "JMT Main Stream Crossings"
        )
      ) %>%
      
      # Map high risk crossings, selected
      execute_if(
        (nrow(highRiskCrossingsSelected()) > 0),
        addMarkers(
          data = highRiskCrossingsSelected(),
          label = ~htmlEscape(Crossing),
          icon = ~redIcon,
          popup = ~htmlEscape(popup_field),
          group = "Selected Crossings"
        )
      ) %>%
      
      # Map medium risk crossings, selected
      execute_if(
        (nrow(mediumRiskCrossingsSelected()) > 0),
        addMarkers(
          data = mediumRiskCrossingsSelected(),
          label = ~htmlEscape(Crossing),
          icon = ~yellowIcon,
          popup = ~htmlEscape(popup_field),
          group = "Selected Crossings"
        )
      ) %>%
      
      # Map low risk crossings, selected
      execute_if(
        (nrow(lowRiskCrossingsSelected()) > 0),
        addMarkers(
          data = lowRiskCrossingsSelected(),
          label = ~htmlEscape(Crossing),
          icon = ~greenIcon,
          popup = ~htmlEscape(popup_field),
          group = "Selected Crossings"
        )
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
      
      execute_if(
        (nrow(route()$segment_geoms) > 0),
        addPolylines(
          data = route()$segment_geoms,
          color = "#126b20",
          label = ~htmlEscape(Name),
          weight = 6,
          opacity = 1,
          group = "Selected Route"
        )
      ) %>%

      addLayersControl(
        overlayGroups = c(
          "Snow Depth",
          "JMT Access Trails",
          "JMT Main Trail",
          "JMT Main Stream Crossings",
          "Main Crossing Watersheds",
          "Selected Route"
        )
      )
  
  }) # end of leaflet function

#Plot to show historical data at selected crossing(s) ####### 
# Select date range based on inputs
  
# Retrieve date range based on date selectors
  prepareGraphDateRange <- function(start, end) {
    date_range <- list(
      'start' = yday(start),
      'end' = yday(end)
    )
    return(date_range)
  }
  
  graph_date_range <- reactive({prepareGraphDateRange(input$start_date, input$end_date)})

  plotTimeSeries <- function() {
    return(
      renderPlot({
        swe_risk_2015_2018 %>%
          mutate(Year = as.factor(Year)) %>%
            filter(watershed %in% selected_crossing()$Crossing) %>% 
              gather("variable", "value", SWE, melt_risk) %>%
              mutate(variable = case_when(variable == "melt_risk" ~ "Risk Score",
                                          variable == "SWE" ~ "Snow Water Equivalent")) %>% 
        ggplot(
          aes(
            x = year_day, 
            y = value, 
            col = Year
          )
        ) + 
        annotate(
          "rect",
          xmin = graph_date_range()$start, 
          xmax = graph_date_range()$end,
          ymin = 0,
          ymax = Inf,
          fill = "grey20",
          alpha = 0.25
        ) +
        geom_line() +
        geom_vline(xintercept = selected_crossing()$crossing_day_of_year, 
                   lty = 2, col = 2, size = 1.2) +  
        facet_grid(
          variable~.,
          scales = "free_y"
        ) +
        theme_classic() +
        theme(
          legend.position = "bottom",
          panel.background = element_blank()
        ) +
        labs(
          x = "Day of the year"
          # title = "Snow Water Equivalent and Associated Risk",
          # subtitle = "2015-2018 historical data"
        )
      },
      bg = "transparent", # Does not appear to be working
      height = 600
      )
    )
  }
  output$time_series <- plotTimeSeries()
}

shinyApp(ui, server)

