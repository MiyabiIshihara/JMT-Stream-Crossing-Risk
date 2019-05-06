
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


# source("inputs.R")
source("functions.R")

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
    tripSelector(),
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
  route <- reactive({compileRoute(input$start_th, input$end_th, input$start_date, input$end_date, jmt_crossings, jmt_all, swe_risk_2015_2018)})
  
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

