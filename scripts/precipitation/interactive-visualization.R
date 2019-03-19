# 
# map of precipitation in JMT area

library(shiny)
library(usmap)
library(ggplot2)

head(seasonal_df)

# ----------------- # 
ui <- pageWithSidebar(
  headerPanel('Precipitation'),
  sidebarPanel(
    sliderInput(inputId = 'time', label = 'Time Range', 
                min = as.Date("2014-12-01"), max = as.Date("2015-11-30"), 
                value = as.Date(c("2014-12-01", "2015-11-30")), 
                timeFormat = '%Y-%m-%d')
  ),
  mainPanel(
    plotOutput('plot1')
  )
)



# ----------------- # 
server <- function(input, output, session) {
  
  # # Combine the selected variables into a new data frame
  # selectedData <- reactive({
  #   seasonal_df %>% filter(input$time == time)
  # })
  
  output$plot1 <- renderPlot({
    # subset data 
    selectedData <- reactive({
      newData <- seasonal_df %>% 
        filter(input$time == time) %>% 
        mutate(mean = mean(precipitation, na.rm = T))
      return(newData)
    })

    
    # make plot
    state_df <- map_data("state", region = "California")
    
    plot <- ggplot(selectedData(), aes(x = longitude, y = latitude)) + 
      geom_point(aes(stationID = stationID, col = mean), size = 1) + 
      geom_polygon(data = state_df, aes(x = long, y = lat, group = group), 
                   fill = NA, color = "grey50")
    plot
  })

  
}

shinyApp(ui, server)



