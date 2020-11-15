
# Source

source("Runtime.R")
source("01.Get_data.R")

##### Server

library("RSQLite") # Data importing
library("shiny") # Shiny components
library("shinydashboard") # Shiny dashboard
library("shinycssloaders") # Animated CSS loader
library("shinyalert") # Shiny Alerts
library("shinyWidgets") # Shiny Widgets
library("shinytest") # For testing 
library("shinyjs") # JavaScript
library("quantmod") # Financial data
library("highcharter") # Plotting
library("markdown") # Reporting
library("ggplot2") # Plotting
library("hrbrthemes") # Themes
library("lubridate") # Tidy
library("scales") # Plotting
library("plotly") # Plotting
library("dplyr") # Tidy
library("DT") # Data tables 

# Icons: https://fontawesome.com/icons?d=listing
# Hchart: http://jkunst.com/highcharter/index.html

server <- function(input, output) {
  
  ### Selected Country: ----------------------------------------------------------------
  
  SelectedInput <- reactive({
    as.numeric(input$CountryId)
  })
  
  ### Get data: ----------------------------------------------------------------
  
  dataInput <- reactive({
    # Selected country
    SelectedCountry <- as.character(DfCountries$Countries[as.integer(SelectedInput())])
    # Filter and mutate
    df0 <- data %>%
      arrange(Date) %>% 
      filter(Country == SelectedCountry) %>% 
      mutate(CumCases = cumsum(Cases),
             CumDeaths = cumsum(Deaths),
             LogCases = log(Cases),
             LogDeaths = log(Deaths),
             PercentChangeCases = (CumCases/lag(CumCases) - 1) * 100,
             PercentChangeDeaths = (CumDeaths/lag(CumDeaths) - 1) * 100) %>%
      filter(CumCases > 100) 
    
    # Move log(0) to 0
    
    df0 <- df0 %>% mutate(LogCases = ifelse(LogCases<0,0,LogCases),
                          LogDeaths = ifelse(LogDeaths<0,0,LogDeaths),
                          PercentChangeCases = ifelse(PercentChangeCases<0,0,PercentChangeCases),
                          PercentChangeDeaths = ifelse(is.na(PercentChangeDeaths)|is.infinite(PercentChangeDeaths),0,PercentChangeDeaths))
    # Output
    df0
  })
  
  ### Some operations: ----------------------------------------------------------------
  
  output$TotalCases <- renderText({
      paste0("The confirmed number of cases is:"," ",max(dataInput()$CumCases))
  })
  
  output$TotalDeaths <- renderText({
    paste0("The confirmed number of deaths is:"," ",max(dataInput()$CumDeaths))
  })
  
  ### Show data: ----------------------------------------------------------------
  
  output$Table1 <- DT::renderDataTable({
    df <- dataInput()
    df <- df %>% select(Date,Cases,Deaths)
    datatable(df,options = list(scrollX = TRUE,pageLength = 15))
  })
  
  ### Main Plots: ----------------------------------------------------------------
  
  output$plot1 <- renderPlotly({
    # Ggplot2
    p1 <- ggplot(data = dataInput(), aes(x = Date, y = Cases)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Cases", title = "Reported cases per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p1) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$plot2 <- renderPlotly({
    # Ggplot2
    p2 <- ggplot(data = dataInput(), aes(x = Date, y = Deaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Deaths", title = "Reported deaths per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p2) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$plot3 <- renderPlotly({
    # Ggplot2
    p3 <- ggplot(data = dataInput(), aes(x = Date, y = CumCases)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Cases", title = "Reported cumulative cases") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p3) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$plot4 <- renderPlotly({
    # Ggplot2
    p4 <- ggplot(data = dataInput(), aes(x = Date, y = CumDeaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Deaths", title = "Reported cumulative deaths") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p4) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$plot5 <- renderPlotly({
    # Ggplot2
    p5 <- ggplot(data = dataInput(), aes(x = Date, y = LogCases)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Log(Cases)", title = "Log of reported cases per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p5) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$plot6 <- renderPlotly({
    # Ggplot2
    p6 <- ggplot(data = dataInput(), aes(x = Date, y = LogDeaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Log(Deaths)", title = "Log of reported deaths per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p6) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$plot7 <- renderPlotly({
    # Ggplot2
    p7 <- ggplot(data = dataInput(), aes(x = Date, y = PercentChangeCases)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Cases", title = "Percentual change in cases per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p7) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  output$plot8 <- renderPlotly({
    # Ggplot2
    p8 <- ggplot(data = dataInput(), aes(x = Date, y = PercentChangeDeaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Deaths", title = "Percentual change in deaths per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p8) %>% plotly::config(displaylogo = FALSE,
                                    modeBarButtonsToRemove = list(
                                      'zoom2d',
                                      'pan2d',
                                      'select2d',
                                      'lasso2d',
                                      'zoomIn2d',
                                      'zoomOut2d',
                                      'autoScale2d',
                                      'resetScale2d',
                                      'hoverClosestCartesian',
                                      'toggleSpikelines',
                                      'sendDataToCloud'
                                    )) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  
  ### Alerts: ----------------------------------------------------------------
  
  # Currently offline
  
  #observeEvent(input$get, {
  #  shinyalert("Getting data", "Hold on")
  #})
  
  #observeEvent(input$plot, {
  #  shinyalert("Plotting data", "Hold on")
  #})
  
} 

#---
