  
#---

# Get data

#------------------ Parameters ------------------

confirmed_color <- "#000080"
main_color <- "#000000"
death_color <- "#000080"

# Source

source("01.Get_data.R")

##### Server

# Packages ----------------------------------------------------------------

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

server <- function(input, output, session) {
  
  ### Selected Country: ----------------------------------------------------------------
  
  SelectedInput <- reactive({
    as.numeric(input$CountryId)
  })
  
  ### Get data: ----------------------------------------------------------------
  
  dataInput <- reactive({
    # Filter and mutate
    df0 <- data %>%
      arrange(Date) %>%
      filter(Country == as.character(DfCountries$Countries[SelectedInput()])) %>%
      mutate(CumCases = cumsum(Cases),
             CumDeaths = cumsum(Deaths),
             LogCases = log(Cases),
             LogDeaths = log(Deaths),
             CumLogCases = log(CumCases),
             CumLogDeaths = log(CumCases)) %>%
      filter(CumCases > 100)
    # Move log(0) to 0
    df0 <- df0 %>% mutate(LogCases = ifelse(LogCases<0,0,LogCases),
                          LogDeaths = ifelse(LogDeaths<0,0,LogDeaths),
                          CumLogCases = ifelse(CumLogCases<0,0,CumLogCases),
                          CumLogDeaths = ifelse(CumLogDeaths<0,0,CumLogDeaths))
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
    ggplotly(p1)
  })
  
  output$plot2 <- renderPlotly({
    # Ggplot2
    p2 <- ggplot(data = dataInput(), aes(x = Date, y = Deaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Deaths", title = "Reported deaths per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p2)
  })
  
  output$plot3 <- renderPlotly({
    # Ggplot2
    p3 <- ggplot(data = dataInput(), aes(x = Date, y = CumCases)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Cases", title = "Reported cumulative cases") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p3)
  })
  
  output$plot4 <- renderPlotly({
    # Ggplot2
    p4 <- ggplot(data = dataInput(), aes(x = Date, y = CumDeaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Deaths", title = "Reported cumulative deaths") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p4)
  })
  
  output$plot5 <- renderPlotly({
    # Ggplot2
    p5 <- ggplot(data = dataInput(), aes(x = Date, y = LogCases)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Cases", title = "Reported cases per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p5)
  })
  
  output$plot6 <- renderPlotly({
    # Ggplot2
    p6 <- ggplot(data = dataInput(), aes(x = Date, y = LogDeaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Deaths", title = "Reported deaths per day") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p6)
  })
  
  output$plot7 <- renderPlotly({
    # Ggplot2
    p7 <- ggplot(data = dataInput(), aes(x = Date, y = CumLogCases)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Cases", title = "Reported cumulative cases") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p7)
  })
  
  output$plot8 <- renderPlotly({
    # Ggplot2
    p8 <- ggplot(data = dataInput(), aes(x = Date, y = CumLogDeaths)) + theme_light() + 
      geom_line(color = main_color) + labs(x = "Time", y = "Deaths", title = "Reported cumulative deaths") +
      theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +
      scale_x_date(expand = c(0,0), date_breaks = "1 week", date_labels = "%b %d")
    ggplotly(p8)
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
