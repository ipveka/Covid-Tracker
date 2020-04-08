  
#---

##### Server

# Packages ----------------------------------------------------------------

library("RSQLite") # Data importing
library("shiny") # Shiny components
library("shinydashboard") # Shiny dashboard
library("shinycssloaders") # Animated CSS loader
library("shinyalert") # Shiny Alerts
library("shinytest") # For testing 
library("shinyjs") # JavaScript
library("quantmod") # Financial data
library("highcharter") # Plotting
library("markdown") # Reporting
library("DT") # Data tables

library(ggplot2) # Plotting
library(hrbrthemes) # Themes
library(shinydashboard) # Shiny
library(shiny) # Shiny
library(lubridate) # Tidy
library(scales) # Plotting
library(plotly) # Plotting
library(dplyr) # Tidy
library(DT) # Tables

# Icons: https://fontawesome.com/icons?d=listing
# Hchart: http://jkunst.com/highcharter/index.html

server <- function(input, output) {
  
  ### Funcions i crides:
  
  #---
  
  ### Get data:
  dataInput <- eventReactive(input$CountryId,{
    source("01.Get_data.R")
    # Selected country
    # Aux <- 110
    SelectedCountry <- as.character(DfCountries$Countries[Aux])
    # Filter and mutate
    df0 <- data %>%
      arrange(Date) %>% 
      filter(Country == SelectedCountry) %>%
      mutate(CumCases = cumsum(Cases),
             CumDeaths = cumsum(Deaths),
             LogCases = log(Cases),
             LogDeaths = log(Deaths),
             CumLogCases = log(CumCases),
             CumLogDeaths = log(CumCases)) %>%
      filter(CumCases > 100)
    
    # Day Index
    df0 <- df0 %>% mutate(DayIndex = c(1:nrow(df0)))
    # Move log(0) to 0
    df0 <- df0 %>% mutate(LogCases = ifelse(LogCases<0,0,LogCases),
                          LogDeaths = ifelse(LogDeaths<0,0,LogDeaths),
                          CumLogCases = ifelse(CumLogCases<0,0,CumLogCases),
                          CumLogDeaths = ifelse(CumLogDeaths<0,0,CumLogDeaths))
    # Output
    df0
  })
  
  ### Show data:
  output$table <- DT::renderDataTable(
    dataInput(),
    options = list(scrollX = TRUE,pageLength = 15)
  )
  
  ### Main charts:
  
  ### Alerts
  
  # Currently offline
  
  #observeEvent(input$get, {
  #  shinyalert("Getting data", "Hold on")
  #})
  
  #observeEvent(input$plot, {
  #  shinyalert("Plotting data", "Hold on")
  #})
  
} 


