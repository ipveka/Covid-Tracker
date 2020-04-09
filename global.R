
#---

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
library("ggplot2") # Plotting
library("hrbrthemes") # Themes
library("lubridate") # Tidy
library("scales") # Plotting
library("plotly") # Plotting
library("dplyr") # Tidy

# Get data

#------------------ Parameters ------------------

confirmed_color <- "#000080"
main_color <- "#000000"
death_color <- "#000080"

# Source

source("01.Get_data.R")

#---

