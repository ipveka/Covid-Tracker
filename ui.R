
#---

##### User Interface

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

### Title:

header <- dashboardHeader(title = "ShinyCovid")

### SideBar:

sidebar <- dashboardSidebar(width = 200,
                            
                            sidebarMenu(
                              
                              menuItem("Home", tabName = "home", icon = icon("fas fa-home")),
                              menuItem("Data", tabName = "data", icon = icon("fas fa-server")),
                              menuItem("Graphics", tabName = "graphics",  icon = icon("far fa-chart-bar")),
                              menuItem("Analysis", tabName = "analysis", icon = icon("fas fa-atom")),
                              menuItem("About", tabName = "about", icon = icon("fas fa-user")),
                              
                              hr(),
                              
                              helpText("Developed by ", 
                                       a("Ignasi Pascual", href = "https://github.com/ipveka"), ".",
                                       style = "padding-left:1em; padding-right:1em;position:absolute;")
                            )
)

### Dashboard:
body <- dashboardBody(
  
  # CSS 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  # Tabintes:
  tabItems(
    
    ### TAB 0 = Home:
    tabItem(tabName = "home",
            fluidPage(
              box(width = 12,
                  shiny::includeMarkdown("Home.md")))
            
    ),
    
    ### TAB 1 = Data:
    tabItem(tabName = "data",
            fluidRow(
              column(width = 4,
                     box(width = 12,
                         helpText("Select a country to examine. Information will be collected from the ECDC."),
                         selectInput("CountryId", label = h3("Select country"), 
                                     choices = ListCountries,
                                     selected = whichspain))
                     ),
              column(width = 8,
                     box(width = 12,
                         title = "Table of content",
                         withSpinner(DT::dataTableOutput("table",width = 800)))))
    ),
              
    ### TAB 2 = Graphics:
    tabItem(tabName = "graphics",
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Number of reported cases",
                         withSpinner(plotOutput("plot1",height = "650px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Number of reported deaths",
                         withSpinner(plotOutput("plot2",height = "650px"))))),
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot1",height = "650px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot2",height = "650px"))))),
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot3",height = "650px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot4",height = "650px")))))
    ),
    
    ### TAB 3 = Analysis:
    tabItem(tabName = "analysis",
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot5",height = "650px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot6",height = "650px"))))),
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot7",height = "650px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Graphics",
                         withSpinner(plotOutput("plot8",height = "650px")))))
    ),
    
    ### TAB 4 = About
    tabItem(tabName = "about",
            fluidPage(
              box(width = 12,
                  shiny::includeMarkdown("README.md"))
            )
    )
  )
)

# Styles

ui <- dashboardPage(header, sidebar, body)

#---
