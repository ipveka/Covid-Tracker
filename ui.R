
#---

##### User Interface

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

### Title: ----------------------------------------------------------------

header <- dashboardHeader(title = "ShinyCovid")

### SideBar: ----------------------------------------------------------------

# helpText(h6("Select a country")),
# selectInput(inputId = "CountryId", label = h6("Countries"),
#             choices = ListCountries,selected = whichspain),
# helpText(h5(paste0("Last data:"," ",LastUpdate))),

sidebar <- dashboardSidebar(width = 250,
   
                            selectInput(inputId = "CountryId",
                                        shiny::HTML("<center><p>Select<span style='color:black'>Please select a country:</span></p></center>"),
                                        choices = ListCountries,selected = whichspain),
                            
                            helpText(h5(paste0("Last data:"," ",LastUpdate)),align = "center"),
                            
                            sidebarMenu(
                              
                              menuItem("Home", tabName = "home", icon = icon("fas fa-home")),
                              menuItem("Data", tabName = "data", icon = icon("fas fa-server")),
                              menuItem("Graphics", tabName = "graphics",  icon = icon("far fa-chart-bar")),
                              menuItem("Analysis", tabName = "analysis", icon = icon("fas fa-atom")),
                              menuItem("About", tabName = "about", icon = icon("fas fa-user")),
                              
                              hr(),
                              
                              helpText("Developed by ", 
                                       a("Ignasi Pascual", href = "https://github.com/ipveka"),
                                       align = "center")
                            )
)

### Dashboard: ----------------------------------------------------------------

body <- dashboardBody(
  
  # CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  # TabItems: ----------------------------------------------------------------
  
  tabItems(
    
    ### TAB 0 = Home: ----------------------------------------------------------------
    
    tabItem(tabName = "home",
            fluidPage(
              box(width = 12,
                  shiny::includeMarkdown("Home.md")))
            
    ),
    
    ### TAB 1 = Data: ----------------------------------------------------------------
    
    tabItem(tabName = "data",tags$head(includeCSS(path = "www/custom.css")),
            fluidRow(
              column(width = 4,
                     box(title = "Last update of data",width = 12,collapsible = TRUE,
                         helpText(strong(paste0("Last data:"," ",LastUpdate)),align = "center")
                     ),
                     box(title = "Numbef of confirmed cases",width = 12,collapsible = TRUE,
                         helpText(strong(textOutput("TotalCases")),align = "center")
                     ),
                     box(title = "Number of confirmed deaths",width = 12,collapsible = TRUE,
                         helpText(strong(textOutput("TotalDeaths")),align = "center")
                     )
              ),
              column(width = 8,
                     box(width = 12,collapsible = TRUE,
                         title = "Table of content",
                         withSpinner(dataTableOutput('Table1')))
                     )
              )
    ),
    
    ### TAB 2 = Graphics: ----------------------------------------------------------------
    
    tabItem(tabName = "graphics",
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Cases per day ",
                         withSpinner(plotlyOutput("plot1",height = "400px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Deaths per day",
                         withSpinner(plotlyOutput("plot2",height = "400px"))))),
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Cumulative cases per day",
                         withSpinner(plotlyOutput("plot3",height = "400px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Cumulative deaths per day",
                         withSpinner(plotlyOutput("plot4",height = "400px")))))
    ),

    # ### TAB 3 = Analysis: ----------------------------------------------------------------
    
    tabItem(tabName = "analysis",
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Log cases per day",
                         withSpinner(plotlyOutput("plot5",height = "400px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Log deaths per day",
                         withSpinner(plotlyOutput("plot6",height = "400px"))))),
            fluidRow(
              column(width = 6,
                     box(width = 12,
                         title = "Percentual change in cases per day",
                         withSpinner(plotlyOutput("plot7",height = "400px")))),
              column(width = 6,
                     box(width = 12,
                         title = "Percentual change in deaths per day",
                         withSpinner(plotlyOutput("plot8",height = "400px")))))
    ),
    
    ### TAB 4 = About ----------------------------------------------------------------
    
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
