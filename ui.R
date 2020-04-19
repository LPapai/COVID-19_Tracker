#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(navbarPage("COVID-19", collapsible = TRUE,
  theme = shinytheme("flatly"),
  tabPanel("Map",
           div(class="wrapper", style="position: fixed;
           top: 35px; padding: 0;
           left: 0; right: 0; bottom: 0;
           overflow: hidden; overflow-y:scroll;",
               conditionalPanel("input.mapOrTable=='map'", tags$style(type = "text/css", "#map {height: calc(100vh - 40px) !important;}"),
                                leafletOutput("map")),
               conditionalPanel("input.mapOrTable=='table'", style="overflow-y:scroll;",
                                column(8, offset=2,
                                  h2(textOutput("h1_2")),
                                tableOutput('table'))),
               absolutePanel(id = "controls", tags$style(type = "text/css", "
                                        #controls {opacity:0.4; transition:150ms; background-color:#fff; padding:10px 25px; border-radius:5px; width: 25vw !important; min-width:250px;} 
                                        #controls:hover {opacity:0.8; transition:150ms;}
                                        #controls .shiny-input-container {width:100% !important;}"),
                             top = 80, left = 20, width = 250, fixed=TRUE,
                             draggable = TRUE, height = "auto",
                             h3(textOutput("h1_1"), align="center"),
                             selectInput("mapOrTable","Select output type:",
                                         choices=list("map"="map","table"="table"), selected = "map"),
                             radioButtons("radio", "Select parameter to show:",
                                          choices = list("Cumulative case count" = "CCases", "Cumulative death count" = "CDeaths",
                                                         "Deaths/Cases ratio (per 100ppl)" = "C.DC", "Cases/Population ratio (per 1000ppl)" = "C.CP",
                                                         "Growth Rate"="GrowthRate"),
                                          selected = "CCases"),
                             sliderInput("date", "Select date:",
                                         min = as.Date("2019-12-30"), max = Sys.Date()-1,
                                         value = Sys.Date())
               ))
  ),
  tabPanel("Trajectory",
           selectizeInput("country","Select countries to show", multiple=T,
                          choices=NULL),
           selectInput("caseordeath","Select the base of the data",
                       choices = list("Confirmed cases"="cases","Confirmed deaths"="deaths"), selected = "cases"),
           checkboxInput("log","Logarithmic scales"),
           checkboxInput("ma","Moving average (2days)",value=T),
           plotlyOutput("trajectory")
  ),
  tabPanel("Deaths/Cases",
           column(8,plotlyOutput("case-cumcase"), offset=2)
  ),
  tabPanel("About",
           titlePanel("COVID-19 cases around the world"),
           p("This is a simple map showing some basic statistics regarding the COVID-19 pandemic up until the given date on the slider below.",br(),
             "The dataset is updated dynamically, and can be accessed from the link below."),
           p(a("Dataset can be downloaded from here. (.csv)",href="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"))
  )
))
