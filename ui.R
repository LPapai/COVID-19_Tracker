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

# Define UI for application that draws a histogram
shinyUI(navbarPage("COVID-19 cases",
  theme = shinytheme("flatly"),
  tabPanel("Map",
           # Application title
           sidebarLayout(
             sidebarPanel(
               h3(textOutput("h1_1")),
               radioButtons("radio", "Select parameter to show:",
                            choices = list("Cumulative case count" = "CCases", "Cumulative death count" = "CDeaths",
                                           "Deaths/Cases ratio (per 100ppl)" = "C.DC", "Cases/Population ratio (per 1000ppl)" = "C.CP",
                                           "Growth Rate"="GrowthRate"),
                            selected = "CCases"),
               sliderInput("date", "Select date:",
                           min = as.Date("2019-12-30"), max = Sys.Date()-1,
                           value = Sys.Date())),
             mainPanel(
               leafletOutput("map"),
               p(),
               h2(textOutput("h1_2")),
               tableOutput('table')))
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
