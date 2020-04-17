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
shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  titlePanel("COVID-19 cases around the world"),
  p("This is a simple map showing some basic statistics regarding the COVID-19 pandemic up until the given date on the slider below.",br(),
    "The dataset is updated dynamically, and can be accessed from the link below."),
  p(a("Dataset can be downloaded from here. (.csv)",href="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", "Select parameter to show:",
                   choices = list("Cumulative case count" = "cases", "Cumulative death count" = "deaths",
                                  "Deaths/Cases ratio (per 100ppl)" = "ratioDC", "Cases/Population ratio (per 1000ppl)" = "ratioCP"),
                   selected = "cases"),
      sliderInput("date", "Select date:",
                  min = as.Date("2019-12-30"), max = Sys.Date()-1,
                  value = Sys.Date())),
    mainPanel(
      h2(textOutput("h1_1")),
      leafletOutput("map"),
      p(),
      h2(textOutput("h1_2")),
      tableOutput('table')))
))
