####################################
## Variables:
## ## CCases    - Cumulative case count
## ## CDeaths   - Cumulative death count
## ## C.CP      - Cumulative Cases / 1000 people
## ## C.DC      - Cumulative Deaths/Cases
## ## GrowthRate  - Growth rate/day, average of 7 days (specified in var grtu)
## ## pop       - Population in 2018
## ## country   - country
## ## geoId     - two-letter country code
## ## days.SinceFirstCase

library(shiny)
library(data.table)
library(leaflet)
library(magrittr)
library(htmltools)
library(plotly)
library(forecast)
source("functions.R")

#Replace vectors
rc<-c("CCases"="Cumulative cases","CDeaths"="Cumulative deaths",
      "C.DC"="Deaths/Cases ratio","C.CP"="Cases/1000people ratio",
      "GrowthRate"="Growth Rate", "pop"="Population","country"="Country")

###################
# Getting the dataset
filename <- paste0("cdc-covid19-data-",Sys.Date(),".csv")
if (!file.exists(filename)) {
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  data<-read.csv(url, na.strings = "", fileEncoding = "UTF-8-BOM")
  write.csv(data, file=filename)
} else {data<-read.csv(filename)}

countryISO<-read.csv("Country List Latitude Longitude ISO 3166 Codes.csv")

# Converting to data.table
data<-data.table(data)
countryISO<-data.table(countryISO)

# Renaming cols
setnames(countryISO, old = "Alpha.2.code", new = "geoId")
setnames(countryISO, old = "Latitude..average.", new = "lat")
setnames(countryISO, old = "Longitude..average.", new = "lng")
setnames(data, old = "popData2018", new = "pop")

# Specifying date class
data$dateRep<-as.Date(data$dateRep, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
setnames(data,"countriesAndTerritories","country")

# Growth Rate time frame
grtu <- 7
setkey(data,dateRep)

# Computing cumulative data and growth rate
data<-data[,':='(CCases=cumsum(cases),CDeaths=cumsum(deaths)),
           by=country][CCases>1,':='(GrowthRate= (CCases-shift(CCases,n=grtu))/
                                       (shift(CCases,n=grtu)-shift(CCases,n=2*grtu))/grtu,
                                     C.DC=100*CDeaths/CCases,
                                     C.CP=1000*CCases/pop[1],
                                     days.SinceFirstCase=dateRep-.SD[CCases>0,.(d=min(dateRep))]$d),
                       by=country]

# Joining tables
d <- merge(data,countryISO[,.(geoId,lat,lng)], by="geoId", all.x=T)
d <- d[!is.na(geoId)]

# Tooltip labels:
d$label <- LabelSnippet(country = d$country, date = d$dateRep, population = d$pop, cases=d$cases, CCases = d$CCases,
                        CDeaths = d$CDeaths,C.CP=d$C.CP,
                        C.DC = d$C.DC,
                        GR=d$GrowthRate)

###################
# Server logic
shinyServer(function(input, output, session) {
  dateR<-reactive ({as.Date(input$date)})
  dataR <- reactive({
    switch(input$radio,
           CCases={d[dateRep==dateR(),][order(-CCases),]},
           CDeaths={d[dateRep==dateR(),][order(-CDeaths),]},
           C.DC={d[dateRep==dateR(),][order(-C.DC),]},
           C.CP={d[dateRep==dateR(),][order(-C.CP),]},
           GrowthRate={d[dateRep==dateR(),][order(-GrowthRate),]},
           {d[dateRep==dateR(),][order(-CCases),]})
  })
  
  ####################
  ## Map
  
  ## Map init.
  output$map <- renderLeaflet({
    leaflet(d) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
      fitBounds(lat1=40, lng1=6, lng2 = 8, lat2 = 55)
  })
  
  ## Map update
  observe({
    ## Circle sizer: making up for the different ranges of the variables
    sizer <- switch(input$radio,
                    C.DC=5E4,
                    C.CP=1E5,
                    CCases=900,
                    CDeaths=1600,
                    GrowthRate=3E5,
                    1000
    )
    ## Leaflet
    leafletProxy("map", data = dataR()) %>%
      clearShapes() %>%
      addCircles(weight = 1, radius = sqrt(dataR()[[input$radio]])*sizer, fillOpacity=0.4, color="#cc1111", popup = ~label)
  })
  
  ####################
  ## Table
  output$table <- renderTable({
    t<-data.table(dataR()[,c(9,11:15)])
    currentCols <- names(rc)%in%names(t)
    setnames(t,old=(names(rc)[currentCols]),new=rc[currentCols])
    t})
  output$h1_1 <- renderText({paste(rc[input$radio], "on", dateR())})
  output$h1_2 <- renderText({paste(rc[input$radio], "by country on",dateR())})
  
  #####################
  ## navbar: Trajectory
  updateSelectizeInput(session, 'country', choices = unique(d$country), server = TRUE)
  output$trajectory <- renderPlotly(({
    if(is.null(input$country)) return(NULL)
    
    d.selected<-d[country %in% input$country,]
    
    axis.x <- list(title=paste("Cumulative",input$caseordeath),showgrid = FALSE)
    axis.y <- list(title=paste("New",input$caseordeath),showgrid = FALSE)
    
    if(input$log) {
      axis.x["type"] <- "log"
      axis.y["type"] <- "log"
      d.selected$cases<-d.selected$cases+1
    }
    if(input$ma) {
      d.selected$cases <- ma(d.selected$cases,2)
      d.selected$deaths <- ma(d.selected$deaths,2)
    }
    
    plot_ly(d.selected, x= ~get(ifelse(input$caseordeath=="cases","CCases","CDeaths")), 
            y = ~get(input$caseordeath), color=~country, hoverinfo='text', mode='lines+markers',
            sizes = c(1, 10), marker = list(sizemode = 'diameter'),
            text=~label) %>% 
      layout(title=paste("Trajectory of COVID-19 Confirmed",input$caseordeath),
             xaxis=axis.x, yaxis=axis.y) %>% 
      add_markers(text = ~text, size=~pop, showlegend = FALSE) %>% 
      add_lines(showlegend = TRUE)
  }))
  
  ####################
  ## navbar: Deaths/Cases
  output$`case-cumcase` <- renderPlotly({
    plot_ly(d[dateRep==max(dateRep) & CCases>50,][order(-pop),head(.SD,80)], type = 'scatter', mode = 'markers',
            x=~(CCases/as.numeric(days.SinceFirstCase)), y=~(C.DC), color=~country, size=~C.CP, hoverinfo='text', 
            sizes = c(10, 70), marker = list(sizemode = 'diameter'),
            text=~label) %>%
      layout(title="Relative deaths ~ cases (size=population)",
             xaxis=list(title="Avarage new cases per day", showgrid = FALSE,type = "log"),
             yaxis=list(title="Death count per 100 cases",showgrid = FALSE,type = "log"),
             height = 600)
  })
})