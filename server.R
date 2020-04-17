#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(leaflet)
library(magrittr)
library(htmltools)

#FUNCTION: Number formatting and rounding
formatNum<-function(number){
    format(round(as.numeric(number),2),nsmall=1, big.mark = " ")
}
#Replace vectors
rc<-c("cases"="Cases","deaths"="Deaths","ratioDC"="Deaths/Cases ratio","ratioCP"="Cases/1000people ratio")

###################
# Getting the dataset
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
data<-read.csv(url, na.strings = "", fileEncoding = "UTF-8-BOM")
countryISO<-read.csv("Country List Latitude Longitude ISO 3166 Codes.csv")

# Renaming cols
setnames(countryISO, old = "Alpha.3.code", new = "countryterritoryCode")
setnames(countryISO, old = "Alpha.2.code", new = "geoId")
setnames(countryISO, old = "Latitude..average.", new = "lat")
setnames(countryISO, old = "Longitude..average.", new = "lng")

# Converting to data.table
data<-data.table(data)
countryISO<-data.table(countryISO)

# specifying date class
data$dateRep<-as.Date(data$dateRep, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))

# Computing cumulative sums and ratios:
latest<-data[order(dateRep),.(dateRep,cases=cumsum(cases),deaths=cumsum(deaths),
                              ratioDC=(100*cumsum(deaths)/cumsum(cases)),
                              ratioCP=(1000*cumsum(cases)/popData2018[1]),
                              pop=popData2018[1]),
             keyby=geoId]

# Joining tables
d <- merge(latest,countryISO[,.(geoId,Country,lat,lng)], by="geoId", all.x=T)
d <- d[!is.na(Country)]

#Tooltip labels:
d$label <- paste0("<b>",d$Country,"</b> (Pop:",formatNum(d$pop),")<br/>",
                  "No. of cases: ",formatNum(d$cases),"<br/>",
                  "No. of deaths: ",formatNum(d$deaths),"<br/>",
                  "Deaths/100 cases ratio: ",formatNum(d$ratioDC),"<br/>",
                  "Cases/1000ppl ratio: ",formatNum(d$ratioCP),"<br/>")

###############
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    dateR<-reactive ({as.Date(input$date)})
    dataR <- reactive({
        switch(input$radio,
               cases={d[dateRep==dateR(),][order(-cases),]},
               deaths={d[dateRep==dateR(),][order(-deaths),]},
               ratioDC={d[dateRep==dateR(),][order(-ratioDC),]},
               ratioCP={d[dateRep==dateR(),][order(-ratioCP),]},
               {d[dateRep==dateR(),][order(-cases),]})
    })
    
    output$map <- renderLeaflet({
        leaflet(d) %>% 
             addTiles() %>%
             fitBounds(lat1=40, lng1=6, lng2 = 8, lat2 = 55)
    })

    observe({
        sizer <- switch(input$radio,
                        ratioDC={sizer<-5E4},
                        ratioCP={sizer<-1E5},
                        cases={sizer<-900},
                        deaths={sizer<-1600},
                        {1000}
        )
        leafletProxy("map", data = dataR()) %>%
            clearShapes() %>%
            addCircles(weight = 0, radius = sqrt(dataR()[[input$radio]])*sizer, fillOpacity=0.4, color="#aa1188", popup = ~label)
    })
    
    output$table <- renderTable(dataR()[,c(8,3:6)])
    output$h1_1 <- renderText({paste(rc[input$radio], "until ", dateR())})
    output$h1_2 <- renderText({paste(rc[input$radio], "by country until ",dateR())})
})
