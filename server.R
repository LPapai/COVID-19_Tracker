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
library(plotly)
library(forecast)

#FUNCTION: Number formatting and rounding
formatNum<-function(number,precision=2){
  format(round(as.numeric(number),precision),nsmall=precision, big.mark = " ")
}

#FUNCTION: hover snippet
LabelSnippet <- function(country=NA,date=NA,population=NA, cases=NA,
                         CCases=NA,CDeaths=NA,C.CP=NA,C.DC=NA,
                         GR=NA,daysSinceFirst=NA){
  paste(ifelse(is.na(country),'',paste('<b>',country,'</b>')),
        ifelse(is.na(date),'',paste('(@',date,')')),
        ifelse(is.na(cases),'',paste('<br>New cases:',formatNum(cases,0))),
        ifelse(is.na(population),'',paste('<br>Population:',formatNum(population,0))),
        ifelse(is.na(CCases),'',paste('<br>Cumulative cases:',formatNum(CCases,0))),
        ifelse(is.na(CDeaths),'',paste('<br>Cumulative deaths:',formatNum(CDeaths,0))),
        ifelse(is.na(C.CP),'',paste('<br>Cases/1000ppl ratio:',formatNum(C.CP,2))),
        ifelse(is.na(C.DC),'',paste('<br>Deaths/100 cases ratio:',formatNum(C.DC,2))),
        ifelse(is.na(GR),'',paste('<br>Growth rate:',formatNum(GR,2)))
  )
}
#Replace vectors
rc<-c("CCases"="Cumulative cases","CDeaths"="Cumulative deaths",
      "C.DC"="Deaths/Cases ratio","C.CP"="Cases/1000people ratio",
      "GrowthRate"="Growth Rate", "pop"="Population","country"="Country")

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

# Specifying date class
data$dateRep<-as.Date(data$dateRep, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
setnames(data,"countriesAndTerritories","country")

# Growth Rate time frame
grtu <- 7
setkey(data,dateRep)

# Computing cumulative data and growth rate
data<-data[,':='(CCases=cumsum(cases),CDeaths=cumsum(deaths),
              pop=popData2018[1]),
        by=country][CCases>10,':='(GrowthRate= (CCases-shift(CCases,n=grtu))/(shift(CCases,n=grtu)-shift(CCases,n=2*grtu)),
                                             C.DC=100*CDeaths/CCases,
                                             C.CP=1000*CCases/popData2018[1]), 
                    by=country][,
                      days.SinceFirstCase:=dateRep-.SD[CCases>0,.(d=min(dateRep))]$d,
                      by=country]

# Joining tables
d <- merge(data,countryISO[,.(geoId,lat,lng)], by="geoId", all.x=T)
d <- d[!is.na(geoId)]

# Tooltip labels:
d$label <- LabelSnippet(country = d$country,population = d$pop,CCases = d$CCases,
                        CDeaths = d$CDeaths,C.CP=d$C.CP,
                        C.DC = d$C.DC,
                        GR=d$GrowthRate)

###############
# Define server logic required to draw a histogram
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
  
  ## MAP init.
  output$map <- renderLeaflet({
    leaflet(d) %>% 
      addTiles() %>%
      fitBounds(lat1=40, lng1=6, lng2 = 8, lat2 = 55)
  })
  
  ## MAP update
  observe({
    ## Circle sizer
    sizer <- switch(input$radio,
                    C.DC=5E4,
                    C.CP=1E5,
                    CCases=900,
                    CDeaths=1600,
                    GrowthRate=1E5,
                    1000
    )
    ## Leaflet
    leafletProxy("map", data = dataR()) %>%
      clearShapes() %>%
      addCircles(weight = 0, radius = sqrt(dataR()[[input$radio]])*sizer, fillOpacity=0.4, color="#aa1188", popup = ~label)
  })
  
  ## Table and headers
  output$table <- renderTable({
    t<-data.table(dataR()[,c(8,11:15)])
    setnames(t,old=(names(rc)[names(rc)%in%names(t)]),new=rc[names(rc)%in%names(t)])
    t})
  output$h1_1 <- renderText({paste(rc[input$radio], "on", dateR())})
  output$h1_2 <- renderText({paste(rc[input$radio], "by country on",dateR())})
  
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
            text=~LabelSnippet(country=country,date=dateRep, population=pop, cases=cases,
                               CCases = CCases, CDeaths = CDeaths,
                               GR=GrowthRate,
                               C.DC = C.DC)) %>% 
      layout(title=paste("Trajectory of COVID-19 Confirmed",input$caseordeath),
             xaxis=axis.x, yaxis=axis.y) %>% 
      add_markers(text = ~text, size=~pop, showlegend = FALSE) %>% 
      add_lines(showlegend = TRUE)
  }))
  
  ## navbar: MORE STAT
  
  output$`case-cumcase` <- renderPlotly({
    plot_ly(d[dateRep==max(dateRep) & CCases>50,][order(-pop),head(.SD,80)], type = 'scatter', mode = 'markers',
            x=~(CCases/as.numeric(days.SinceFirstCase)), y=~(C.DC), color=~country, size=~C.CP, hoverinfo='text', 
            sizes = c(10, 70), marker = list(sizemode = 'diameter'),
            text=~LabelSnippet(country=country,date=dateRep, population=pop,
                               CCases = CCases, CDeaths = CDeaths,
                               GR=GrowthRate,
                               C.DC=C.DC)) %>%
      layout(title="Relative deaths ~ cases (size=population)",
             xaxis=list(title="Avarage new cases per day", showgrid = FALSE,type = "log"),
             yaxis=list(title="Death count per 100 cases",showgrid = FALSE,type = "log"),
             height = 600)
  })
})
