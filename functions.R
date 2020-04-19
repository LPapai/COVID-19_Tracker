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
            ifelse(is.na(population),'',paste('<br>Population:',formatNum(population,0))),
            ifelse(is.na(cases),'',paste('<br>New cases:',formatNum(cases,0))),
            ifelse(is.na(CCases),'',paste('<br>Cumulative cases:',formatNum(CCases,0))),
            ifelse(is.na(CDeaths),'',paste('<br>Cumulative deaths:',formatNum(CDeaths,0))),
            ifelse(is.na(C.CP),'',paste('<br>Cases/1000ppl ratio:',formatNum(C.CP,2))),
            ifelse(is.na(C.DC),'',paste('<br>Deaths/100 cases ratio:',formatNum(C.DC,2))),
            ifelse(is.na(GR),'',paste('<br>Growth rate:',formatNum(GR,2)))
      )
}