## ---------------------------
##
## Script name: getData.R
##
## Purpose of script: Scrape data from gitHub repository established to track nCov20
##
## Author: Ben Phillips - modified by Mathieu Jacob
##
## Date Created: 2020-02-07
## Date modified: 2020-03-24
##
## Copyright (c) Ben Phillips, 2020
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need:  

library("readr")

## ---------------------------
## load up functions
source("functions.R")

## ---------------------------


## Get data
tsConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
tsDeath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
tsTesting <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_testing_global.csv"

tsI<-read_csv(file = tsConf)
tsD<-read_csv(file = tsDeath)
#tsT<-read_csv(file = tsTesting)

## get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols], format = "%m/%d/%y")

## Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
#names(tsT)[!dCols] <- make.names(names(tsT)[!dCols])

## Filter for Canada
tsI <- tsI[tsI$Country.Region == "Canada",]
tsD <- tsD[tsD$Country.Region == "Canada",]

## add recovery lag -- assumes all cases recover at 22 days
matI<-as.matrix(tsI[, dCols])
matD<-as.matrix(tsD[, dCols])
matA<-matI-matD #remove deaths
matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA))]) # recovered
matA <- matA - matR

tsA <- cbind(tsI[,!dCols], matA) # active cases
tsR <- cbind(tsI[,!dCols], matR) # recovered cases

# tsACountry <- countryAgg(tsA) # aggregated to country
# tsACountry <- tsACountry[rev(order(tsACountry[[ncol(tsACountry)-1]])),] # ordered from most to least active cases

##Build Canada only data:
tsICanada <- SelectCountry(tsI, "Canada")
tsACanada <- SelectCountry(tsA, "Canada")
tsRCanada <- SelectCountry(tsR, "Canada")
tsDCanada <- SelectCountry(tsD, "Canada")


## Define menus
ddNames <- tsACanada$Country
ddReg <- ddNames
names(ddReg) <- ddNames
#ddReg <- paste(ddReg, collapse = ", ") # menu specifier

#save(tsI, tsD, tsR, tsA, tsACountry, dates, ddNames, ddReg, file = paste0("dat/cacheData", format(Sys.Date(), format = "%m%d"), ".RData"))

# 
# yI <- tsICanada %>% filter(Country == input$countryFinder) %>% .[,-1]
# yA <- tsACanada %>% filter(Country == input$countryFinder) %>% .[,-1]
# yR <- tsRCanada %>% filter(Country == input$countryFinder) %>% .[,-1]
# yD <- tsDCanada %>% filter(Country == input$countryFinder) %>% .[,-1]
# 
# 

input <- list()
input$countryFinder <- "Canada"
input$countryGrowthRate <- "Canada"
input$countryFinderCFI <- "Canada"

population <- c(14446515,
                8433301,
                5020302,
                4345737,
                1360396,
                1168423,
                965382,
                772094,
                523790,
                154748,
                37654671)
Prov<- c("Ontario",
                      "Quebec",
                      "British Columbia",
                      "Alberta",
                      "Manitoba",
                      "Saskatchewan",
                      "Nova Scotia",
                      "New Brunswick",
                      "Newfoundland and Labrador",
                      "Prince Edward Island",
                      "Canada")
population <- data.frame(Country=Prov,
                        population = population)

