## ---------------------------
##
## Author: https://www.linkedin.com/in/mathieu-jacob/
## Adapted from: Ben Phillips - phillipsb@unimelb.edu.au
## Date Created: 2020-03-26
##
## ---------------------------
## load up the packages we will need:  

library("readr")
library("stringr")
## ---------------------------
## load up functions
source("functions.R")

## ---------------------------

## ---------------------------
## Get data
tsConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
tsDeath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
# tsTesting <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_testing_global.csv"

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
tsI <- tsI[tsI$Country.Region %in% c("Canada","US"),]
tsD <- tsD[tsD$Country.Region %in% c("Canada","US"),]

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
# ddNames <- tsACanada$Country
ddNames <- c("Canada",
             "Ontario",
             "Quebec",
             "Alberta",
             "British Columbia",
             "US",
             "New York",
             "New Jersey")
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




# 
# ## --------------------------- IN PROGRESS
# ## Get data for US - data curretly changing - temporary fix
# tsConf.US <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv"
# tsDeath.US <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Deaths_archived_0325.csv"
# 
# tsI.US<-read_csv(file = tsConf.US)
# tsD.US<-read_csv(file = tsDeath.US)
# 
# ## get Date range
# dCols<-dateCols(tsI.US)
# dates<-as.Date(colnames(tsI.US)[dCols], format = "%m/%d/%y")
# 
# ## Tidy up names
# names(tsI.US)[!dCols] <- make.names(names(tsI.US)[!dCols])
# names(tsD.US)[!dCols] <- make.names(names(tsD.US)[!dCols])
# 
# ## Filter for Canada
# tsI.US <- tsI.US[tsI.US$Province.State %in% c("New York", "New Jersey"),]
# tsD.US <- tsD.US[tsD.US$Province.State %in% c("New York", "New Jersey"),]
# 
# ## add recovery lag -- assumes all cases recover at 22 days
# matI<-as.matrix(tsI.US[, dCols])
# matD<-as.matrix(tsD.US[, dCols])
# matA<-matI-matD #remove deaths
# matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA))]) # recovered
# matA <- matA - matR
# 
# tsA.US <- cbind(tsI.US[,!dCols], matA) # active cases
# tsR.US <- cbind(tsI.US[,!dCols], matR) # recovered cases
# 
# tsI.US <- as.data.frame(tsI.US[,-(2:4)])
# tsA.US <- tsA.US[,-2]
# tsR.US <- tsR.US[,-2]
# tsD.US <- tsD.US[,-2]
# 
# 
# tsConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
# tsI<-read_csv(file = tsConf)
# max.date.CA <- names(tsI)[length(names(tsI))]
# max.date.US <- names(tsI.US)[length(names(tsI.US))]

## ---------------------------