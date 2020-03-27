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
########################################
## Get data
########################################
#Region to get
Country <- c("Canada", "United States")
Region<- c("Ontario",
           "Quebec",
           "British Columbia",
           "Alberta",
           "Manitoba",
           "Saskatchewan",
           "Nova Scotia",
           "New Brunswick",
           "Newfoundland and Labrador",
           "Prince Edward Island",
           "Canada",
           "United States",
           "New York",
           "New Jersey",
           "Pennsylvania",
           "Connecticut",
           "South Carolina",
           "Massachusetts",
           "Florida")

# Soure
tsConfirmed <- "https://coviddata.github.io/covid-api/v1/regions/cases.csv"
tsDeaths <- "https://coviddata.github.io/covid-api/v1/regions/deaths.csv"
tsRecovered <- "https://coviddata.github.io/covid-api/v1/regions/recoveries.csv"

tsI<-read_csv(file = tsConfirmed)
tsD<-read_csv(file = tsDeaths)
tsR<-read_csv(file = tsRecovered)

## Filter for Canada & US
tsI <- tsI[tsI$Country %in% Country,]
tsD <- tsD[tsD$Country %in% Country,]
tsR <- tsD[tsD$Country %in% Country,]

## get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols], format = "%Y-%m-%d")

## Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
names(tsR)[!dCols] <- make.names(names(tsR)[!dCols])

## add recovery lag -- assumes all cases recover at 22 days
matI<-as.matrix(tsI[, dCols])
matD<-as.matrix(tsD[, dCols])
matR<-as.matrix(tsR[, dCols])
matA <-matI-matD - matR #remove deaths & recovered
tsA <- cbind(tsI[,!dCols], matA) # active cases


## Final data
tsICanada <- SelectRegion(tsI,keep=Region)
tsACanada <- SelectRegion(tsA,keep=Region)
tsRCanada <- SelectRegion(tsR,keep=Region)
tsDCanada <- SelectRegion(tsD,keep=Region)



########################################
## Other
########################################
## Define menus
# ddNames <- tsACanada$Country
ddNames <- c("Canada",
             "Ontario",
             "Quebec",
             "Alberta",
             "British Columbia",
             "United States",
             "New York",
             "New Jersey",
             "Pennsylvania",
             "Connecticut",
             "South Carolina",
             "Massachusetts",
             "Florida")
ddReg <- ddNames
names(ddReg) <- ddNames
#ddReg <- paste(ddReg, collapse = ", ") # menu specifier


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
                37654671,
                330485691,
                19440469,
                8936574,
                12820878,
                3563077,
                5210095,
                6976597,
                21992985)
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
         "Canada",
         "United States",
         "New York",
         "New Jersey",
         "Pennsylvania",
         "Connecticut",
         "South Carolina",
         "Massachusetts",
         "Florida")
population <- data.frame(Country=Prov,
                        population = population)
