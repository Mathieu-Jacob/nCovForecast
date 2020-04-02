########################################
# Author: https://www.linkedin.com/in/mathieu-jacob/
# Date Created: 2020-03-26
########################################
# Data Source: https://covidtracking.com/


########################################
# Library & functions
########################################
library("readr")
library("plyr")
library("tidyverse")


source("functions.R")
source("getDataNew.JHU.R")
source("getDataNew.CovidTracking.R")

########################################
# 0 - Parameters
########################################
reovery.time <- 22
Country <- c("Canada", "US", "United States")
Region <- c(#"Canada",
  "Ontario",
  "Quebec",
  "British Columbia",
  "Alberta",
  "Manitoba",
  "Saskatchewan",
  "Nova Scotia",
  "New Brunswick",
  "Newfoundland and Labrador",
  "Prince Edward Island",
  
  #"United States",
  "New York",
  "New Jersey",
  "Pennsylvania",
  "Connecticut",
  "South Carolina",
  "Massachusetts",
  "Florida",
  "Maine",
  "ME",
  "California",
  "CA")

Region.Keep<- c("Canada",
                "Ontario",
                "Alberta",
                "Quebec",
                "British Columbia",
                # "Manitoba",
                # "Saskatchewan",
                # "Nova Scotia",
                # "New Brunswick",
                # "Newfoundland and Labrador",
                # "Prince Edward Island",
                
                # "US",
                "United States",
                "New York",
                "NY",
                "New Jersey",
                "NJ",
                "Pennsylvania",
                "PA",
                "Connecticut",
                "CT",
                "South Carolina",
                "SC",
                "Massachusetts",
                "MA",
                "Florida",
                "FL",
                "Maine",
                "ME",
                "California",
                "CA")


########################################
# 1 - Import Data
########################################
data.US <- getDataNew.CovidTracking()
data.CA <- getDataNew.JHU()

missing.in.CA <- unique(names(data.US)[! names(data.US) %in% names(data.CA)])
missing.in.US <- unique(names(data.CA)[! names(data.CA) %in% names(data.US)])

data.US <- fill.missing(data.US, missing.in.US, fill=NA)
data.CA <- fill.missing(data.CA, missing.in.CA, fill=NA)

data <- rbind(data.US, data.CA)

 
########################################
## UI Parameters
########################################
# dates
dCols<-dateCols(data)
dates<-as.Date(colnames(data)[dCols], format = "%Y-%m-%d")

# Drop down names
ddNames <- unique(data$Country)[unique(data$Country) %in% Region.Keep]
ddReg <- ddNames
names(ddReg) <- ddNames

#Popultation
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
                330485691,
                19440469,
                19440469,
                8936574,
                8936574,
                12820878,
                12820878,
                3563077,
                3563077,
                5210095,
                5210095,
                6976597,
                6976597,
                21992985,
                21992985,
                1345790,
                1345790,
                39937489,
                39937489)
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
         "US",
         
         "New York",
         "NY",
         "New Jersey",
         "NJ",
         "Pennsylvania",
         "PA",
         "Connecticut",
         "CT",
         "South Carolina",
         "SC",
         "Massachusetts",
         "MA",
         "Florida",
         "FL",
         "Maine",
         "ME",
         "California",
         "CA")
population <- data.frame(Country=Prov,
                         population = population)


#Selection for testing offline
input <- list()
input$countryFinder <- "United States"
input$countryFinder <- "Canada"
input$countryFinder <- "United States"
input$countryFinder <- "CA"
input$countryGrowthRate <- c("Canada", "United States")
input$countryFinderCFI <- c("Canada", "United States")
