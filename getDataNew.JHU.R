########################################
# Author: https://www.linkedin.com/in/mathieu-jacob/
# Date Created: 2020-03-26
########################################
# Data Source: https://github.com/CSSEGISandData/COVID-19/ 


########################################
# Library & functions
########################################
library("readr")
library("tidyverse")
source("functions.R")

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
            "Florida")

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
                
                "US",
                "United States",
                "New York",
                "New Jersey",
                "Pennsylvania",
                "Connecticut",
                "South Carolina",
                "Massachusetts",
                "Florida")

########################################
# 1 - Read Data
########################################
# Soure
tsI.URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
tsD.URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
tsR.URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

tsI<-read_csv(file = tsI.URL)
tsD<-read_csv(file = tsD.URL)
tsR<-read_csv(file = tsR.URL)

########################################
# 2 - Tidy Up Data
########################################
## get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols], format = "%m/%d/%y")

## Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
names(tsR)[!dCols] <- make.names(names(tsR)[!dCols])

########################################
# 3 - Augment Data, Filter Country, Aggregate by Country
########################################
## add recovery lag -- assumes all cases recover at 22 days (see parameter section)
matI <- as.matrix(tsI[, dCols])
matD <- as.matrix(tsD[, dCols])
matA <- matI - matD #remove deaths
matR <- cbind(matrix(0, nrow = nrow(matA), ncol = reovery.time), matA[, -((ncol(matA)-(reovery.time-1)):ncol(matA))]) # recovered
matA <- matA - matR #remove recovered
tsA <- cbind(tsI[,!dCols], matA) # ts of active cases
tsR <- cbind(tsI[,!dCols], matR) # ts of recovered cases

#Helper Function to aggregate & select
SelectRegion<-function(x, keep){
  Province <- cbind(Country = x$Province.State [!is.na(x$Province.State )],
                    x[!is.na(x$Province.State ), dCols])
  
  CountryTotal <- aggregate(x[, dCols], by = list(Country = x$Country.Region), FUN = sum)
  out <- rbind(CountryTotal, Province)
  out[out$Country %in% keep,]
}

tsI <- tsI %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
tsA <- tsA %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
tsR <- tsR %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
tsD <- tsD %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)



########################################
## UI Parameters
########################################
# Drop down names
ddNames <- Region.Keep[Region.Keep %in% tsI$Country]
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
         "US",
         "New York",
         "New Jersey",
         "Pennsylvania",
         "Connecticut",
         "South Carolina",
         "Massachusetts",
         "Florida")
population <- data.frame(Country=Prov,
                         population = population)


#Selection for testing offline
input <- list()
input$countryFinder <- "United States"
input$countryFinder <- "Canada"
input$countryGrowthRate <- "Canada"
input$countryFinderCFI <- "Canada"

