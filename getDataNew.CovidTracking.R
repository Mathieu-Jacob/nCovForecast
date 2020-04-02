########################################
# Author: https://www.linkedin.com/in/mathieu-jacob/
# Date Created: 2020-03-26
########################################
# Data Source: https://covidtracking.com/


########################################
# Library & functions
########################################
library("readr")
library("tidyverse")

getDataNew.CovidTracking <- function(){
  ########################################
  # 1 - Read Data
  ########################################
  # Soure
  file <- "http://covidtracking.com/api/states/daily.csv"
  
  data <- read_csv(file)
  data$Country.Region <- "United States"
  data$Province.State <- data$state
  tsI <- data %>% select(dateChecked, Country.Region, Province.State, positive) %>% spread(key=dateChecked, value=positive, fill =0)
  tsD <- data %>% select(dateChecked, Country.Region, Province.State, death) %>% spread(key=dateChecked, value=death, fill =0)
  tsH <- data %>% select(dateChecked, Country.Region, Province.State, hospitalized) %>% spread(key=dateChecked, value=hospitalized, fill =0)
  tsT <- data %>% select(dateChecked, Country.Region, Province.State, totalTestResults) %>% spread(key=dateChecked, value=totalTestResults, fill =0)
  tsP <- data %>% select(dateChecked, Country.Region, Province.State, pending) %>% spread(key=dateChecked, value=pending, fill =0)
  
  ########################################
  # 2 - Tidy Up Data
  ########################################
  ## get Date range
  dCols<-dateCols(tsI)
  dates<-as.Date(colnames(tsI)[dCols], format = "%Y-%m-%d")
  
  ## Tidy up names
  names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
  names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
  names(tsT)[!dCols] <- make.names(names(tsT)[!dCols])
  names(tsH)[!dCols] <- make.names(names(tsH)[!dCols])
  names(tsP)[!dCols] <- make.names(names(tsP)[!dCols])
  
  names(tsI)[dCols] <- as.character(dates)
  names(tsD)[dCols] <- as.character(dates)
  names(tsT)[dCols] <- as.character(dates)
  names(tsH)[dCols] <- as.character(dates)
  names(tsP)[dCols] <- as.character(dates)
  ########################################
  # 3 - Augment Data, Filter Country, Aggregate by Country
  ########################################
  ## add recovery lag -- assumes all cases recover at 22 days (see parameter section)
  matI <- as.matrix(tsI[, dCols])
  matD <- as.matrix(tsD[, dCols])
  matA <- matI - matD #remove deaths
  matR <- cbind(matrix(0, nrow = nrow(matA), ncol = reovery.time), matA[, -((ncol(matA)-(reovery.time-1)):ncol(matA))]) # recovered
  colnames(matR) <- colnames(matA)
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
  tsH <- tsH %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
  tsT <- tsT %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
  tsP <- tsP %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
  
  tsI$data.type <- "tsI"
  tsA$data.type <- "tsA"
  tsR$data.type <- "tsR"
  tsD$data.type <- "tsD"
  tsH$data.type <- "tsH"
  tsT$data.type <- "tsT"
  tsP$data.type <- "tsP"
  
  return(rbind(tsI, tsA, tsR, tsD, tsH, tsT, tsP))
}



