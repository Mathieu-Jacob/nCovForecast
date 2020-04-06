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

getDataNew.IHME <- function(){
  ########################################
  # 1 - Read Data
  ########################################
  file <- "./ihme-covid19.zip"
  download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", file)
  
  read.zip <- function(file){  #inZip is the ZIP Archive, varList is the variable specifications from readr
    outFile <- data.frame()
    
    fileList <- unzip(file, list = TRUE) # Create list of files
    for(f in 1:nrow(fileList)) { # Loop through the list of files
      if(grepl("csv",fileList[f,1])) {  #If a file is a csv file, unzip it and read the data
        oFa <- read_csv(unz(file, fileList[f,1]),col_names=TRUE)
        outFile <- rbind(outFile,oFa)    #Then add the data files together
      }
    }
    return(outFile)  
  }
  data <- read.zip(file)
  

  data$Country.Region <- "United States"
  data$Province.State <- data$location_name
  tsD <- data %>% select(date, Country.Region, Province.State, totdea_mean) %>% distinct() %>% spread(key=date, value=totdea_mean, fill =0)
  tsDl <- data %>% select(date, Country.Region, Province.State, totdea_lower) %>% distinct() %>% spread(key=date, value=totdea_lower, fill =0)
  tsDu <- data %>% select(date, Country.Region, Province.State, totdea_upper) %>% distinct() %>% spread(key=date, value=totdea_upper, fill =0)
  
  ########################################
  # 2 - Tidy Up Data
  ########################################
  ## get Date range
  dCols<-dateCols(tsD)
  dates<-as.Date(colnames(tsD)[dCols], format = "%Y-%m-%d")
  
  ## Tidy up names
  names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
  names(tsDl)[!dCols] <- make.names(names(tsDl)[!dCols])
  names(tsDu)[!dCols] <- make.names(names(tsDu)[!dCols])
  
  names(tsD)[dCols] <- as.character(dates)
  names(tsDl)[dCols] <- as.character(dates)
  names(tsDu)[dCols] <- as.character(dates)
  
  ########################################
  # 3 - Augment Data, Filter Country, Aggregate by Country
  ########################################
  
  # Incubation Period: 5 days (2.2-11.5 days: https://www.publichealthontario.ca/-/media/documents/ncov/research-lauer-anninternmed-the-incubation-period.pdf?la=en)
  # Quarantine Period: 14 days
  # Assumes Time to Death of 14 days
  # Assumes fatality rate of 1.4% (http://www.cidrap.umn.edu/news-perspective/2020/03/global-covid-19-total-passes-850000-study-shows-14-fatality-rate)
  
  fatality.rate <- 1.4/100
  time.to.death <- 14
  matD <- as.matrix(tsD[, dCols])
  matI <- matD / fatality.rate
  matI <- cbind(matI[, 1:(ncol(matI)-(time.to.death))], matrix(0, nrow = nrow(matI), ncol = time.to.death))
  colnames(matI) <- colnames(matD)
  tsI <- cbind(tsD[,!dCols], matI) # ts of active cases
  
  #Helper Function to aggregate & select
  SelectRegion<-function(x, keep){
    Province <- cbind(Country = x$Province.State [!is.na(x$Province.State )],
                      x[!is.na(x$Province.State ), dCols])
    
    CountryTotal <- aggregate(x[, dCols], by = list(Country = x$Country.Region), FUN = sum)
    out <- rbind(CountryTotal, Province)
    out[out$Country %in% keep,]
  }
  
  tsI <- tsI %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
  tsD <- tsD %>% filter(Country.Region %in% Country) %>% SelectRegion(., Region.Keep)
  
  tsI$data.type <- "tsI.IHME"
  tsD$data.type <- "tsD.IHME"
  
  return(rbind(tsI, tsD))
}



