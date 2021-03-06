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
  process.file <- function(type){
    if(type == "ref"){
      filename = "Reference_hospitalization"
    }else if(type == "best"){
      filename = "Best_mask_hospitalization"
    }else{
      filename = "Worse_hospitalization"
    }
    
    
    read.zip <- function(file){  #inZip is the ZIP Archive, varList is the variable specifications from readr
      outFile <- data.frame()
      
      fileList <- unzip(file, list = TRUE) # Create list of files
      fileSelected <- fileList[grepl("csv",fileList[,1]) & grepl(filename,fileList[,1]),1]
      oFa <- read_csv(unz(file, fileSelected),col_names=TRUE)
      outFile <- rbind(outFile,oFa)    #Then add the data files together
      return(outFile)  
    }
    data <- read.zip(file)
    
    CAN.Prov<- c("Ontario",
                 "Quebec",
                 "British Columbia",
                 "Alberta",
                 "Manitoba",
                 "Saskatchewan",
                 "Nova Scotia",
                 "New Brunswick",
                 "Newfoundland and Labrador",
                 "Prince Edward Island")
    data$Country.Region <- ifelse(data$location_name %in% CAN.Prov, "Canada", "United States")
    data$Province.State <- data$location_name
    
    tsI = data %>%
      select(date, Country.Region, Province.State, est_infections_mean) %>%
      arrange(Country.Region, Province.State, date, desc(est_infections_mean)) %>% 
      group_by(Country.Region, Province.State, date) %>%
      filter(row_number() == 1) %>%
      spread(key=date, value=est_infections_mean, fill =0)
    
    tsD = data %>%
      select(date, Country.Region, Province.State, totdea_mean) %>%
      arrange(Country.Region, Province.State, date, desc(totdea_mean)) %>% 
      group_by(Country.Region, Province.State, date) %>%
      filter(row_number() == 1) %>%
      spread(key=date, value=totdea_mean, fill =0)
    
    ########################################
    # 2 - Tidy Up Data
    ########################################
    ## get Date range
    dCols<-dateCols(tsD)
    dates<-as.Date(colnames(tsD)[dCols], format = "%Y-%m-%d")
    ## Tidy up names
    names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
    names(tsD)[dCols] <- as.character(dates)
    
    ## get Date range
    dCols<-dateCols(tsI)
    dates<-as.Date(colnames(tsI)[dCols], format = "%Y-%m-%d")
    ## Tidy up names
    names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
    names(tsI)[dCols] <- as.character(dates)
    
    ########################################
    # 3 - Augment Data, Filter Country, Aggregate by Country
    ########################################
    # 
    # # Incubation Period: 5 days (2.2-11.5 days: https://www.publichealthontario.ca/-/media/documents/ncov/research-lauer-anninternmed-the-incubation-period.pdf?la=en)
    # # Quarantine Period: 14 days
    # # Assumes Time to Death of 14 days
    # # Assumes fatality rate of 1.4% (http://www.cidrap.umn.edu/news-perspective/2020/03/global-covid-19-total-passes-850000-study-shows-14-fatality-rate)
    # 
    # fatality.rate <- 1.4/100
    # time.to.death <- 14
    # matD <- as.matrix(tsD[, dCols])
    # matI <- matD / fatality.rate
    # matI <- cbind(matI[, 1:(ncol(matI)-(time.to.death))], matrix(0, nrow = nrow(matI), ncol = time.to.death))
    # colnames(matI) <- colnames(matD)
    # tsI <- cbind(tsD[,!dCols], as.data.frame(matI)) # ts of active cases
    
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
    
    tsI$data.type <- paste0("tsI.IHME",".",type)
    tsD$data.type <- paste0("tsD.IHME",".",type)
    
    return(rbind(tsI, tsD))
  }
  
  file <- "./ihme-covid19.zip"
  # download.file("https://ihmecovid19storage.blob.core.windows.net/archive/2020-06-15/ihme-covid19.zip", file)
  download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", file)
  
  
  ref <- process.file("ref")
  best <- process.file("best")
  worst <- process.file("worst")
  return(rbind(ref, best, worst))
}

