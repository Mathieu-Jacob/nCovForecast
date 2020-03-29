
library("readr")
library("tidyverse")


file <- "https://coronadatascraper.com/timeseries-tidy.csv"

data <- read_csv(file)

data$Key.full <- paste0(replace_na(data$city, "ALL"), "/", replace_na(data$county, "ALL"), "/", replace_na(data$state, "ALL"), "/", replace_na(data$country, "ALL"))
data$Key.state <- paste0(replace_na(data$state, "ALL"), "/", replace_na(data$country, "ALL"))

keep<-c("ALL/ALL/ALL/CAN",
        "ALL/ALL/ALL/USA",
        "ALL/ALL/ON/CAN",
        "ALL/ALL/QC/CAN",
        "ALL/ALL/AB/CAN",
        "ALL/ALL/BC/CAN",
        "ALL/ALL/NY/USA",
        "ALL/ALL/NJ/USA",
        "ALL/ALL/PA/USA",
        "ALL/ALL/CT/USA",
        "ALL/ALL/SC/USA",
        "ALL/ALL/MA/USA",
        "ALL/ALL/FL/USA")

data <- data[data$Key.full %in% keep & data$type %in% c("cases","deaths"),c("Key.state", "population", "date", "type", "value")]

data %>% filter(type=="cases" & Key.state == "ALL/USA") %>% 
  spread(date, value)

gather(data[data$typ], key=, value=)
tail(data %>% filter(type=="cases" & Key.state == "ALL/USA"))
