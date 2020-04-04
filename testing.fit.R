## load up the packages we will need 
library(shiny)
library(tidyverse)
library(plotly)
library(deSolve)
## ---------------------------

## source files
source("getDataNew.R")
source("functions.R")

pop <- population[population$Country %in% input$countryFinder,"population"]
yA <- tsSub(data, "tsA", input$countryFinder)
yD <- tsSub(data, "tsD", input$countryFinder)
yI <- tsSub(data, "tsI", input$countryFinder)
yR <- tsSub(data, "tsR", input$countryFinder)
yH <- tsSub(data, "tsH", input$countryFinder)
yT <- tsSub(data, "tsT", input$countryFinder)
yP <- tsSub(data, "tsP", input$countryFinder)

projection.period <- 70
data <- data.frame(dates = c(dates,max(dates)+1:projection.period),
                   yA = c(yA,rep(NA,projection.period)),
                   yD = c(yD,rep(NA,projection.period)),
                   yI = c(yI,rep(NA,projection.period)),
                   yR = c(yR,rep(NA,projection.period)),
                   yH = c(yH,rep(NA,projection.period)),
                   yT = c(yT,rep(NA,projection.period)),
                   yP = c(yP,rep(NA,projection.period)))
row.names(data) <- c()

add.exponential.2 <- function(data, min.count=50, as.of="2020-03-30", proj=70){
  Date.ID.Latest <- max(which(!is.na(data$yA)))
  Date.ID.AsOf <- which(data$dates == as.of)
  Date.ID.Max <- min(Date.ID.Latest, Date.ID.AsOf)
  Date.ID.Min <- min(which(data$yA > min.count))
  
  id.fit <- Date.ID.Min:Date.ID.Max
  dates.fit <- data$dates[id.fit]
  
  lnActive <- log(data$yA[id.fit])
  lnActive[is.infinite(lnActive)] <- NA
  model.exp <- lm(lnActive~dates.fit)
  
  id.predict <- Date.ID.Min:(Date.ID.Latest + proj)
  dates.predict <- data$dates[id.predict]
  yA.exp <- exp(predict(model.exp, newdata = list(dates.fit = dates.predict)))
  yA.exp <- data.frame(dates = dates.predict,
                       yA.exp = round(yA.exp))
  data <- left_join(data, yA.exp)
  data
}



data <- add.exponential(data, inWindow=10, proj=70)
data <- add.SIR(data, N=pop, R0=1.3)






maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
maxy <- 10^floor(log10(maxy)) * ceiling(maxy/10^floor(log10(maxy)))

p <- plot_ly(data=data) %>% 
  add_markers(name = 'Actual - Active', x = ~dates, y = ~yA) %>%
  add_markers(name = 'Actual - Deaths', x = ~dates, y = ~yD) %>%
  # add_markers(name = 'Actual - Hospitalized', x = ~dates, y = ~yH) %>%
  # add_markers(name = 'Actual - Tests Total', x = ~dates, y = ~yT) %>%
  # add_markers(name = 'Actual - Tests Pending', x = ~dates, y = ~yP) %>%
  add_lines(name = 'Exponential - Active', x = ~dates, y = ~yA.exp) %>% 
  add_lines(name = 'SIR R0=1.3 - Active', x = ~dates, y = ~yA.SIR.R1.3) %>% 
  add_lines(name = 'SIR R0=1.5 - Active', x = ~dates, y = ~yA.SIR.R1.5) %>% 
  add_lines(name = 'SIR R0=1.7 - Active', x = ~dates, y = ~yA.SIR.R1.7) %>% 
  add_lines(name = 'SIR R0=1.9 - Active', x = ~dates, y = ~yA.SIR.R1.9) %>% 
  add_lines(name = 'SIR R0=2.1 - Active', x = ~dates, y = ~yA.SIR.R2.1) %>% 
  add_lines(name = 'SIR R0=2.3 - Active', x = ~dates, y = ~yA.SIR.R2.3) %>% 
  layout(title = paste0(input$countryFinder,': Projection Over Time'),
         xaxis = list(title = "Dates"),
         yaxis = list(title = "Number of People", range=c(0,maxy)) )
p