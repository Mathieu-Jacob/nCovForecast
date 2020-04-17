library(shiny)
library(tidyverse)
library(plotly)
library(deSolve)
## ---------------------------
## source files
source("getDataNew.R")
source("functions.R")

add.SIR <- function(data, N=pop, R0=R, inWindow=10, proj=10, input.asofmodel="2020-04-03", input.countryFinder = "Canada"){
  Region.fit <- input.countryFinder
  
  max.yA <- max(which(!is.na(data$yA)))
  max.fit <- min(max.yA,which(data$dates == input.asofmodel))
  
  min.time <- min(which(data$yA>=50))
  start.time <- max(min.time, max.fit-inWindow+1)
  
  id.fit <- start.time:max.fit
  dates.fit <- data$dates[id.fit]
  
  id.predict <- min(id.fit):(max.yA+proj)
  dates.predict <- data$dates[id.predict]
  
  SIR <- function(time, state, parameters) {
    # https://stats.stackexchange.com/questions/446712/fitting-sir-model-with-2019-ncov-data-doesnt-conververge
    par <- as.list(c(state, parameters))
    ####
    #### use as change of variables variable
    #### const = (beta-gamma)
    #### delta = gamma/beta
    #### R0 = beta/gamma > 1 
    #### 
    #### beta-gamma = beta*(1-delta)
    #### beta-gamma = beta*(1-1/R0)
    #### gamma = beta/R0
    with(par, { 
      beta  <- const/(1-1/R0)  
      gamma <- const/(R0-1)  
      dS <- -(beta * (S/N)      ) * I 
      dI <-  (beta * (S/N)-gamma) * I 
      dR <-  (             gamma) * I
      list(c(dS, dI, dR))
    })
  }
  
  nbpoints = ifelse(length(id.fit)<inWindow,length(id.fit),inWindow)
  const <- exponential.slope(data, type="yI", inWindow=nbpoints, input.asofmodel=input.asofmodel, input.countryFinder = input.countryFinder)
  
  init <- c(S = N - data$yI[start.time], I = data$yI[start.time], R = data$yD[start.time])
  fit <- round(ode(y = init, times = id.predict, func = SIR, parms = c(const, R0)))[,-1]
  colnames(fit) <- paste0(c("yS.SIR.R", "yA.SIR.R", "yR.SIR.R"),R0)
  fit <- cbind(data.frame(dates=dates.predict), fit)
  
  data <- left_join(data, fit)
  data
}



{

  
  ## ---------------------------
  options(scipen=9)
  
  
  input.asofmodel <- input$asofmodel
  input.countryFinder <- input$countryFinder
  
  pop <- population[population$Country %in% input$countryFinder,"population"]
  yA <- tsSub(data, "tsA", input$countryFinder)
  yD <- tsSub(data, "tsD", input$countryFinder)
  yI <- tsSub(data, "tsI", input$countryFinder)
  yR <- tsSub(data, "tsR", input$countryFinder)
  yH <- tsSub(data, "tsH", input$countryFinder)
  yT <- tsSub(data, "tsT", input$countryFinder)
  yP <- tsSub(data, "tsP", input$countryFinder)
  
  
  
  yD.IHME <- tsSub(data.IHME, "tsD.IHME", input$countryFinder)
  yI.IHME <- tsSub(data.IHME, "tsI.IHME", input$countryFinder)
  
  min.data <- names(yA)[1]
  min.data.IHME <- names(yD.IHME)[1]
  cols.to.add <- as.Date(min.data.IHME) - as.Date(min.data)
  max.cols <- length(yA) + 70
  yD.IHME <- c(rep(NA,cols.to.add), yD.IHME)[1:(min(length(yD.IHME),max.cols))]
  yI.IHME <- c(rep(NA,cols.to.add), yI.IHME)[1:(min(length(yI.IHME),max.cols))]
  
  
  
  
  
  projection.period <- 70
  data <- data.frame(dates = c(dates,max(dates)+1:projection.period),
                     yA = c(yA,rep(NA,projection.period)),
                     yD = c(yD,rep(NA,projection.period)),
                     yI = c(yI,rep(NA,projection.period)),
                     yR = c(yR,rep(NA,projection.period)),
                     yH = c(yH,rep(NA,projection.period)),
                     yT = c(yT,rep(NA,projection.period)),
                     yP = c(yP,rep(NA,projection.period)),
                     yD.IHME = yD.IHME,
                     yI.IHME = yI.IHME)
  row.names(data) <- c()
  
  print(input$asofmodel)
  
  data <- add.exponential(data, type="yA", inWindow=10, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.exponential(data, type="yI", inWindow=10, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.exponential(data, type="yD", inWindow=10, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=1.05, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=1.1, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=1.2, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=1.3, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=1.5, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=1.7, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=1.9, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=2.1, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
  data <- add.SIR(data, N=pop, R0=2.3, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)


  data$yI.SIR.R1.05 <- data$yA.SIR.R1.05 + data$yR.SIR.R1.05
  data$yI.SIR.R1.1 <- data$yA.SIR.R1.1 + data$yR.SIR.R1.1

  
  
}






maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
maxy <- 10^floor(log10(maxy)) * ceiling(maxy/10^floor(log10(maxy)))

p <- plot_ly(data=data) %>% 
  add_markers(name = 'Actual - Active', x = ~dates, y = ~yA) %>%
  add_markers(name = 'Actual - Infected', x = ~dates, y = ~yI) %>%
  add_markers(name = 'Actual - Deaths', x = ~dates, y = ~yD) %>%

  
  add_lines(name = 'Exponential - Active', x = ~dates, y = ~yA.exp) %>%
  
  add_lines(name = 'SIR R0=1.05 - Active', x = ~dates, y = ~yA.SIR.R1.05) %>% 
  add_lines(name = 'SIR R0=1.1 - Active', x = ~dates, y = ~yA.SIR.R1.1) %>% 
  # add_lines(name = 'SIR R0=1.2 - Active', x = ~dates, y = ~yA.SIR.R1.2) %>% 
  # add_lines(name = 'SIR R0=1.3 - Active', x = ~dates, y = ~yA.SIR.R1.3) %>% 
  # add_lines(name = 'SIR R0=1.5 - Active', x = ~dates, y = ~yA.SIR.R1.5) %>% 
  # add_lines(name = 'SIR R0=1.7 - Active', x = ~dates, y = ~yA.SIR.R1.7) %>% 
  # add_lines(name = 'SIR R0=1.9 - Active', x = ~dates, y = ~yA.SIR.R1.9) %>% 
  # add_lines(name = 'SIR R0=2.1 - Active', x = ~dates, y = ~yA.SIR.R2.1) %>% 
  # add_lines(name = 'SIR R0=2.3 - Active', x = ~dates, y = ~yA.SIR.R2.3) %>% 
  # add_lines(name = 'IHME - Cumul Cases', x = ~dates, y = ~yI.IHME) %>% 
  # add_lines(name = 'IHME - Deaths', x = ~dates, y = ~yD.IHME) %>% 
  layout(title = paste0(input$countryFinder,': Projection Over Time'),
         xaxis = list(title = "Dates"),
         yaxis = list(title = "Number of People", range=c(0,maxy)) )
p
