########################################
# Author: https://www.linkedin.com/in/mathieu-jacob/
# Date Created: 2020-03-26
# Adapted from: Ben Phillips - phillipsb@unimelb.edu.au
########################################

########################################
# getDataNew Functions
########################################
# to identify the date columns in ts dataframes
dateCols<-function(x){
  grepl(pattern = "\\d", x = colnames(x))
}

# Add aggregated results to country level & clean columns
SelectRegion<-function(x, keep){
  Province <- cbind(Country = x$Region [!is.na(x$Province.State )],
                    x[!is.na(x$Region ), dCols])
  
  CountryTotal <- aggregate(x[, dCols], by = list(Country = x$Country), FUN = sum)
  out <- rbind(CountryTotal, Province)
  out[out$Country %in% keep,]
}

########################################
# Server Functions
########################################
# calculates doubling time over the last inWindow days.
doubTime <- function(cases, time, inWindow = 10){
  r <- projSimpleSlope(cases, time)[2]
  log(2)/r
}


# growth rate
growthRate <- function(cases, inWindow=10){
  nn <- length(cases)
  ss <- (nn - inWindow + 1):nn
  rate <- numeric(length(ss))
  rate[ss] <- (cases[ss] - cases[ss-1]) / cases[ss-1]
}


# calculates the curve flatenning index.
  # it is the second derivative of logA wrt t (the change in growth rate) divided by first differential (the current growth rate).
cfi <- function(active){
  cfiInd <- -diff(diff(active))/abs(diff(active)[-1])#*ifelse(diff(active)[-1]>0, -1, 1)
  cfiInd[abs(cfiInd)>10]<-NA # remove crazy values associated with changed test/diagnosis
  cfiInd
}

# estimates detection rate based on assumptions about cfr, ttd
detRate<-function(infd, deaths, cfr = 0.033, ttd=17, window=5){
  obs<-c(rep(NA, window), diff(infd, window)) # observed new cases
  deathDiff<-diff(deaths, window) # observed new deaths
  expd<-deathDiff/cfr #expected new cases given cfr
  expd<-expd[-(1:(ttd-window))]
  expd<-c(expd, rep(NA, ttd))
  detRate<-obs/expd
  detRate[detRate==0]<-NA
  detRate[is.infinite(detRate)]<-NA
  out<-mean(detRate, na.rm = TRUE)
  if (is.nan(out)) return(NA)
  if (out>1) out<-1
  out
}

# Simple projection based on growth over last inWindow days
  # returns extended plotting data
projSimple<-function(rawN, rawTime, inWindow=10, proj=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:proj)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "confidence")
  y <- exp(extFit)
  list(x=x, y=y)
}

# Simple projection based on growth over last inWindow days
# returns coefficients
projSimpleSlope<-function(rawN, rawTime, inWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:inWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  coefficients(mFit)
}



# To subset time series data and aggregate totals
tsSub <- function(x, subset){
  xSub<-x[subset, dateCols(x)]
  colSums(xSub)
}

#Fit SIR Model
fit.SIR <- function(data, N=37590000, R0, proj=40, Region.fit){
  start.time <- sum(data$yI>=0 & data$yI<=50) + 1
  if(Region.fit %in% c("US", "United States")){
    start.time <- max(start.time, which(data$dates == "2020-03-04"))
  }
  if(Region.fit == "Ontario"){
    start.time <- max(start.time, which(data$dates == "2020-03-17"))
  }
  if(Region.fit == "Massachusetts"){
    start.time <- max(start.time, which(data$dates == "2020-03-18"))
  }

  
  data.model <- data[1:nrow(data) >= start.time,]
  data.model$time <- start.time:(start.time+nrow(data.model)-1)
  
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
  
  nbpoints = ifelse(length(data.model$yA)<10,length(data.model$yA),10)
  const <- projSimpleSlope(data.model$yA, 1:nrow(data.model), inWindow=nbpoints)[2]
  if(Region.fit == "China"){
    const <- projSimpleSlope(data.model$yA[1:50], 1:50, inWindow=49)[2]
  }
  
  init <- c(S = N - data.model$yA[1], I = data.model$yA[1], R = 0)
  dates.needed <- c(data.model$dates,max(data.model$dates)+1:proj)
  fit <- ode(y = init, times = 1:length(dates.needed), func = SIR, parms = c(const, R0))
  fit <- cbind(data.frame(dates = dates.needed), fit)
  return(fit)
}

#Fit SEIR Model
fit.SEIR <- function(data, N=37590000, R0, proj=40, Region.fit){
  start.time <- sum(data$yI>=0 & data$yI<=50) + 1
  if(Region.fit %in% c("US", "United States")){
    start.time <- max(start.time, which(data$dates == "2020-03-04"))
  }
  if(Region.fit == "Ontario"){
    start.time <- max(start.time, which(data$dates == "2020-03-17"))
  }
  if(Region.fit == "Massachusetts"){
    start.time <- max(start.time, which(data$dates == "2020-03-18"))
  }
  
  
  data.model <- data[1:nrow(data) >= start.time,]
  data.model$time <- start.time:(start.time+nrow(data.model)-1)
  
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
  
  nbpoints = ifelse(length(data.model$yA)<10,length(data.model$yA),10)
  const <- projSimpleSlope(data.model$yA, 1:nrow(data.model), inWindow=nbpoints)[2]
  
  init <- c(S = N - data.model$yA[1], I = data.model$yA[1], R = 0)
  dates.needed <- c(data.model$dates,max(data.model$dates)+1:proj)
  fit <- ode(y = init, times = 1:length(dates.needed), func = SIR, parms = c(const, R0))
  fit <- cbind(data.frame(dates = dates.needed), fit)
  return(fit)
}




plot.SEIR <- function(){
  pop <- population[population$Country %in% input$countryFinder,"population"]
  yA <- tsSub(tsACanada,tsACanada$Country %in% input$countryFinder)
  yD <- tsSub(tsDCanada,tsDCanada$Country %in% input$countryFinder)
  yI <- tsSub(tsICanada,tsICanada$Country %in% input$countryFinder)
  yR <- tsSub(tsRCanada,tsRCanada$Country %in% input$countryFinder)
  data <- data.frame(dates,
                     yA,
                     yD,
                     yI,
                     yR)
  row.names(data) <- c()
  
  
  data.Actual <- data.frame(dates = dates,
                            yA = yA,
                            yD = yD,
                            yR = yR + yD)
  
  p <- plot_ly(data = data.Actual, x = ~dates, y = ~yA) %>%
    add_markers(name = 'Infected - Actual', legendgroup = "Infected - Actual") %>%
    add_markers(data = data.Actual, x = ~dates, y = ~yD, name = 'Deaths - Actual', legendgroup = "Deaths - Actual") %>%
    add_markers(data = data.Actual, x = ~dates, y = ~yR, name = 'Removed - Actual', legendgroup = "Removed - Actual") %>%
    add_lines(data = data.ExpModel, x = ~dates, y = ~yA, name = 'Exponential Model', legendgroup = "Exponential") %>%
    # add_lines(data = model.SIR, x = ~dates, y = ~I, name = 'SIR Model', legendgroup = "SIR") %>%
    # add_lines(data = data.ExpModelLbound, x = ~dates, y = ~yA, name = 'Exponential Model - Lower Bound', legendgroup = "Exponential", showlegend=TRUE) %>%
    # add_lines(data = data.ExpModelUbound, x = ~dates, y = ~yA, name = 'Exponential Model - Upper Bound', legendgroup = "Exponential", showlegend=TRUE) %>%
    layout(title = paste0(input$countryFinder,': Active Cases Over Time'),
           xaxis = list(title = "Dates"),
           yaxis = list(title = "Confirmed Active Cases"))
  maxy<-0
  for(R in c(1.3, 1.5, 1.7, 1.9, 2.1, 2.3)){
    model.SIR <- fit.SIR(data, N=pop, R0=R, proj=70, Region.fit=input$countryFinder)
    maxy <- max(maxy,model.SIR$I)
    p <- p %>% add_lines(data = model.SIR, x = ~dates, y = ~I, name = paste0("SIR - R0=",R), legendgroup = paste0("SIR - R0=",R))
  }
  p %>% layout(yaxis = list(range=c(0,maxy)))
}
