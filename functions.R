## ---------------------------
##
## Script name: functions.R
##
## Purpose of script: Hold a bunch of functions for coronaRisk app
##
## Author: Ben Phillips
##
## Date Created: 2020-03-12
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 

## ---------------------------

## function definitions

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


# aggregates results to country
countryAgg<-function(x){
  xSelect<-x[, dCols]
  aggregate(xSelect, by = list(Country = x$Country.Region), FUN = sum)
}

# aggregates results to province for specific country
SelectCountry<-function(x, Country){
  Province <- cbind(Country = x$Province.State[x$Country.Region == Country],
                         x[x$Country.Region == Country, dCols])
  
  CountryTotal <- aggregate(x[x$Country.Region == Country, dCols], by = list(Country = x$Country.Region[x$Country.Region == Country]), FUN = sum)
  rbind(CountryTotal, Province)
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


# to identify the date columns in ts dataframes
dateCols<-function(x){
  grepl(pattern = "\\d", x = colnames(x))
}

# To subset time series data and aggregate totals
tsSub <- function(x, subset){
  xSub<-x[subset, dateCols(x)]
  colSums(xSub)
}

#Fit Sir Model
fit.SIR <- function(data, N=37590000, R0, proj=40){
  data.model <- data[data$yI>50,]
  data.model$time <- 1:nrow(data.model)
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
  
  const <- projSimpleSlope(data.model$yA, data.model$time)[2]
  
  
  init <- c(S = N - data.model$yA[1], I = data.model$yA[1], R = 0)
  dates.needed <- c(data.model$dates,max(data.model$dates)+1:proj)
  fit <- ode(y = init, times = 1:length(dates.needed), func = SIR, parms = c(const, R0))
  fit <- cbind(data.frame(dates = c(data.model$dates,max(data.model$dates)+1:proj)), fit)
  return(fit)
}


