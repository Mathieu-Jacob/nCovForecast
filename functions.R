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

#Fill missing coluns between 2 data.fram
fill.missing <- function(data.missing, missing, fill){
  if(length(missing) > 0){
    data.to.add <- matrix(fill,nrow=nrow(data.missing), ncol=length(missing))
    colnames(data.to.add) <- missing
    data.new <- cbind(data.missing, data.to.add)
  }else{
    data.new <- data.missing
  }
  dCols<-dateCols(data.new)
  data.new <- cbind(data.new[,!dCols], data.new[,dCols][,order(names(data.new[,dCols]))])
  data.new
}

########################################
# Server Functions
########################################
# To subset time series data and aggregate totals
tsSub <- function(data, ts.type, Country.set, sumcols=TRUE){
  data.filtered <- data %>% filter(data.type == ts.type, Country %in% Country.set)

  if(sumcols){
    data.filtered <- data.filtered[,dateCols(data.filtered)]
    data.filtered <- colSums(data.filtered)
  }else{
    Country <- data.filtered$Country
    data.filtered <- cbind(Country,data.filtered[,dateCols(data.filtered)])
  }
  data.filtered
}


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





plot.actual <- function(data){
  p <- plot_ly(data = filter(data, type == "Actual")) %>% 
    layout(title = paste0(input$countryFinder,': Projection'),
           xaxis = list(title = "Dates"),
           yaxis = list(title = "Number of People")) %>% 
    add_markers(name = 'Actual - Active', x = ~dates, y = ~Infected) %>%
    add_markers(name = 'Actual - Deaths', x = ~dates, y = ~Deaths) %>%
    add_markers(name = 'Actual - Removed', x = ~dates, y = ~Removed)
  p
}


# Simple projection based on growth over last inWindow days
# returns extended plotting data
add.exponential <- function(data, inWindow=10, proj=10){
  max.actual <- max(which(!is.na(data$yA)))
  id.fit <- (max.actual - inWindow + 1):max.actual
  dates.fit <- data$dates[id.fit]
  lnActive <- log(data$yA[id.fit])
  lnActive[is.infinite(lnActive)] <- NA
  model.exp <- lm(lnActive~dates.fit)
  
  dates.predict <- c(dates.fit, max(dates.fit)+1:proj)
  yA.exp <- exp(predict(model.exp, newdata = list(dates.fit = dates.predict)))
  yA.exp <- data.frame(dates = dates.predict,
                       yA.exp = round(yA.exp))
  data <- left_join(data, yA.exp)
  data
}
exponential.slope <- function(data, inWindow=10){
  max.actual <- max(which(!is.na(data$yA)))
  id.fit <- (max.actual - inWindow + 1):max.actual
  dates.fit <- data$dates[id.fit]
  lnActive <- log(data$yA[id.fit])
  lnActive[is.infinite(lnActive)] <- NA
  model.exp <- lm(lnActive~dates.fit)
  coefficients(model.exp)[2]
}

add.SIR <- function(data, N=pop, R0=R, inWindow=10){
  Region.fit = input$countryFinder
  end.time <- max(which(!is.na(data$yA)))
  start.time <- data$dates[min(which(data$yA>=50))]
  start.time <- which(data$dates == start.time)
  if(Region.fit %in% c("US", "United States")){
    start.time <- max(start.time, which(data$dates == "2020-03-18"))
  }
  if(Region.fit %in% c("FL", "Florida")){
    start.time <- max(start.time, which(data$dates == "2020-03-15"))
  }
  if(Region.fit %in% c("MA", "Massachusetts")){
    start.time <- max(start.time, which(data$dates == "2020-03-16"))
  }
  if(Region.fit %in% c("NJ", "New Jersey")){
    start.time <- max(start.time, which(data$dates == "2020-03-18"))
  }
  if(Region.fit %in% c("NY", "New York")){
    start.time <- max(start.time, which(data$dates == "2020-03-20"))
  }
  if(Region.fit %in% c("US", "United States")){
    start.time <- max(start.time, which(data$dates == "2020-03-04"))
  }
  if(Region.fit == "Ontario"){
    start.time <- max(start.time, which(data$dates == "2020-03-17"))
  }
  if(Region.fit %in% c("MA", "Massachusetts")){
    start.time <- max(start.time, which(data$dates == "2020-03-18"))
  }

  id.fit <- start.time:end.time
  
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
  id.predict <- start.time:nrow(data)
  const <- exponential.slope(data, inWindow=nbpoints)


  
  dates.predict <- data$dates[start.time:nrow(data)]
  
  init <- c(S = N - data$yI[start.time] - data$yR[start.time] - data$yD[start.time], I = data$yA[start.time], R = data$yR[start.time])
  
  fit <- round(ode(y = init, times = id.predict, func = SIR, parms = c(const, R0)))[,-1]
  colnames(fit) <- paste0(c("yS.SIR.R", "yA.SIR.R", "yR.SIR.R"),R0)
  fit <- cbind(data.frame(dates=dates.predict), fit)
  data <- left_join(data, fit)
  data
}







#Fit SIR Model
fit.SIR <- function(data, N=37590000, R0, proj=40, Region.fit){
  start.time <- sum(data$yI>=0 & data$yI<=50) + 1
  if(Region.fit %in% c("US", "United States")){
    start.time <- max(start.time, which(data$dates == "2020-03-18"))
  }
  if(Region.fit %in% c("FL", "Florida")){
    start.time <- max(start.time, which(data$dates == "2020-03-15"))
  }
  if(Region.fit %in% c("MA", "Massachusetts")){
    start.time <- max(start.time, which(data$dates == "2020-03-16"))
  }
  if(Region.fit %in% c("NJ", "New Jersey")){
    start.time <- max(start.time, which(data$dates == "2020-03-18"))
  }
  if(Region.fit %in% c("NY", "New York")){
    start.time <- max(start.time, which(data$dates == "2020-03-20"))
  }
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
