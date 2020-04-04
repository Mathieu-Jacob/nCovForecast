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
# Server Functions - General
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


########################################
# Server Functions - Models
########################################



# Simple projection based on growth over last inWindow days
# returns extended plotting data
add.exponential <- function(data, inWindow=10, proj=10, input.asofmodel="2020-04-03", input.countryFinder = "Canada"){
  max.yA <- max(which(!is.na(data$yA)))
  max.fit <- min(max.yA,which(data$dates == input.asofmodel))
  print(dates[max.fit])
  
  id.fit <- (max.fit - inWindow + 1):max.fit
  dates.fit <- data$dates[id.fit]
  
  id.predict <- min(id.fit):(max.yA+proj)
  dates.predict <- data$dates[id.predict]
  
  lnActive <- log(data$yA[id.fit])
  lnActive[is.infinite(lnActive)] <- NA
  model.exp <- lm(lnActive~dates.fit)
  
  yA.exp <- exp(predict(model.exp, newdata = list(dates.fit = dates.predict)))
  yA.exp <- data.frame(dates = dates.predict,
                       yA.exp = round(yA.exp))
  data <- left_join(data, yA.exp)
  data
}
exponential.slope <- function(data, inWindow=10, input.asofmodel="2020-04-03", input.countryFinder = "Canada"){
  max.yA <- max(which(!is.na(data$yA)))
  max.fit <- min(max.yA,which(data$dates == input.asofmodel))
  
  id.fit <- (max.fit - inWindow + 1):max.fit
  dates.fit <- data$dates[id.fit]

  # id.predict <- min(id.fit):(max.yA+proj)
  # dates.predict <- data$dates[id.predict]
  
  lnActive <- log(data$yA[id.fit])
  lnActive[is.infinite(lnActive)] <- NA
  model.exp <- lm(lnActive~dates.fit)
  coefficients(model.exp)[2]
}

add.SIR <- function(data, N=pop, R0=R, inWindow=10, proj=10, input.asofmodel="2020-04-03", input.countryFinder = "Canada"){
  Region.fit <- input.countryFinder
  
  max.yA <- max(which(!is.na(data$yA)))
  max.fit <- min(max.yA,which(data$dates == input.asofmodel))
  
  min.time <- min(which(data$yA>=50))
  start.time <- max(min.time, max.fit-inWindow+1)
  
  # if(Region.fit %in% c("US", "United States")){
  #   start.time <- max(start.time, which(data$dates == "2020-03-18"))
  # }else if(Region.fit %in% c("FL", "Florida")){
  #   start.time <- max(start.time, which(data$dates == "2020-03-15"))
  # }else if(Region.fit %in% c("MA", "Massachusetts")){
  #   start.time <- max(start.time, which(data$dates == "2020-03-16"))
  # }else if(Region.fit %in% c("NJ", "New Jersey")){
  #   start.time <- max(start.time, which(data$dates == "2020-03-18"))
  # }else if(Region.fit %in% c("NY", "New York")){
  #   start.time <- max(start.time, which(data$dates == "2020-03-20"))
  # }else if(Region.fit %in% c("US", "United States")){
  #   start.time <- max(start.time, which(data$dates == "2020-03-04"))
  # }else if(Region.fit == "Ontario"){
  #   start.time <- max(start.time, which(data$dates == "2020-03-17"))
  # }else if(Region.fit %in% c("MA", "Massachusetts")){
  #   start.time <- max(start.time, which(data$dates == "2020-03-18"))
  # }
  
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
  const <- exponential.slope(data, inWindow=nbpoints, input.asofmodel=input.asofmodel, input.countryFinder = input.countryFinder)
  
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


# data <- data.frame(yA = c(40, 45, 43, 31, 20),
#                    Prov = c("A", "A", "B", "B", "B"))
# 
# data <- add.lag(data, target="yA", group="Prov", n=1)
# data <- add.lag(data, target="yA", group="Prov", n=2)
# 
# data <- add.ewma(data, target="yA", group="Prov", alpha=0.3)





add.ewma <- function(data, target, group, alpha) {
  data[,paste0(target,"_EWMA",alpha)] <- NA
  
  for(g in unique(data[,group])){
    EWMA <- NULL
    x <- data[data[,group] == g & !is.na(data[,target]),target]
    EWMA[1] <- x[1]
    for(i in 2:length(x)){
      EWMA[i] <-   alpha * x[i] + (1-alpha) * EWMA[i-1]
    }
    data[data[,group] == g & !is.na(data[,target]),paste0(target,"_EWMA",alpha)] <- EWMA
  }
  data
  
}
add.lag <- function(data, target, group, n=1){
  data[,paste0(target,"_L",n)] <- NA
  
  for(g in unique(data[,group])){
    data[data[,group] == g,paste0(target,"_L",n)] <- c(rep(NA,n),data[data[,group] == g,target])[1:length(data[data[,group] == g,target])]
  }
  data
}

add.GLM <- function(data, inWindow=10, proj=10){
  max.actual <- max(which(!is.na(data$yA)))
  min.actual <- min(which(data$yA > 80))
  
  id.fit <- min.actual:max.actual
  dates.fit <- data$dates[id.fit]
  
  add.features <- function(){
    data$group <- input$countryFinder
    data <- add.lag(data, target="yA", group="group", n=1)
    data <- add.lag(data, target="yA", group="group", n=2)
    data <- add.lag(data, target="yA", group="group", n=3)
    data <- add.lag(data, target="yA", group="group", n=4)
    data <- add.lag(data, target="yA", group="group", n=5)
    data <- add.lag(data, target="yA", group="group", n=6)
    data <- add.lag(data, target="yA", group="group", n=7)
    data <- add.lag(data, target="yA", group="group", n=8)
    data <- add.lag(data, target="yA", group="group", n=9)
    data <- add.lag(data, target="yA", group="group", n=10)
    # data$ya_meanL1_5 <- mean(data$yA_L1,data$yA_L2,data$yA_L3,data$yA_L4,data$yA_L5)
    # data$ya_meanL6_10 <- mean(data$yA_L6,data$yA_L7,data$yA_L8,data$yA_L9,data$yA_L10)
    # data$ya_meanL1_10 <- mean(data$yA_L1,data$yA_L2,data$yA_L3,data$yA_L4,data$yA_L5,data$yA_L6,data$yA_L7,data$yA_L8,data$yA_L9,data$yA_L10)
    # 
    for(alpha in (1:9)/10){
      data <- add.ewma(data, target="yA", group="group", alpha=alpha)
      for(L in 1:10){
        print(alpha)
        print(L)
        data <- add.ewma(data, target=paste0("yA_L",L), group="group", alpha=alpha)
      }
    }
    data
  }
  data <- add.features()
  
  data$lnyA <- log(data$yA)
  data$lnyA[is.infinite(data$lnyA)] <- NA
  
  features <- c("dates", names(data)[grepl("yA_",names(data))])
  formula <- as.formula(paste("lnyA", paste(features, collapse=" + "), sep=" ~ "))
  
  model.lm <- lm(lnyA ~ dates + yA_L1 + yA_L2, data=data[id.fit,])
  
  id.predict <- c(id.fit,max(id.fit)+1:proj)
  yA.bak <- data$yA
  for(t in id.predict){
    print(t)
    yA.lm <- exp(predict(model.lm, newdata = data[t,]))
    data$yA[t] <- yA.lm
    data <- add.features()
  }
  data$yA.lm <- data$yA
  data$yA <- yA.bak
  
  dates.predict <- c(dates.fit, max(dates.fit)+1:proj)
  yA.lm <- exp(predict(model.lm, newdata = data[c(id.fit,max(id.fit)+1:proj),]))
  yA.lm <- data.frame(dates = dates.predict,
                      yA.lm = round(yA.lm))
  data <- left_join(data, yA.lm)
  data
}


