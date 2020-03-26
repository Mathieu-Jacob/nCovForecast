

library(deSolve)

###################################################
## 1 - Import Data
###################################################

## source files
source("getDataNew.R")

## ---------------------------
options(scipen=9)


###################################################
## 2 - Import Data
###################################################
input$countryFinder = "Canada"
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

# https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/#estimating-changes-in-the-effective-reproduction-number
# https://stats.stackexchange.com/questions/446712/fitting-sir-model-with-2019-ncov-data-doesnt-conververge

N <- 37590000
# N <- 8485000
data <- data.frame(dates,
                   yA,
                   yD,
                   yI,
                   yR)
row.names(data) <- c()
data <- data[yI>50,]
data$time <- 1:nrow(data)

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
  fit <- ode(y = init, times = c(data$time, max(data$time)+1:proj), func = SIR, parms = c(const, R0))
  fit <- cbind(data.frame(dates = c(data$dates,max(data$dates)+1:proj)), fit)
  return(fit)
}

SIRmodel = fit.SIR(data, R0= 2.2)



data.SIRmodel <- cbind(data.frame(dates = c(data$dates,max(data$dates)+1:40)),
                       SIRmodel)

EXPmodel <- projSimple(yA, dates, inWindow=40)
data.EXPmodel <- data.frame(dates = EXPmodel$x,
                            I = EXPmodel$y[,1])
data.Actual <- data.frame(type ="Actual",
                          dates = dates,
                          yA = yA)

p <- plot_ly(data = data.Actual, x = ~dates, y = ~yI) %>% add_markers(name = 'Actual') %>% 
  add_lines(data = data.EXPmodel, x = ~dates, y = ~I, name = 'Exponential Model') %>% 
  add_lines(data = data.SIRmodel, x = ~dates, y = ~I, name = 'SIR Model') %>%
  layout(title = paste0(input$countryFinder,': Active Cases Over Time'),
         xaxis = list(title = "Dates"),
         yaxis = list(title = "Confirmed Active Cases"))
p



