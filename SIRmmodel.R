

library(rvest)
library(lubridate)
library(ggplot2)
library(deSolve)
library(tidyverse)
library(plotly)
library(MetricsWeighted)
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
data <- data[yI>0,]
data$time <- 1:nrow(data)

fit.SIR <- function(data, N=37590000){
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta * I * S/N
      dI <- beta * I * S/N - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  
  LOSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    init <- c(S = N - data$yI[1], I = data$yI[1], R = 0)
    out <- ode(y = init, times = data$time, func = SIR, parms = parameters)
    sqrt(mean((data$yI - out[, 3])^2))
    
  }
  
  OPT <- optim(c(0.5, 0.5), LOSS, method = "L-BFGS-B", lower = c(0.01, 0.01), upper = c(1, 1))

  # RES <- sapply(seq(from=0, to=1, by=0.01),function(x){LOSS(c(x,0.1234568))})
  # seq(from=0, to=1, by=0.01)[which(RES == min(RES))]
  # RES <- sapply(seq(from=0.2, to=0.3, by=0.0001),function(x){LOSS(c(x,0.1234568))})
  # seq(from=0.2, to=0.3, by=0.0001)[which(RES == min(RES))]
  
  # check for convergence
  OPT$message
  
  Opt_par <- setNames(OPT$par, c("beta", "gamma"))
  Opt_par
  R0 <- Opt_par[1] / Opt_par[2]
  R0
  D <- 1 / Opt_par[2]
  D < 5.2 + 2.9
  
  Opt_par[1]<-0.3
  Opt_par[2]<-0.2
  
  
  init <- c(S = N - data$yI[1], I = data$yI[1], R = 0)
  fit <- ode(y = init, times = c(data$time, max(data$time)+1:10), func = SIR, parms = Opt_par)
  
  return(list(Opt_par = Opt_par,
              fit = fit))

}

SIRmodel = fit.SIR(data)
data.SIRmodel <- cbind(data.frame(dates = c(data$dates,max(data$dates)+1:10)),
                       SIRmodel$fit)

EXPmodel <- projSimple(yI, dates)
data.EXPmodel <- data.frame(dates = EXPmodel$x,
                            I = EXPmodel$y[,1])

p <- plot_ly(data = data.Actual, x = ~dates, y = ~yI) %>% add_markers(name = 'Actual') %>% 
  add_lines(data = data.EXPmodel, x = ~dates, y = ~I, name = 'Exponential Model') %>% 
  add_lines(data = data.SIRmodel, x = ~dates, y = ~I, name = 'SIR Model') %>%
  layout(title = paste0(input$countryFinder,': Active Cases Over Time'),
         xaxis = list(title = "Dates"),
         yaxis = list(title = "Confirmed Active Cases"))
p






dat <- data.frame(data.model[,c("date", "t", "confirmed.cumul")],
                  confirmed.cumul.fitted =ode(y = init, times = data.model$t, 
                                              func = SIR, parms = Opt_par))

ggplot(dat, aes(x = date)) + geom_line(aes(y = confirmed.cumul.fitted.I), colour = "red") + 
  geom_point(aes(y = confirmed.cumul), colour = "orange") + 
  labs(y = "Cumulative incidence", title = "COVID-19 fitted vs observed cumulative incidence, Canada", 
       subtitle = "(red=fitted incidence from SIR model, orange=observed incidence)")




library(tidyverse)
# time in days for predictions
t <- 1:80
# get the fitted values from our SIR model
data.fitted <- data.frame(ode(y = init, times = t, 
                                              func = SIR, parms = Opt_par))
data.fitted$Date <- ymd(sir_start_date) + days(t - 1)
data.fitted$cumulative_incident_cases = round(data.fitted$I)
data.model.adj <- data.model[,c("t", "confirmed.cumul")]
#data.model.adj$t <- as.integer(data.model.adj$t - (sir_start_date - data$date[1]))

sir_start_date - data$date[1]

data.fitted <- data.fitted %>% 
  left_join(., data.model.adj[,c("t", "confirmed.cumul")], by = c("time" = "t"))
            
# plot the data
p <- data.fitted %>% ggplot(aes(x = Date)) +
  geom_line(aes(y = I), colour = "red") +
  geom_line(aes(y = S), colour = "black") + 
  geom_line(aes(y = R), colour = "green") +
  geom_point(aes(y = confirmed.cumul), colour = "orange") +
  scale_y_continuous(labels = scales::comma) + 
  labs(y = "Persons", title = "COVID-19 fitted vs observed cumulative incidence, Canada") + 
  scale_colour_manual(name = "", values = c(red = "red", black = "black", green = "green", orange = "orange"),
                      labels = c("Susceptible", "Recovered", "Observed incidence", "Infectious"))

p
p + scale_y_log10()
R0


max(data.fitted$I) / N


