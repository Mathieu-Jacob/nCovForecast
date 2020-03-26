## ---------------------------
##
## Author: https://www.linkedin.com/in/mathieu-jacob/
## Adapted from: Ben Phillips - phillipsb@unimelb.edu.au
## Date Created: 2020-03-26
##
## ---------------------------
## load up the packages we will need 
library(shiny)
library(tidyverse)
library(plotly)
library(deSolve)
## ---------------------------

## source files
source("getDataNew.R")

## ---------------------------
options(scipen=9)


# Define server logic 
shinyServer(function(input, output) {
  ##### Raw stats #####  
  output$rawStats <- renderTable({
    yA <- tsSub(tsACanada,tsACanada$Country %in% input$countryFinder)
    yD <- tsSub(tsDCanada,tsDCanada$Country %in% input$countryFinder)
    yI <- tsSub(tsICanada,tsICanada$Country %in% input$countryFinder)
    #yR <- tsSub(tsRCanada,tsRCanada$Country %in% input$countryFinder)
    nn <-length(yI)
    if (is.na(yA[nn])) nn <- nn-1
    out <- as.integer(c(yI[nn], yD[nn]))
    dim(out) <-c(1,2)
    colnames(out) <- c("Total", "Deaths")
    format(out, big.mark = ",")
  }, rownames = FALSE)
  
  ##### Raw plot #####  
  output$rawPlot <- renderPlotly({
    population <- population[population$Country %in% input$countryFinder,"population"]
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
    
    model.EXP <- projSimple(yA, dates, proj=70)
    
    data.Actual <- data.frame(type ="Actual",
                              dates = dates,
                              yA = yA)
    data.ExpModel <- data.frame(type ="Exp.Model",
                                dates = model.EXP$x,
                                yA = model.EXP$y[,1])
    
    p <- plot_ly(data = data.Actual, x = ~dates, y = ~yA) %>%
      add_markers(name = 'Actual', legendgroup = "Actual") %>%
      add_lines(data = data.ExpModel, x = ~dates, y = ~yA, name = 'Exponential Model', legendgroup = "Exponential") %>%
      # add_lines(data = model.SIR, x = ~dates, y = ~I, name = 'SIR Model', legendgroup = "SIR") %>%
      # add_lines(data = data.ExpModelLbound, x = ~dates, y = ~yA, name = 'Exponential Model - Lower Bound', legendgroup = "Exponential", showlegend=TRUE) %>%
      # add_lines(data = data.ExpModelUbound, x = ~dates, y = ~yA, name = 'Exponential Model - Upper Bound', legendgroup = "Exponential", showlegend=TRUE) %>%
      layout(title = paste0(input$countryFinder,': Active Cases Over Time'),
             xaxis = list(title = "Dates"),
             yaxis = list(title = "Confirmed Active Cases"))
    maxy<-0
    for(R in c(1.3, 1.5, 1.7, 1.9, 2.1, 2.3)){
      model.SIR <- fit.SIR(data, N=population, R0=R, proj=70)
      maxy <- max(maxy,model.SIR$I)
      p <- p %>% add_lines(data = model.SIR, x = ~dates, y = ~I, name = paste0("SIR - R0=",R), legendgroup = paste0("SIR - R0=",R))
    }
    p %>% layout(yaxis = list(range=c(0,maxy)))
    
  })
  
  ##### Log plot #####    
  output$logPlot <- renderPlotly({
    population <- population[population$Country %in% input$countryFinder,"population"]
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
    
    model.EXP <- projSimple(yA, dates, proj=70)
    
    data.Actual <- data.frame(type ="Actual",
                              dates = dates,
                              yA = yA)
    data.ExpModel <- data.frame(type ="Exp.Model",
                                dates = model.EXP$x,
                                yA = model.EXP$y[,1])
    
    p <- plot_ly(data = data.Actual, x = ~dates, y = ~yA) %>%
      add_markers(name = 'Actual', legendgroup = "Actual") %>%
      add_lines(data = data.ExpModel, x = ~dates, y = ~yA, name = 'Exponential Model', legendgroup = "Exponential") %>%
      # add_lines(data = model.SIR, x = ~dates, y = ~I, name = 'SIR Model', legendgroup = "SIR") %>%
      # add_lines(data = data.ExpModelLbound, x = ~dates, y = ~yA, name = 'Exponential Model - Lower Bound', legendgroup = "Exponential", showlegend=TRUE) %>%
      # add_lines(data = data.ExpModelUbound, x = ~dates, y = ~yA, name = 'Exponential Model - Upper Bound', legendgroup = "Exponential", showlegend=TRUE) %>%
      layout(title = paste0(input$countryFinder,': Active Cases Over Time'),
             xaxis = list(title = "Dates"),
             yaxis = list(title = "Confirmed Active Cases"))
    
    for(R in c(1.3, 1.5, 1.7, 1.9, 2.1, 2.3)){
      model.SIR <- fit.SIR(data, N=population, R0=R, proj=70)
      p <- p %>% add_lines(data = model.SIR, x = ~dates, y = ~I, name = paste0("SIR - R0=",R), legendgroup = paste0("SIR - R0=",R))
    }
    
    logPlot <- p %>% layout(yaxis = list(type = "log"))
    logPlot
  })
  
  ##### Event PLanner #####
  output$EventPlanner <-
    renderText({
      #https://github.com/jsweitz/covid-19-event-risk-planner
      
      c(
        '<img src="',
        "https://2oqz471sa19h3vbwa53m33yj-wpengine.netdna-ssl.com/wp-content/uploads/2020/03/event-risk-assessment-chart-1.jpg",
        '">'
      )
    })
  
  
  ##### Detection rate #####    
  output$detRate <- renderText({
    yD <- tsSub(tsDCanada,tsDCanada$Country %in% input$countryFinder)
    yI <- tsSub(tsICanada,tsICanada$Country %in% input$countryFinder)
    dR<-round(detRate(yI, yD), 4)
    if (is.na(dR)) "Insufficient data for estimation" else dR
  })
  
  ##### Prediction table confirmed #####    
  output$tablePredConf <- renderTable({
    yA <- tsSub(tsACanada,tsACanada$Country %in% input$countryFinder)
    lDat <- projSimple(yA, dates)
    nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Now", "In 10 days (min-max)")
    nowThen
  }, rownames = FALSE)
  
  ##### Prediction table true #####    
  output$tablePredTrue <- renderText({
    yA <- tsSub(tsACanada,tsACanada$Country %in% input$countryFinder)
    yD <- tsSub(tsDCanada,tsDCanada$Country %in% input$countryFinder)
    yI <- tsSub(tsICanada,tsICanada$Country %in% input$countryFinder)
    dRate <- detRate(yI, yD)
    lDat <- projSimple(yA, dates)
    now <- tail(yA[!is.na(yA)], 1)
    nowTrue <- format(round(now/dRate, 0), big.mark = ",")
    #nowThenTrue <- c(round(nowThenTrue[1],0), paste(round(nowThenTrue[2],0), "-", round(nowThenTrue[3],0)))
    #dim(nowThenTrue) <- c(1, 2)
    #colnames(nowThenTrue)<-c("Now", "In 10 days (min-max)")
    nowTrue
  })
  
  ##### Curve-flattenning #####    
  output$cfi <- renderPlot({
    pDat <- subset(tsACanada, tsACanada$Country %in% input$countryFinderCFI)
    pMat<-as.matrix(log(pDat[,-1]))
    row.names(pMat)<-pDat$Country
    cfiDat<-apply(pMat, MARGIN = 1, FUN = "cfi")
    cfiDat[!is.finite(cfiDat)]<-0
    clrs<-hcl.colors(length(input$countryFinderCFI))
    dateSub<-3:length(dates) # date subset
    plot(cfiDat[,1]~dates[dateSub], 
         type = "n", 
         ylim = range(c(-1.2,1.2)*sd(cfiDat)),
         bty = "l",
         xlab = "Date",
         ylab = "Curve-flatenning index")
    abline(a = 0, b = 0, lty = 2, lwd = 2)
    for (cc in 1:ncol(cfiDat)){
      cfiSmooth<-loess(cfiDat[,cc]~as.numeric(dates[dateSub]))
      lines(cfiSmooth$fitted~dates[dateSub], col = clrs[cc], lwd=2)
    }
    legend("topleft", 
           legend = input$countryFinderCFI, 
           lty = 1, 
           col = clrs,
           bty = "n")
  })
  ##### Growth rate #####    
  output$growthRate <- renderPlot({
    pDat <- subset(tsACanada, tsACanada$Country %in% input$countryGrowthRate)
    gRate <- as.matrix(growthRate(pDat))
    clrs<-hcl.colors(length(input$countryGrowthRate))
    dates10 <- dates[(length(pDat)-10+1):length(pDat)]
    counts <- table(gRate)
    barplot(gRate,
            main="Growth rate",
            xlab="Date", 
            ylab="Growth rate",
            beside=TRUE,
            col = clrs,
            legend = input$countryGrowthRate,
            args.legend = list(bty = "n", x = "topleft"))
  })
  
  ##### Doubling time ##### 
  output$doubTime <- renderText({
    pDat <- tsSub(tsACanada, tsACanada$Country %in% input$countryFinder)
    dTime <- round(doubTime(pDat, dates), 1)
  })
  
  ##### Doubling time plot #####    
  output$doubTimePlot <- renderPlot({
    pDat <- subset(tsACanada, tsACanada$Country %in% input$countryGrowthRate)
    dTime <- as.matrix(doubTime(pDat))
    dTime[!is.finite(dTime)]<-NA
    clrs<-hcl.colors(length(input$countryGrowthRate))
    dates10 <- dates[(length(pDat)-10+1):length(pDat)]
    counts <- table(dTime)
    barplot(dTime,
            main="Doubling time",
            xlab="Date", 
            ylab="Doubling time (days)",
            beside=TRUE,
            col = clrs,
            legend = input$countryGrowthRate,
            args.legend = list(bty = "n", x = "topleft"))
  })
})