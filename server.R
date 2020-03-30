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
source("functions.R")

## ---------------------------
options(scipen=9)


# Define server logic 
shinyServer(function(input, output) {

  ##### Raw plot #####  
  output$rawPlot <- renderPlotly({
    pop <- population[population$Country %in% input$countryFinder,"population"]
    yA <- tsSub(data, "tsA", input$countryFinder)
    yD <- tsSub(data, "tsD", input$countryFinder)
    yI <- tsSub(data, "tsI", input$countryFinder)
    yR <- tsSub(data, "tsR", input$countryFinder)
    projection.period <- 70
    data <- data.frame(dates = c(dates,max(dates)+1:projection.period),
                       yA = c(yA,rep(NA,projection.period)),
                       yD = c(yD,rep(NA,projection.period)),
                       yI = c(yI,rep(NA,projection.period)),
                       yR = c(yR,rep(NA,projection.period)))
    row.names(data) <- c()
    data <- add.exponential(data, inWindow=10, proj=70)
    data <- add.SIR(data, N=pop, R0=1.3)
    data <- add.SIR(data, N=pop, R0=1.5)
    data <- add.SIR(data, N=pop, R0=1.7)
    data <- add.SIR(data, N=pop, R0=1.9)
    data <- add.SIR(data, N=pop, R0=2.1)
    data <- add.SIR(data, N=pop, R0=2.3)
    maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
    maxy <- 10^floor(log10(maxy)) * ceiling(maxy/10^floor(log10(maxy)))
    
    p <- plot_ly(data=data) %>% 
      add_markers(name = 'Actual - Active', x = ~dates, y = ~yA) %>%
      add_markers(name = 'Actual - Deaths', x = ~dates, y = ~yD) %>%
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
    
  })
  
  ##### Log plot #####    
  output$logPlot <- renderPlotly({
    pop <- population[population$Country %in% input$countryFinder,"population"]
    yA <- tsSub(data, "tsA", input$countryFinder)
    yD <- tsSub(data, "tsD", input$countryFinder)
    yI <- tsSub(data, "tsI", input$countryFinder)
    yR <- tsSub(data, "tsR", input$countryFinder)
    projection.period <- 70
    data <- data.frame(dates = c(dates,max(dates)+1:projection.period),
                       yA = c(yA,rep(NA,projection.period)),
                       yD = c(yD,rep(NA,projection.period)),
                       yI = c(yI,rep(NA,projection.period)),
                       yR = c(yR,rep(NA,projection.period)))
    row.names(data) <- c()
    data <- add.exponential(data, inWindow=10, proj=70)
    data <- add.SIR(data, N=pop, R0=1.3)
    data <- add.SIR(data, N=pop, R0=1.5)
    data <- add.SIR(data, N=pop, R0=1.7)
    data <- add.SIR(data, N=pop, R0=1.9)
    data <- add.SIR(data, N=pop, R0=2.1)
    data <- add.SIR(data, N=pop, R0=2.3)
    
    maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
    maxy <- 10^floor(log10(maxy)) * ceiling(maxy/10^floor(log10(maxy)))
    
    p <- plot_ly(data=data) %>% 
      add_markers(name = 'Actual - Active', x = ~dates, y = ~yA) %>%
      add_markers(name = 'Actual - Deaths', x = ~dates, y = ~yD) %>%
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

    maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
    maxy <- ceiling(log10(maxy))
    logPlot <- p %>% layout(yaxis = list(type = "log", range=c(0,maxy)))
    logPlot
  })
  
  # Downloadable csv of data ----
  datasetInput <- reactive({
    pop <- population[population$Country %in% input$countryFinder,"population"]
    yA <- tsSub(data, "tsA", input$countryFinder)
    yD <- tsSub(data, "tsD", input$countryFinder)
    yI <- tsSub(data, "tsI", input$countryFinder)
    yR <- tsSub(data, "tsR", input$countryFinder)
    projection.period <- 70
    data <- data.frame(dates = c(dates,max(dates)+1:projection.period),
                       yA = c(yA,rep(NA,projection.period)),
                       yD = c(yD,rep(NA,projection.period)),
                       yI = c(yI,rep(NA,projection.period)),
                       yR = c(yR,rep(NA,projection.period)))
    row.names(data) <- c()
    data <- add.exponential(data, inWindow=10, proj=70)
    data <- add.SIR(data, N=pop, R0=1.3)
    data <- add.SIR(data, N=pop, R0=1.5)
    data <- add.SIR(data, N=pop, R0=1.7)
    data <- add.SIR(data, N=pop, R0=1.9)
    data <- add.SIR(data, N=pop, R0=2.1)
    data <- add.SIR(data, N=pop, R0=2.3)
  })
  output$downloadData <- downloadHandler(
    filename = paste0("CovidPrediction_Data_",format(Sys.time(),"%Y%m%d"),".csv"),
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  output$population <- renderText({
    population <- population[population$Country %in% input$countryFinder,"population"]
    format(population, big.mark = ",")
  })
  
  
  ##### Raw stats ##### 
  output$rawStats <- renderTable({
    
    yA <- tsSub(data, "tsA", input$countryFinder)
    yD <- tsSub(data, "tsD", input$countryFinder)
    yI <- tsSub(data, "tsI", input$countryFinder)
    # yR <- tsSub(data, "tsR", input$countryFinder)
    nn <-length(yI)
    if (is.na(yA[nn])) nn <- nn-1
    out <- as.integer(c(yI[nn], yD[nn]))
    dim(out) <-c(1,2)
    colnames(out) <- c("Total", "Deaths")
    format(out, big.mark = ",")
  }, rownames = FALSE)
  
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
    # yA <- tsSub(data, "tsA", input$countryFinder)
    yD <- tsSub(data, "tsD", input$countryFinder)
    yI <- tsSub(data, "tsI", input$countryFinder)
    # yR <- tsSub(data, "tsR", input$countryFinder)
    dR<-round(detRate(yI, yD), 4)
    if (is.na(dR)) "Insufficient data for estimation" else dR
  })
  
  ##### Prediction table confirmed #####    
  output$tablePredConf <- renderTable({
    yA <- tsSub(data, "tsA", input$countryFinder)
    # yD <- tsSub(data, "tsD", input$countryFinder)
    # yI <- tsSub(data, "tsI", input$countryFinder)
    # yR <- tsSub(data, "tsR", input$countryFinder)
    lDat <- projSimple(yA, dates)
    nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Now", "In 10 days (min-max)")
    nowThen
  }, rownames = FALSE)
  
  ##### Prediction table true #####    
  output$tablePredTrue <- renderText({
    yA <- tsSub(data, "tsA", input$countryFinder)
    yD <- tsSub(data, "tsD", input$countryFinder)
    yI <- tsSub(data, "tsI", input$countryFinder)
    # yR <- tsSub(data, "tsR", input$countryFinder)
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
    pDat <- tsSub(data, "tsA", input$countryFinderCFI, sumcols=FALSE)
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
    pDat <- tsSub(data, "tsA", input$countryGrowthRate, sumcols=FALSE)
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
    pDat <- tsSub(data, "tsA", input$countryFinder, sumcols=TRUE)
    dTime <- round(doubTime(pDat, dates), 1)
  })
  
  ##### Doubling time plot #####    
  output$doubTimePlot <- renderPlot({
    pDat <- tsSub(data, "tsA", input$countryGrowthRate, sumcols=FALSE)
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