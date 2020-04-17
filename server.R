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
shinyServer <- function(input, output) {
  
  ##### Data #####  
  GetModels <- reactive({
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
    if (min.data > min.data.IHME){
      cols.to.remove <- as.Date(min.data) - as.Date(min.data.IHME)
      yD.IHME <- yD.IHME[-(1:cols.to.remove)]
      yI.IHME <- yI.IHME[-(1:cols.to.remove)]
    }else{
      cols.to.add <- as.Date(min.data.IHME) - as.Date(min.data)
      yD.IHME <- c(rep(NA,cols.to.add), yD.IHME)
      yI.IHME <- c(rep(NA,cols.to.add), yI.IHME)
    }

    max.cols <- length(yA) + 70
    yD.IHME <- yD.IHME[1:(min(length(yD.IHME),max.cols))]
    yI.IHME <- yI.IHME[1:(min(length(yI.IHME),max.cols))]
    
    
    
    
    
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
    data <- add.SIR(data, N=pop, R0=1.15, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
    data <- add.SIR(data, N=pop, R0=1.2, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
    data <- add.SIR(data, N=pop, R0=1.3, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
    data <- add.SIR(data, N=pop, R0=1.5, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
    data <- add.SIR(data, N=pop, R0=1.7, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
    data <- add.SIR(data, N=pop, R0=1.9, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
    data <- add.SIR(data, N=pop, R0=2.1, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)
    data <- add.SIR(data, N=pop, R0=2.3, proj=70, input.asofmodel = input.asofmodel, input.countryFinder = input.countryFinder)

    data
  })
  
  ##### Raw plot #####  
  output$rawPlot <- renderPlotly({
    data <- GetModels()
    
    maxy <- max(data$yA.SIR.R1.3[!is.na(data$yA.SIR.R1.3)])
    maxy <- 10^floor(log10(maxy)) * ceiling(maxy/10^floor(log10(maxy)))
    
    p <- plot_ly(data=data) %>% 
      add_markers(name = 'Actual - Active', x = ~dates, y = ~yA) %>%
      add_markers(name = 'Actual - Deaths', x = ~dates, y = ~yD) %>%
      add_lines(name = 'Exponential - Active', x = ~dates, y = ~yA.exp) %>% 
      add_lines(name = 'SIR R0=1.05 - Active', x = ~dates, y = ~yA.SIR.R1.05) %>% 
      add_lines(name = 'SIR R0=1.1 - Active', x = ~dates, y = ~yA.SIR.R1.1) %>% 
      add_lines(name = 'SIR R0=1.15 - Active', x = ~dates, y = ~yA.SIR.R1.15) %>% 
      add_lines(name = 'SIR R0=1.2 - Active', x = ~dates, y = ~yA.SIR.R1.2) %>% 
      add_lines(name = 'SIR R0=1.3 - Active', x = ~dates, y = ~yA.SIR.R1.3) %>% 
      # add_lines(name = 'SIR R0=1.5 - Active', x = ~dates, y = ~yA.SIR.R1.5) %>% 
      # add_lines(name = 'SIR R0=1.7 - Active', x = ~dates, y = ~yA.SIR.R1.7) %>% 
      # add_lines(name = 'SIR R0=1.9 - Active', x = ~dates, y = ~yA.SIR.R1.9) %>% 
      # add_lines(name = 'SIR R0=2.1 - Active', x = ~dates, y = ~yA.SIR.R2.1) %>% 
      # add_lines(name = 'SIR R0=2.3 - Active', x = ~dates, y = ~yA.SIR.R2.3) %>% 
      add_lines(name = 'IHME - Cumul Cases', x = ~dates, y = ~yI.IHME) %>% 
      add_lines(name = 'IHME - Deaths', x = ~dates, y = ~yD.IHME) %>% 
      layout(title = paste0(input$countryFinder,': Projection Over Time'),
             xaxis = list(title = "Dates"),
             yaxis = list(title = "Number of People", range=c(0,maxy)) )
    p
    
  })
  
  ##### Log plot #####    
  output$logPlot <- renderPlotly({
    data <- GetModels()
    
    maxy <- max(data$yA.SIR.R1.3[!is.na(data$yA.SIR.R1.3)])
    maxy <- 10^floor(log10(maxy)) * ceiling(maxy/10^floor(log10(maxy)))
    
    p <- plot_ly(data=data) %>% 
      add_markers(name = 'Actual - Active', x = ~dates, y = ~yA) %>%
      add_markers(name = 'Actual - Deaths', x = ~dates, y = ~yD) %>%
      add_lines(name = 'Exponential - Active', x = ~dates, y = ~yA.exp) %>% 
      add_lines(name = 'SIR R0=1.05 - Active', x = ~dates, y = ~yA.SIR.R1.05) %>% 
      add_lines(name = 'SIR R0=1.1 - Active', x = ~dates, y = ~yA.SIR.R1.1) %>% 
      add_lines(name = 'SIR R0=1.15 - Active', x = ~dates, y = ~yA.SIR.R1.15) %>% 
      add_lines(name = 'SIR R0=1.2 - Active', x = ~dates, y = ~yA.SIR.R1.2) %>% 
      add_lines(name = 'SIR R0=1.3 - Active', x = ~dates, y = ~yA.SIR.R1.3) %>% 
      # add_lines(name = 'SIR R0=1.5 - Active', x = ~dates, y = ~yA.SIR.R1.5) %>% 
      # add_lines(name = 'SIR R0=1.7 - Active', x = ~dates, y = ~yA.SIR.R1.7) %>% 
      # add_lines(name = 'SIR R0=1.9 - Active', x = ~dates, y = ~yA.SIR.R1.9) %>% 
      # add_lines(name = 'SIR R0=2.1 - Active', x = ~dates, y = ~yA.SIR.R2.1) %>% 
      # add_lines(name = 'SIR R0=2.3 - Active', x = ~dates, y = ~yA.SIR.R2.3) %>% 
      add_lines(name = 'IHME - Cumul Cases', x = ~dates, y = ~yI.IHME) %>% 
      add_lines(name = 'IHME - Deaths', x = ~dates, y = ~yD.IHME) %>% 
      layout(title = paste0(input$countryFinder,': Projection Over Time'),
             xaxis = list(title = "Dates"),
             yaxis = list(title = "Number of People", range=c(0,maxy)) )
    p
    
    maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
    maxy <- ceiling(log10(maxy))
    logPlot <- p %>% layout(yaxis = list(type = "log", range=c(0,maxy)))
    logPlot
  })
  

  output$myPlot <- renderPlotly({
    data <- GetModels()
    
    maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
    maxy <- 10^floor(log10(maxy)) * ceiling(maxy/10^floor(log10(maxy)))

    p <- plot_ly(data=data) %>% 
      add_markers(name = 'Actual - Cases', x = ~dates, y = ~yI) %>%
      add_markers(name = 'Actual - Deaths', x = ~dates, y = ~yD) %>%
      
      add_lines(name = 'Exponential - Deaths', x = ~dates, y = ~yD.exp) %>% 
      add_lines(name = 'IHME - Deaths', x = ~dates, y = ~yD.IHME) %>% 
      # add_lines(name = 'SIR R0=1.05 - Deaths', x = ~dates, y = ~yR.SIR.R1.05) %>% 
      # add_lines(name = 'SIR R0=1.1 - Deaths', x = ~dates, y = ~yR.SIR.R1.1) %>% 
      # add_lines(name = 'SIR R0=1.2 - Deaths', x = ~dates, y = ~yR.SIR.R1.2) %>% 
      
      add_lines(name = 'Exponential - Cases', x = ~dates, y = ~yI.exp) %>% 
      add_lines(name = 'IHME - Cases', x = ~dates, y = ~yI.IHME) %>% 
      add_lines(name = 'SIR R0=1.05 - Cases', x = ~dates, y = ~yI.SIR.R1.05) %>% 
      add_lines(name = 'SIR R0=1.1 - Cases', x = ~dates, y = ~yI.SIR.R1.1) %>% 
      add_lines(name = 'SIR R0=1.2 - Cases', x = ~dates, y = ~yI.SIR.R1.2) %>% 

      layout(title = paste0(input$countryFinder,': Cumulative Cases (Not removing deaths & recovered)'),
             xaxis = list(title = "Dates"),
             yaxis = list(title = "Number of People", range=c(0,maxy)) )
    p
    
    maxy <- max(data$yA.SIR.R2.3[!is.na(data$yA.SIR.R2.3)])
    maxy <- ceiling(log10(maxy))
    logPlot <- p %>% layout(yaxis = list(type = "log", range=c(0,maxy)))
    logPlot
  })
  
  
  ##### DownloadTable ##### 
  output$downloadData <- downloadHandler(
    filename = paste0("CovidPrediction_Data_",format(Sys.time(),"%Y%m%d"),".csv"),
    content = function(file) {
      write.csv(GetModels(), file, row.names = FALSE)
    })
  
  
  
  ##### Statistics ##### 
  output$KeyMetrics <- renderTable({
    data <- GetModels()
    pop <- population[population$Country %in% input$countryFinder,"population"]
    slope <- round(exponential.slope(data, inWindow=10, input.asofmodel=input$asofmodel, input.countryFinder = input$countryFinder),4)
    
    Latest.A <- data$yA %>% .[!is.na(.)] %>% .[length(.)]
    Latest.D <- data$yD %>% .[!is.na(.)] %>% .[length(.)]
    Latest.I <- data$yI %>% .[!is.na(.)] %>% .[length(.)]
    Latest.R <- data$yR %>% .[!is.na(.)] %>% .[length(.)]
    Latest.T <- data$yT %>% .[!is.na(.)] %>% .[length(.)]
    Latest.P <- data$yP %>% .[!is.na(.)] %>% .[length(.)]
    Latest.H <- data$yH %>% .[!is.na(.)] %>% .[length(.)]
    if (input$countryFinder == "United States" | nchar(input$countryFinder) == 2){
      Latest.T <- format(Latest.T, big.mark=",")
      Latest.P <- format(Latest.P, big.mark=",")
      Latest.H <- format(Latest.H, big.mark=",")
      Latest.R <- format(Latest.R, big.mark=",")
    }else{
      Latest.R <- NA
      Latest.T <- NA
      Latest.P <- NA
      Latest.H <- NA
    }
    
    pDat <- data[!is.na(data$yA),c("dates","yA")]
    dTime <- round(doubTime(pDat$yA, pDat$dates), 1)
    
    out <- c(format(pop, big.mark=","),
             format(Latest.I, big.mark=","),
             format(Latest.A, big.mark=","),
             format(Latest.D, big.mark=","),
             Latest.R,
             Latest.H,
             Latest.T,
             Latest.P,
             slope,
             dTime)
    dim(out) <-c(length(out), 1)
    rownames(out) <- c("Population", "Infected", "Active", "Deaths", "Recovered", "Hospitalized", "Testings", "Pending Tests", "Exponential Growth Slope", "Doubling Time")
    out
  }, rownames = TRUE, colnames = FALSE)
  
  
  ##### Event PLanner #####
  output$EventPlanner <- renderText({
    #https://github.com/jsweitz/covid-19-event-risk-planner
    c('<img src="', "https://2oqz471sa19h3vbwa53m33yj-wpengine.netdna-ssl.com/wp-content/uploads/2020/03/event-risk-assessment-chart-1.jpg", '">')
  })
  
  
  ##### Detection rate #####   
  output$detRate <- renderTable({
    data <- GetModels()

    yA <- data$yA %>% .[!is.na(.)]
    yD <- data$yD %>% .[!is.na(.)]
    yI <- data$yI %>% .[!is.na(.)]
    
    dR<-round(detRate(yI, yD), 4)
    if (is.na(dR)) "Insufficient data for estimation" else dR
    
    Latest.I <- data$yI %>% .[!is.na(.)] %>% .[length(.)]
    Latest.I.True <- format(round(Latest.I/dR, 0), big.mark = ",")
   
    
    out <- c(dR, Latest.I.True)
    dim(out) <-c(length(out), 1)
    rownames(out) <- c("Estimated proportion of cases detected:",
                       "Possible true number of cases now given imperfect detection:")
    out
    

    
  }, rownames = TRUE, colnames = FALSE)
  
  
  
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
  
  
}











