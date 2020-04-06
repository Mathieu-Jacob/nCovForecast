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
library(rmarkdown)

## ---------------------------

## load up our functions into memory
## source files
source("getDataNew.R")
source("functions.R")

## ---------------------------
## ---------------------------
options(scipen=9)

# Define UI
shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("Coronavirus Prediction"),
  navbarPage(p("As of", format(dates[length(dates)], "%d %b")),
             ##### 10-day forecast #####             
             tabPanel("Model forecast",
                      # Sidebar 
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Location"),
                          selectInput(inputId = "countryFinder",
                                      label = "Select Region:",
                                      choices = ddReg, 
                                      selected = ddNames[1]),
                          dateInput(inputId = "asofmodel",
                                    label = "Model As Of:",
                                    value = max(dates),
                                    min = "2020-03-05",
                                    max = max(dates)),
                          
                          titlePanel("Key Metrics:"),
                          tableOutput(outputId = "KeyMetrics"),
                          p("Active cases are total number of infections minus deaths and recoveries."),
                          
                          titlePanel("Detection"),
                          tableOutput(outputId = "detRate"),
                          p("Take this last number with a grain of salt; it is rough.  But low detection indicates that there are many more deaths in the country than there should be given reported case numbers (so there must be more cases than are reported)."),
                          
                          
                          titlePanel("Download Data"),
                          downloadButton("downloadData", "Download")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotlyOutput("rawPlot")
                          , plotlyOutput("logPlot")
                          , htmlOutput("EventPlanner")
                          # , h3("In Development:")
                          # , plotlyOutput("myPlot")
                        )
                      )
             ),
             tabPanel("Publicly Available Models",
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        titlePanel("IHME Model"),
                        p("The Institute for Health Metrics and Evaluation (IHME) is an independent population health research center at University of Washington, that provides rigorous and comparable measurement of the world's most important health problems and evaluates the strategies used to address them."),
                        p("Referenced in a White House press briefing as the \"Chris Murray Model\", IHME’s COVID-19 projections show demand for hospital services in each state. The demand for these services is expected to exceed capacity. "),
                        p(a("IHME Model Results", href = "https://covid19.healthdata.org/", target="_blank")),
                        p(a("IHME Model Details", href = "https://www.medrxiv.org/content/10.1101/2020.03.27.20043752v1.full.pdf", target="_blank"))
                      )
             ),
             ##### Growth Rate ##### 
             tabPanel("Growth rate",
                      # Sidebar
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Location selector"),
                          checkboxGroupInput(inputId = "countryGrowthRate",
                                             label = "Select Canada/Province:",
                                             choices = ddReg,
                                             selected = ddNames[1:3])
                        ),
                        mainPanel(
                          plotOutput("growthRate"),
                          p("This is the growth rate of the number of active cases for the last 10 days."),
                          p("Positive is bad, negative is good. Progress in control would be indicated by steady decline in growth rate over time, and holding in negative territory."),
                          p("Note, days with low or zero growth followed by large spikes are reporting issues: countries miss a day (or several) of reporting and then aggregate cases into the following day.")
                        )
                      )
             ),
             ##### CFI ##### 
             tabPanel("Curve-flattening index",
                      # Sidebar
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Location selector"),
                          checkboxGroupInput(inputId = "countryFinderCFI",
                                             label = "Select Canada/Province:",
                                             choices = ddReg, 
                                             selected = ddNames[1:3])
                        ),
                        mainPanel(
                          plotOutput("cfi"),
                          h5("This is a measure of how well a country is flattening the pandemic curve at any point in time.  Positive values are good, and China is an excellent reference series."),
                          h5("The index is sensitive to changes in screening/reporting.  
                      It's only as good as the data."),
                          h5(p("For more details see", 
                               a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank")))
                        )
                      )
             ),
             ##### CFI ##### 
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("About.Rmd")
                      )
             )
             
  )
))
