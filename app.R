library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)

source("target_crm.R")

ui <- navbarPage("DELPHI",
                 tabPanel("Home"),
                 tabPanel("Design",
                          fluidRow(
                            column(4,
                                   selectizeInput("designPriorTox", "Prior Toxicity Probabilities", choices = NULL, multiple = TRUE, options = list(create = TRUE)),
                                   sliderInput("designTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
                                   sliderInput("designNumTrials", "Number of Trials", min = 0, max = 50000, value = 1000),
                                   selectizeInput("designTrueTox", "True Toxicity Probabilities", choices = NULL, multiple = TRUE, options = list(create = TRUE)),
                                   sliderInput("designArrivalRate", "Mean Inter-Arrival Time", min = 1, max = 100, value = 15),
                                   sliderInput("designPropB", "Proportion of Patients in Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
                                   radioButtons("designTargetCRM", "Target CRM Option", choices = c(0,1,2)),
                                   sliderInput("designMinCohortB", "Minimum Cohort B Patients Option", min = 0, max = 100, value = 2),
                                   sliderInput("designCycleLength", "Duration of DLT Observation", min = 1, max = 100, value = 28),
                                   sliderInput("designCohortSize", "Patients to Treat at Current Dose", min = 1, max = 100, value = 3),
                                   sliderInput("designMaxN", "Maximum Enrolled Patients", min = 1, max = 100, value = 20),
                                   sliderInput("designStartLevel", "Starting Dose Level", min = 1, max = 10, value = 2),
                                   checkboxGroupInput("designSelector", "Select Designs", choices = c("TARGET-CRM", "3+3")),
                                   actionButton("designSimulate", "Simulate")
                            ),
                            column(8,
                                   withSpinner(plotlyOutput("designPlotly1"), type = 7, color = "#003087", size = 2)
                            )
                          )
                  ),
                 tabPanel("Conduct"),
                 tabPanel("Help")
)

server <- function(input, output, session) {
  
  designTargetCRM <- eventReactive(input$designSimulate, {
    if (input$designSelector == 'TARGET-CRM') {
      target.crm(prior = as.numeric(input$designPriorTox), target.tox = input$designTargetTox, number.trials = input$designNumTrials, 
                 true.tox = as.numeric(input$designTrueTox), arrival.rate = input$designArrivalRate, prop.B = input$designPropB, 
                 target.crm = as.numeric(input$designTargetCRM), min.cohortB = input$designMinCohortB, cycle.length = input$designCycleLength, 
                 cohort.size = input$designCohortSize, max.N = input$designMaxN, start.level = input$designStartLevel)
    }
    else{
      #three.plus.three()
    }
  })
  
  output$designPlotly1 <- renderPlotly({
    p1 <- designTargetCRM()$df %>% ggplot(aes(x=Dose.Number, y=MTD.Selection)) + geom_bar(stat = 'identity')
    
    ggplotly(p1) %>% config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server)
