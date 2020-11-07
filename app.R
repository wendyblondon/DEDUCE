library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)

source("target_crm.R")
source("three_plus_three.R")

ui <- navbarPage("DELPHI",
                 tabPanel("Home"),
                 tabPanel("Design",
                          fluidRow(
                            column(4,
                                   checkboxGroupInput("designSelector", "Dose-Escalation Designs", choices = c("3+3", "TARGET-CRM"), selected = "3+3"),
                                   uiOutput("designUI"),
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

  # Rendering the UI
  output$designUI <- renderUI({
    
    if (input$designSelector == "3+3") {
      tagList(
        textInput("designDoseLabels", "Dose Level Labels", value = "-1,1,2,3"),
        sliderInput("designTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("designNumTrials", "Number of Simulated Trials", min = 0, max = 10000, value = 100),
        textInput("designTrueTox", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        sliderInput("designArrivalRate", "Patient Enrollment Rate", min = 0, max = 180, value = 15),
        sliderInput("designPropB", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("designCycleLength", "Duration of DLT Observation Period", min = 0, max = 365, value = 28),
        selectInput("designStartLevel", "Starting Dose Level", choices = NULL)
      )
    }
    
    else if(input$designSelector == "TARGET-CRM"){
      tagList(
        textInput("designDoseLabels2", "Dose Level Labels", value = "-1,1,2,3"),
        textInput("designPriorTox", "Prior Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        sliderInput("designTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("designNumTrials", "Number of Simulated Trials", min = 0, max = 10000, value = 100),
        textInput("designTrueTox", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        sliderInput("designArrivalRate", "Patient Enrollment Rate", min = 0, max = 180, value = 15),
        sliderInput("designPropB", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
        selectInput("designTargetCRM", "Target-CRM Option", choices = c(0,1,2), selected = 1),
        sliderInput("designMinCohortB", "Minimum Enrollment of Cohort B Patients (Optional)", min = 0, max = 100, value = 2),
        sliderInput("designCycleLength", "Duration of DLT Observation Period", min = 0, max = 365, value = 28),
        selectInput("designCohortSize", "Cohort Size", choices = c(seq(1,9)), selected = 3),
        sliderInput("designMaxN", "Maximum Sample Size", min = 1, max = 200, value = 18),
        selectInput("designStartLevel", "Starting Dose Level", choices = NULL)
      )
    }
  })
  
  
  
  # Get Dose Level Labels
  designDoseLabels <- reactive({
    as.vector(unlist(strsplit(input$designDoseLabels, ",")))
  })
  
  
  observe({
    updateSelectInput(session, "designStartLevel", choices = designDoseLabels(), selected = as.numeric(designDoseLabels()[2]))
    updateSliderInput(session, "designMinCohortB", max = input$designMaxN)
  })
  
  
  # Running the Design(s)
  designTargetCRM <- eventReactive(input$designSimulate, {
    if (input$designSelector == 'TARGET-CRM') {
      target.crm(prior = as.numeric(unlist(strsplit(input$designPriorTox, ","))), target.tox = input$designTargetTox, number.trials = input$designNumTrials, 
                 true.tox = as.numeric(unlist(strsplit(input$designTrueTox, ","))), arrival.rate = input$designArrivalRate, prop.B = input$designPropB, 
                 target.crm = as.numeric(input$designTargetCRM), min.cohortB = input$designMinCohortB, cycle.length = input$designCycleLength, 
                 cohort.size = input$designCohortSize, max.N = input$designMaxN, start.level = as.numeric(input$designStartLevel))
    }
    if (input$designSelector == '3+3') {
      three.plus.three(target.tox = input$designTargetTox, number.trials = input$designNumTrials, 
                       true.tox = as.numeric(unlist(strsplit(input$designTrueTox, ","))), arrival.rate = input$designArrivalRate, 
                       input$designPropB, cycle.length = input$designCycleLength, start.level = as.numeric(input$designStartLevel))
    }
  })
  
  # Displaying the Plots
  # output$designPlotly1 <- renderPlotly({
  #   p1 <- designTargetCRM()$df %>% mutate(MTD.Selection = MTD.Selection/100) %>%
  #     ggplot(aes(x=Dose.Number, y=MTD.Selection)) + geom_bar(stat = 'identity')
  #   
  #   ggplotly(p1) %>% config(displayModeBar = FALSE)
  #})
  
}

shinyApp(ui, server)
