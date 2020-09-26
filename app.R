library(shiny)
library(shinydashboard)
library(shinycssloaders)

source("target_crm.R")

ui <- navbarPage("DELPHI",
                 tabPanel("Home"),
                 tabPanel("Design",
                          tabBox(
                            tabPanel("Inputs",
                              div(style = 'overflow-y:scroll;height:500px;',
                                  selectizeInput("designPriorTox", "Prior Toxicity Probabilities", choices = NULL, multiple = TRUE, options = list(create = TRUE)),
                                  sliderInput("designTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
                                  sliderInput("designNumTrials", "Number of Trials", min = 0, max = 50000, value = 1000),
                                  selectizeInput("designTrueTox", "True Toxicity Probabilities", choices = NULL, multiple = TRUE, options = list(create = TRUE)),
                                  sliderInput("designStartLevel", "Starting Dose Level", min = 1, max = 10, value = 2),
                                  sliderInput("designArrivalRate", "Mean Inter-Arrival Time", min = 1, max = 100, value = 15),
                                  sliderInput("designStartLevel", "Starting Dose Level", min = 1, max = 10, value = 2),
                                  sliderInput("designPropB", "Proportion of Patients in Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
                                  sliderInput("designCycleLength", "Duration of DLT Observation", min = 1, max = 100, value = 28),
                                  radioButtons("designTargetCRM", "Target CRM Option", choices = c(0,1,2)),
                                  sliderInput("designMinCohortB", "Minimum Cohort B Patients Option", min = 0, max = 100, value = 2),
                                  sliderInput("designCohortSize", "Patients to Treat at Current Dose", min = 1, max = 100, value = 3),
                                  sliderInput("designMaxN", "Maximum Enrolled Patients", min = 1, max = 100, value = 20),
                                  checkboxGroupInput("designSelector", "Select Designs", choices = c("TARGET-CRM", "3+3")),
                                  actionButton("designSimulate", "Simulate")
                              )
                            ),
                            tabPanel("Results",
                                     h1("Plot Output Place Holder")
                            )
                          )
                  ),
                 tabPanel("Conduct"),
                 tabPanel("Help")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
