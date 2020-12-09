library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)

source("target_crm.R")
source("three_plus_three.R")

# Functions
numerizer <- function(x){
  as.numeric(unlist(strsplit(x, ",")))
}

ui <- navbarPage("DELPHI",
                 tabPanel("Home"),
                 tabPanel("Design",
                          fluidRow(
                            column(3, style="overflow-y:scroll; max-height: 500px;",
                                   h3("Dose Escalation Designs:"),
                                   prettyCheckbox("DTSelectorTPT", "3+3", value = TRUE, status = "success", shape = "round", fill = TRUE, inline = TRUE),
                                   prettyCheckbox("DTSelectorTCRM", "TARGET-CRM", value = FALSE, status = "success", shape = "round", fill = TRUE, inline = TRUE),
                                   prettyCheckbox("DTSelectorOther", "Other", value = FALSE, status = "success", shape = "round", fill = TRUE, inline = TRUE),
                                   textInput("DTDoseLabels", "Dose Level Labels", value = "-1,1,2,3"),
                                   bsTooltip("DTDoseLabels", "Please enter the dose level labels (seperated by commas) for each dose level evaluated in the trial", 
                                             "top", options = list(container = "body")),
                                   selectInput("DTStartLevel", "Starting Dose Level", choices = c(-1,1,2,3), selected = 1),
                                   bsTooltip("DTStartLevel", "Please enter the starting dose level using the dose level labels above", 
                                             "top", options = list(container = "body")),
                                   uiOutput("DTInputs"),
                                   actionButton("DTSimulate", "Simulate")
                            ),
                            column(9,
                                   uiOutput("DTPlotsUI"),
                                   uiOutput("DTNoneUI")
                            )
                          )
                 ),
                 tabPanel("Conduct"),
                 tabPanel("Help")
)

server <- function(input, output, session) {
  
  # Get Selected Designs for Rendering UI Guidance
  DTSelectedDesigns <- reactive({
    
    # Only 3+3 Selected
    if(input$DTSelectorTPT == 1 & input$DTSelectorTCRM == 0 & input$DTSelectorOther == 0){
      return("some")
    }
    
    # Nothing Selected
    else if(input$DTSelectorTPT == 0 & input$DTSelectorTCRM == 0 & input$DTSelectorOther == 0){
      return(NULL)
    }
    
    else{
      return("all")
    }
  })
  
  # Get Length of Selected Designs for Plotting Guidance
  DTSelectedDesignsLength <- reactive({
    return(sum(c(input$DTSelectorTPT, input$DTSelectorTCRM, input$DTSelectorOther)))
  })
  
  # Rendering UI Select Input Based on Dose Labels
  output$DTInputs <- renderUI({
    
    req(!is.null(DTSelectedDesigns()))
    
    # 3+3
    if (DTSelectedDesigns() == "some") {
      tagList(
        sliderInput("DTTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
        bsTooltip("DTTargetTox", "Please enter the target toxicity probability of the study agent", 
                  "top", options = list(container = "body")),
        sliderInput("DTNumTrials", "Number of Simulated Trials", min = 0, max = 10000, value = 100),
        bsTooltip("DTNumTrials", "Please enter the number of simulated trials. A larger number of simulations increases the precision of simulation results and computation time", 
                  "top", options = list(container = "body")),
        textInput("DTTrueTox", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        bsTooltip("DTTrueTox", "Please enter the true toxicity probabilities for each dose level evaluated in the trial. Toxicity probabilities must increase with each subsequent dose level", 
                  "top", options = list(container = "body")),
        sliderInput("DTArrivalRate", "Patient Enrollment Rate", min = 0, max = 180, value = 15),
        bsTooltip("DTArrivalRate", "Please enter the average time between enrolling patients (in days)", 
                  "top", options = list(container = "body")),
        sliderInput("DTPropB", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
        bsTooltip("DTPropB", "Please enter the proportion of enrolled patients belonging to the “enrichment” Cohort B", 
                  "top", options = list(container = "body")),
        sliderInput("DTCycleLength", "Duration of DLT Observation Period", min = 0, max = 365, value = 28),
        bsTooltip("DTCycleLength", "Please enter the duration of the DLT observation period (in days)", 
                  "top", options = list(container = "body"))
        
      )
    }
    # TARGET-CRM or Both
    else if (DTSelectedDesigns() == "all") {
      tagList(
        textInput("DTPriorTox", "Prior Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        bsTooltip("DTPriorTox", "Please enter the prior toxicity probabilities for each dose level evaluated in the trial. Toxicity probabilities must increase with each subsequent dose level", 
                  "top", options = list(container = "body")),
        sliderInput("DTTargetTox2", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
        bsTooltip("DTTargetTox2", "Please enter the target toxicity probability of the study agent", 
                  "top", options = list(container = "body")),
        sliderInput("DTNumTrials2", "Number of Simulated Trials", min = 0, max = 10000, value = 100),
        bsTooltip("DTNumTrials2", "Please enter the number of simulated trials. A larger number of simulations increases the precision of simulation results and computation time", 
                  "top", options = list(container = "body")),
        textInput("DTTrueTox2", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        bsTooltip("DTTrueTox2", "Please enter the true toxicity probabilities for each dose level evaluated in the trial. Toxicity probabilities must increase with each subsequent dose level", 
                  "top", options = list(container = "body")),
        sliderInput("DTArrivalRate2", "Patient Enrollment Rate", min = 0, max = 180, value = 15),
        bsTooltip("DTArrivalRate2", "Please enter the average time between enrolling patients (in days)", 
                  "top", options = list(container = "body")),
        sliderInput("DTPropB2", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
        bsTooltip("DTPropB2", "Please enter the proportion of enrolled patients belonging to the “enrichment” Cohort B", 
                  "top", options = list(container = "body")),
        selectInput("DTTargetCRM", "Target-CRM Option", choices = c(0,1,2), selected = 1),
        bsTooltip("DTTargetCRM", "Please enter the desired variation of the TARGET-CRM design", 
                  "top", options = list(container = "body")),
        sliderInput("DTMaxN", "Maximum Sample Size", min = 1, max = 200, value = 18),
        bsTooltip("DTMaxN", "Please enter the maximum number of patients to be enrolled per trial", 
                  "top", options = list(container = "body")),
        sliderInput("DTMinCohortB", "Minimum Enrollment of Cohort B Patients (Optional)", min = 0, max = 100, value = 2),
        bsTooltip("DTMinCohortB", "Please enter the minimum number of Cohort B patients to be enrolled in the trial", 
                  "top", options = list(container = "body")),
        sliderInput("DTCycleLength2", "Duration of DLT Observation Period", min = 0, max = 365, value = 28),
        bsTooltip("DTCycleLength2", "Please enter the duration of the DLT observation period (in days)", 
                  "top", options = list(container = "body")),
        selectInput("DTCohortSize", "Cohort Size", choices = c(seq(1,9)), selected = 3),
        bsTooltip("DTCohortSize", "Please enter the cohort size. The cohort size is the number of patients to be treated at the current dose level before a dose escalation decision is made", 
                  "top", options = list(container = "body"))
      )
    }
  })
  
  # Update Start Level Based on Dose Labels
  observe({
    updateSelectInput(session, "DTStartLevel", choices = numerizer(input$DTDoseLabels), selected = numerizer(input$DTDoseLabels)[2])
  })
  
  # Update Max Depending on Previous Input
  observe({
    updateSliderInput(session, "DTMinCohortB", max = input$DTMaxN)
  })
  
  # UI if No Design Selected
  output$DTNoneUI <- renderUI({
    req(DTSelectedDesignsLength() == 0)
    tagList(
      fluidRow(
        h2("Please select a design to begin", style="color: #ff0033")
      )
    )
  })
  
  # Main Plotting UI
  output$DTPlotsUI <- renderUI({
    req(DTSelectedDesignsLength() > 0)
    tagList(
      column(6,
             withSpinner(plotOutput("DTPlot1"), type = 7, color = "#003087", size = 2),
             withSpinner(plotOutput("DTPlot2"), type = 7, color = "#003087", size = 2)
      ),
      column(6,
             withSpinner(plotOutput("DTPlot3"), type = 7, color = "#003087", size = 2),
             withSpinner(plotOutput("DTPlot4"), type = 7, color = "#003087", size = 2)
      )
    )
  })
  
  # Running the Design(s)
  DTFunctionOutputs <- eventReactive(input$DTSimulate, {
    
    # 3+3
    if (input$DTSelectorTPT == TRUE) {
      
      TPT <- three.plus.three(target.tox = input$DTTargetTox, number.trials = input$DTNumTrials, 
                              true.tox = numerizer(input$DTTrueTox), arrival.rate = input$DTArrivalRate, 
                              prop.B = input$DTPropB, cycle.length = input$DTCycleLength, start.level = as.numeric(input$DTStartLevel))
    }
    
    # TARGET-CRM
    if(input$DTSelectorTCRM == TRUE) {
      
      TCRM <- target.crm(prior = numerizer(input$DTPriorTox), target.tox = input$DTTargetTox2, 
                         number.trials = input$DTNumTrials2, true.tox = numerizer(input$DTTrueTox2), 
                         arrival.rate = input$DTArrivalRate2, prop.B = input$DTPropB2, target.crm = as.numeric(input$DTTargetCRM), 
                         min.cohortB = input$DTMinCohortB, cycle.length = input$DTCycleLength2, 
                         cohort.size = as.numeric(input$DTCohortSize), max.N = input$DTMaxN, start.level = as.numeric(input$DTStartLevel))
      
    }
    
    # Other
    if(input$DTSelectorOther == TRUE) {
      
      Other <- target.crm(prior = numerizer(input$DTPriorTox), target.tox = input$DTTargetTox2, 
                          number.trials = input$DTNumTrials2, true.tox = numerizer(input$DTTrueTox2), 
                          arrival.rate = input$DTArrivalRate2, prop.B = input$DTPropB2, target.crm = as.numeric(input$DTTargetCRM), 
                          min.cohortB = input$DTMinCohortB, cycle.length = input$DTCycleLength2, 
                          cohort.size = as.numeric(input$DTCohortSize), max.N = input$DTMaxN, start.level = as.numeric(input$DTStartLevel))
    }
    
    all <- list(get0("TPT"), get0("TCRM"), get0("Other"))
    
    return(all[lengths(all) != 0])
    
  })
  
  # DF for Plot
  DTPlotDF <- reactive({
    
    # Multiple Designs Selected
    if (DTSelectedDesignsLength() > 1) {
      funLength <- DTSelectedDesignsLength()
      funList <- list()
      
      for (v in seq(1, funLength)) {
        df <- DTFunctionOutputs()[[v]]$df
        funList[[v]] <- df
      }
      
      finaldf <- bind_rows(funList)
      finaldf$DoseLevel <- rep(unlist(strsplit(input$DTDoseLabels, ",")), funLength)
      finaldf$design <- as.factor(finaldf$design)
      
      return(finaldf)
    }
    
    # Only 1 Design Selected
    else if(DTSelectedDesignsLength() == 1){
      
      df <- DTFunctionOutputs()[[1]]$df
      df$DoseLevel <- unlist(strsplit(input$DTDoseLabels, ","))
      df$design <- as.factor(df$design)
      return(df)
    }
  })
  
  # DF2 for Plot
  DTPlotDF2 <- reactive({
    funLength <- DTSelectedDesignsLength()
    funList <- list()
    
    for (v in seq(1, funLength)) {
      df <- data.frame("design"=DTFunctionOutputs()[[v]]$df$design[1], "MeanDuration"=DTFunctionOutputs()[[v]]$mean.duration, 
                       "SDDuration"=DTFunctionOutputs()[[v]]$sd.duration)
      funList[[v]] <- df
      
    }
    finaldf <- bind_rows(funList)
    finaldf$design <- as.factor(finaldf$design)
    return(finaldf)
  })
  
  
  
  # Plot1
  output$DTPlot1 <- renderPlot({
    
    if(DTSelectedDesignsLength() > 1){
      
      DTPlotDF() %>% mutate(MTD.Prop=MTD.Freq/input$DTNumTrials2) %>%
        ggplot(aes(x=DoseLevel, y=MTD.Prop, fill=design)) + 
        geom_bar(stat="identity", position="dodge") + xlab("Dose Level") + ylab("Proportion of Simulated Trials") +
        ggtitle("Proportion of Simulated Trials Selecting Each Dose Level as True MTD")
    }
    
    else if (DTSelectedDesignsLength() == 1){
      
      DTPlotDF() %>% mutate(MTD.Prop = MTD.Freq/input$DTNumTrials) %>%
        ggplot(aes(x=DoseLevel, y=MTD.Prop)) + 
        geom_bar(stat='identity') + xlab("Dose Level") + 
        ylab("Proportion of Simulated Trials") + ggtitle("Proportion of Simulated Trials Selecting Each Dose Level as True MTD")
    }
    
  })
  
  # Plot2
  output$DTPlot2 <- renderPlot({
    
    if (DTSelectedDesignsLength() > 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=obs.tox.table, fill=design)) + 
        geom_bar(stat="identity", position="dodge") + geom_hline(aes(yintercept=input$DTTargetTox2), linetype="dashed") +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + 
        ggtitle("Proportion of Patients Experiencing a DLT Per Dose Level")
    }
    
    else if (DTSelectedDesignsLength() == 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=obs.tox.table)) + 
        geom_bar(stat="identity") + geom_hline(aes(yintercept=input$DTTargetTox), linetype="dashed") +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + 
        ggtitle("Proportion of Patients Experiencing a DLT Per Dose Level")
    }
  })
  
  # Plot3
  output$DTPlot3 <- renderPlot({
    
    if (DTSelectedDesignsLength() > 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=patient.allocation.table, fill=design)) + 
        geom_bar(stat="identity", position="dodge") + xlab("Dose Level") + ylab("Proportion of Patients Allocated") + 
        ggtitle("Proportion of Patients Allocated to Each Dose Level")
    }
    
    else if (DTSelectedDesignsLength() == 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=patient.allocation.table)) + 
        geom_bar(stat="identity") + xlab("Dose Level") + ylab("Proportion of Patients Allocated") + 
        ggtitle("Proportion of Patients Allocated to Each Dose Level")
    }
  })
  
  # Plot4
  output$DTPlot4 <- renderPlot({
    
    DTPlotDF2() %>%
      ggplot(aes(x=design, y=MeanDuration)) + 
      geom_bar(stat="identity") + geom_errorbar(aes(ymin= MeanDuration - SDDuration, ymax = MeanDuration + SDDuration)) + xlab("Design") +
      ylab("Mean Study Duration (Days)") + ggtitle("Mean Study Duration in Days (+/- 1 SD)")
  })
}

shinyApp(ui, server)