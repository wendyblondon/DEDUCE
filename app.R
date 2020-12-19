library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(waiter)
library(tidyverse)

source("target_crm.R")
source("three_plus_three.R")

# Functions
numerizer <- function(x){
  as.numeric(unlist(strsplit(x, ",")))
}

sequencer <- function(x){
  seq(1,length(unlist(strsplit(x, ","))))
}

# CSS
CSS <- "
#DTSimulate:hover{
  background-color: #4CAF50;
  color: white;
}

#DTReset:hover{
  background-color: #f44336;
  color: white;
}

#DTPlot1 {
height: calc(50vh - 50px) !important;} 

#DTPlot2 {
height: calc(50vh - 50px) !important;} 

#DTPlot3 {
height: calc(50vh - 50px) !important;} 

#DTPlot4 {
height: calc(50vh - 50px) !important;} 
"

ui <- dashboardPage(title = "DELPHI", skin = "black",
                    dashboardHeader(title = strong("DELPHI")),
                    dashboardSidebar(
                      sidebarMenu(id='tabs',
                                  menuItem("Home", tabName = "Home", icon = icon("home")),
                                  menuItem("Design", tabName = "Design", icon = icon("pen")),
                                  menuItem("Conduct", tabName = "Conduct", icon = icon("dolly-flatbed")),
                                  menuItem("Help", tabName = "Help", icon = icon("question-circle"))
                      )
                    ),
                    dashboardBody(
                      useShinyjs(), inlineCSS(CSS), useShinyFeedback(), use_waiter(),
                      waiter_show_on_load(logo = "logo.PNG", color = "white"),
                      tabItems(
                        tabItem(tabName = "Home",
                                h1("Something Here")
                        ),
                        tabItem(tabName = "Design",
                                fluidRow(
                                  column(3, style="overflow-y:scroll; height: 70vh;",
                                         h1("Inputs", style="text-align: center; text-decoration: underline;"),
                                         br(),
                                         h4("Designs:", style = "font-weight: bold;"),
                                         prettyCheckbox("DTSelectorTPT", "3+3", value = TRUE, status = "success", shape = "round", fill = TRUE, inline = TRUE),
                                         prettyCheckbox("DTSelectorTCRM", "TARGET-CRM", value = FALSE, status = "success", shape = "round", fill = TRUE, inline = TRUE),
                                         prettyCheckbox("DTSelectorOther", "Other", value = FALSE, status = "success", shape = "round", fill = TRUE, inline = TRUE),
                                         sliderInput("DTNumDoses", "How Many Doses Will There Be?", min = 3, max = 10, value = 4, width = "100%", ticks = FALSE),
                                         textInput("DTDoseLabels", "Dose Level Labels", value = "-1,1,2,3", width = "100%"),
                                         bsTooltip("DTDoseLabels", "Please enter the dose level labels (seperated by commas) for each dose level evaluated in the trial", 
                                                   "top", options = list(container = "body")),
                                         selectInput("DTStartLevel", "Starting Dose Level", choices = c(-1,1,2,3), selected = 1, width = "100%"),
                                         bsTooltip("DTStartLevel", "Please enter the starting dose level from the dose level labels above", 
                                                   "top", options = list(container = "body")),
                                         uiOutput("DTInputs"),
                                         splitLayout(cellWidths = c("50%", "25%", "25%"),
                                                     actionButton("DTSimulate", "Simulate", width = "100%", style = "font-weight: bold;"),
                                                     actionButton("DTResults", "Results", width = "100%", style = "font-weight: bold;"),
                                                     actionButton("DTReset", "Reset", width = "100%", style = "font-weight: bold;")
                                         ),
                                         bsTooltip("DTSimulate", "Simulates the selected design(s) using the values of the above inputs", 
                                                   "top", options = list(container = "body")),
                                         bsTooltip("DTResults", "Shows the table and summary of results", 
                                                   "top", options = list(container = "body")),
                                         bsTooltip("DTReset", "WARNING: Resets all of the inputs and results, cannot be undone", 
                                                   "top", options = list(container = "body"))
                                  ),
                                  column(9,
                                         uiOutput("DTPlotsUI"),
                                         uiOutput("DTNoneUI")
                                  )
                                )
                        ),
                        tabItem(tabName = "Conduct",
                                h1("Something Here")
                        ),
                        tabItem(tabName = "Help",
                                h1("Something Here")
                        )
                      )
                    )
)

server <- function(input, output, session) {
  waiter_hide()
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
      div(id="DTUISome",
          sliderInput("DTTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1, width = "100%", ticks = FALSE),
          bsTooltip("DTTargetTox", "Please enter the target toxicity probability of the study agent", 
                    "top", options = list(container = "body")),
          sliderInput("DTNumTrials", "Number of Simulated Trials", min = 0, max = 10000, value = 100, width = "100%", ticks = FALSE),
          bsTooltip("DTNumTrials", "Please enter the number of simulated trials. A larger number of simulations increases the precision of simulation results and computation time", 
                    "top", options = list(container = "body")),
          textInput("DTTrueTox", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3", width = "100%"),
          bsTooltip("DTTrueTox", "Please enter the true toxicity probabilities for each dose level evaluated in the trial. Toxicity probabilities must increase with each subsequent dose level", 
                    "top", options = list(container = "body")),
          sliderInput("DTArrivalRate", "Patient Enrollment Rate", min = 0, max = 180, value = 15, width = "100%", ticks = FALSE),
          bsTooltip("DTArrivalRate", "Please enter the average time between enrolling patients (in days)", 
                    "top", options = list(container = "body")),
          sliderInput("DTPropB", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1, width = "100%", ticks = FALSE),
          bsTooltip("DTPropB", "Please enter the proportion of enrolled patients belonging to the “enrichment” Cohort B", 
                    "top", options = list(container = "body")),
          sliderInput("DTCycleLength", "Duration of DLT Observation Period", min = 0, max = 365, value = 28, width = "100%", ticks = FALSE),
          bsTooltip("DTCycleLength", "Please enter the duration of the DLT observation period (in days)", 
                    "top", options = list(container = "body"))
          
      )
    }
    # TARGET-CRM or Both
    else if (DTSelectedDesigns() == "all") {
      div(id="DTUIAll",
          textInput("DTPriorTox", "Prior Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3", width = "100%"),
          bsTooltip("DTPriorTox", "Please enter the prior toxicity probabilities for each dose level evaluated in the trial. Toxicity probabilities must increase with each subsequent dose level", 
                    "top", options = list(container = "body")),
          sliderInput("DTTargetTox2", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1, width = "100%", ticks = FALSE),
          bsTooltip("DTTargetTox2", "Please enter the target toxicity probability of the study agent", 
                    "top", options = list(container = "body")),
          sliderInput("DTNumTrials2", "Number of Simulated Trials", min = 0, max = 10000, value = 100, width = "100%", ticks = FALSE),
          bsTooltip("DTNumTrials2", "Please enter the number of simulated trials. A larger number of simulations increases the precision of simulation results and computation time", 
                    "top", options = list(container = "body")),
          textInput("DTTrueTox2", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3", width = "100%"),
          bsTooltip("DTTrueTox2", "Please enter the true toxicity probabilities for each dose level evaluated in the trial. Toxicity probabilities must increase with each subsequent dose level", 
                    "top", options = list(container = "body")),
          sliderInput("DTArrivalRate2", "Patient Enrollment Rate", min = 0, max = 180, value = 15, width = "100%", ticks = FALSE),
          bsTooltip("DTArrivalRate2", "Please enter the average time between enrolling patients (in days)", 
                    "top", options = list(container = "body")),
          sliderInput("DTPropB2", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1, width = "100%", ticks = FALSE),
          bsTooltip("DTPropB2", "Please enter the proportion of enrolled patients belonging to the “enrichment” Cohort B", 
                    "top", options = list(container = "body")),
          selectInput("DTTargetCRM", "Target-CRM Option", choices = c(0,1,2), selected = 1, width = "100%"),
          bsTooltip("DTTargetCRM", "Please enter the desired variation of the TARGET-CRM design", 
                    "top", options = list(container = "body")),
          sliderInput("DTMaxN", "Maximum Sample Size", min = 1, max = 200, value = 18, width = "100%", ticks = FALSE),
          bsTooltip("DTMaxN", "Please enter the maximum number of patients to be enrolled per trial", 
                    "top", options = list(container = "body")),
          sliderInput("DTMinCohortB", "Minimum Enrollment of Cohort B Patients (Optional)", min = 0, max = 100, value = 2, width = "100%", ticks = FALSE),
          bsTooltip("DTMinCohortB", "Please enter the minimum number of Cohort B patients to be enrolled in the trial", 
                    "top", options = list(container = "body")),
          sliderInput("DTCycleLength2", "Duration of DLT Observation Period", min = 0, max = 365, value = 28, width = "100%", ticks = FALSE),
          bsTooltip("DTCycleLength2", "Please enter the duration of the DLT observation period (in days)", 
                    "top", options = list(container = "body")),
          selectInput("DTCohortSize", "Cohort Size", choices = c(seq(1,9)), selected = 3, width = "100%"),
          bsTooltip("DTCohortSize", "Please enter the cohort size. The cohort size is the number of patients to be treated at the current dose level before a dose escalation decision is made", 
                    "top", options = list(container = "body"))
      )
    }
  })
  
  # Update Start Level Based on Dose Labels
  observe({
    updateSelectInput(session, "DTStartLevel", choices = sequencer(input$DTDoseLabels), selected = sequencer(input$DTDoseLabels)[2])
  })
  
  # Update Max Depending on Previous Input
  observe({
    updateSliderInput(session, "DTMinCohortB", max = input$DTMaxN)
  })
  
  ## Warning for Invalid Inputs
  # Dose Labels
  observeEvent(list(input$DTDoseLabels, input$DTNumDoses), {
    req(input$DTDoseLabels)
    feedbackDanger(
      inputId = "DTDoseLabels",
      show = length(unlist(strsplit(input$DTDoseLabels, ",")))!= input$DTNumDoses,
      text = "The length must match the # of doses chosen above"
    )
  })
  # True Tox
  observeEvent(list(input$DTTrueTox, input$DTNumDoses), {
    req(input$DTTrueTox)
    feedbackDanger(
      inputId = "DTTrueTox",
      show = length(unlist(strsplit(input$DTTrueTox, ",")))!= input$DTNumDoses,
      text = "The length must match the # of doses chosen above"
    )
  })
  # True Tox 2
  observeEvent(list(input$DTTrueTox2, input$DTNumDoses), {
    req(input$DTTrueTox2)
    feedbackDanger(
      inputId = "DTTrueTox2",
      show = length(unlist(strsplit(input$DTTrueTox2, ",")))!= input$DTNumDoses,
      text = "The length must match the # of doses chosen above"
    )
  })
  # Prior Tox
  observeEvent(list(input$DTPriorTox, input$DTNumDoses), {
    req(input$DTPriorTox)
    feedbackDanger(
      inputId = "DTPriorTox",
      show = length(unlist(strsplit(input$DTPriorTox, ",")))!= input$DTNumDoses,
      text = "The length must match the # of doses chosen above"
    )
  })
  
  # Main Plotting UI
  output$DTPlotsUI <- renderUI({
    req(DTSelectedDesignsLength() > 0)
    hidden(
      div(id="DTUIPlots",
          column(6,
                 plotOutput("DTPlot1", width = "100%", height = "100%"),
                 plotOutput("DTPlot2", width = "100%", height = "100%"),
          ),
          column(6,
                 plotOutput("DTPlot3", width = "100%", height = "100%"),
                 plotOutput("DTPlot4", width = "100%", height = "100%"),
          )
      )
    )
  })
  
  # Shows the Plots UI After Clicking Simulate
  observeEvent(input$DTSimulate, {
    show("DTUIPlots")
  })
  
  # Hide/Reset the UI Elements
  observeEvent(input$DTReset, {
    hide("DTUIPlots")
    reset("DTSelectorTPT")
    reset("DTSelectorTCRM")
    reset("DTSelectorOther")
    reset("DTDoseLabels")
    reset("DTStartLevel")
    reset("DTUISome")
    reset("DTUIAll")
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
  
  # Running the Design(s)
  DTFunctionOutputs <- eventReactive(input$DTSimulate, {
    w <- Waiter$new(html = spin_heartbeat(), color = "black")
    w$show()
    
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
    w$hide()
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