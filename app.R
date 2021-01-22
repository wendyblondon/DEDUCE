library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(waiter)
library(tidyverse)

source("crm.R")
source("target_crm.R")
source("three_plus_three.R")

theme_set(theme_minimal(base_size = 15))

# Functions
numerizer <- function(x){
  as.numeric(unlist(strsplit(x, ",")))
}

sequencer <- function(x){
  seq(1,length(unlist(strsplit(x, ","))))
}

incrementCheck <- function(x) {
  if (is.character(x)) {
    
    lengthX <- length(unlist(strsplit(x, ",")))
    
    if (lengthX > 1) {
      vecX <- unlist(strsplit(x, ","))
      
      for (i in 1:(lengthX - 1)) {
        if (vecX[i + 1] > vecX[i]) {
          next
        }
        else{
          return(FALSE)
        }
      }
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    stop("The input must be a character")
  }
}

decimalCheck <- function(x){
  if (is.character(x)) {
    
    lengthX <- length(unlist(strsplit(x, ",")))
    
    if (lengthX > 1) {
      vecX <- unlist(strsplit(x, ","))
      
      for (i in 1:lengthX) {
        if (grepl("[.]", vecX[i]) == FALSE) {
          return(FALSE)
        }
        else{
          next
        }
      }
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    stop("The input must be a character")
  }
}

nullToNA <- function(x){
  ifelse(is.null(x), NA, x)
}

# CSS
CSS <- "
.sidebar-menu{
  font-size: 20px;
}

.pretty{
  font-size: 18px;
}

#DTDoseLabels{
  font-size: 18px;
}

#DTTrueTox{
  font-size: 18px;
}

#DTTrueTox2{
  font-size: 18px;
}

#DTPriorTox{
  font-size: 18px;
}

.control-label{
  font-size: 18px;
}

.irs-min{
  font-size: 16px;
  line-height: 16px;
}

.irs-max{
  font-size: 16px;
  line-height: 16px;
}

.irs-single{
  font-size: 16px;
  line-height: 16px;
}

.selectize-input { 
  font-size: 18px; 
  line-height: 18px;
}

.selectize-dropdown {
  font-size: 18px; 
  line-height: 18px;
}

#DTSimulate:hover{
  background-color: #1b9e77;
  color: white;
}

#DTResults:hover{
  background-color: #7570b3;
  color: white;
}

#DTReset:hover{
  background-color: #d95f02;
  color: white;
}

#DTPlot1 {
  height: calc(50vh - 50px) !important;
} 

#DTPlot2 {
  height: calc(50vh - 50px) !important;
} 

#DTPlot3 {
  height: calc(50vh - 50px) !important;
} 

#DTPlot4 {
  height: calc(50vh - 50px) !important;
} 
"

ui <- dashboardPage(title = "DELPHI", skin = "black",
                    dashboardHeader(title = strong("DELPHI")),
                    dashboardSidebar(useShinyjs(), inlineCSS(CSS), useShinyFeedback(), use_waiter(),
                                     waiter_show_on_load(html = img(src="logo.PNG"), color = "white"),
                                     sidebarMenu(id='tabs',
                                                 menuItem("Home", tabName = "Home", icon = icon("home")),
                                                 menuItem("Design", tabName = "Design", icon = icon("pen")),
                                                 menuItem("Conduct", tabName = "Conduct", icon = icon("dolly-flatbed")),
                                                 menuItem("Help", tabName = "Help", icon = icon("question-circle"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                h1("Something Here")
                        ),
                        tabItem(tabName = "Design",
                                fluidRow(
                                  column(3, style="overflow-y:scroll; height: 70vh;",
                                         h1("Inputs", style="text-align: center; text-decoration: underline;"),
                                         br(),
                                         p("Designs:", style = "font-weight: 700; font-size: 18px;"),
                                         prettyCheckbox("DTSelectorTPT", "3+3", value = TRUE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("DTSelectorTPT", "Select to run the 3+3 Design", 
                                                   "top", options = list(container = "body")),
                                         prettyCheckbox("DTSelectorTCRM", "TARGET-CRM", value = FALSE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("DTSelectorTCRM", "Select to run the TARGET-CRM Design", 
                                                   "top", options = list(container = "body")),
                                         prettyCheckbox("DTSelectorCRM", "CRM", value = FALSE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("DTSelectorOther", "Select to run the CRM Design", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("DTNumDoses", "How Many Doses Will There Be?", min = 3, max = 10, value = 4, width = "100%", ticks = FALSE),
                                         bsTooltip("DTNumDoses", "Please enter the number of doses that will be used", 
                                                   "top", options = list(container = "body")),
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
                                         bsModal("modalTable1", "Summary of Simulation Results", "DTResults", DT::dataTableOutput("DTTable1")),
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
  Sys.sleep(2)
  waiter_hide()
  # Get Selected Designs for Rendering UI Guidance
  DTSelectedDesigns <- reactive({
    
    # Only 3+3 Selected
    if(input$DTSelectorTPT == 1 & input$DTSelectorTCRM == 0 & input$DTSelectorCRM == 0){
      return("some")
    }
    
    # Nothing Selected
    else if(input$DTSelectorTPT == 0 & input$DTSelectorTCRM == 0 & input$DTSelectorCRM == 0){
      return(NULL)
    }
    
    else{
      return("all")
    }
  })
  
  # Get Length of Selected Designs for Plotting Guidance
  DTSelectedDesignsLength <- reactive({
    return(sum(c(input$DTSelectorTPT, input$DTSelectorTCRM, input$DTSelectorCRM)))
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
          bsTooltip("DTPropB", "Please enter the proportion of enrolled patients belonging to the 'enrichment' Cohort B", 
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
          bsTooltip("DTPropB2", "Please enter the proportion of enrolled patients belonging to the 'enrichment' Cohort B", 
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
          sliderInput("DTCohortSize", "Cohort Size", min = 1, max = 9, value = 3, width = "100%", ticks = FALSE),
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
  
  ## Warnings for Invalid Inputs
  
  # Dose Labels
  observeEvent(list(input$DTDoseLabels, input$DTNumDoses), {
    req(input$DTDoseLabels)
    hideFeedback("DTDoseLabels")
    if (length(unlist(strsplit(input$DTDoseLabels, ",")))!= input$DTNumDoses) {
      showFeedbackDanger("DTDoseLabels", "The length must match the number of doses chosen above")
    }
  })
  
  
  # True Tox
  observeEvent(list(input$DTTrueTox, input$DTNumDoses), {
    req(input$DTTrueTox)
    hideFeedback("DTTrueTox")
    if (length(unlist(strsplit(input$DTTrueTox, ",")))!= input$DTNumDoses) {
      showFeedbackDanger("DTTrueTox", "The length must match the number of doses chosen above")
    }
    else if (incrementCheck(input$DTTrueTox)==FALSE) {
      showFeedbackDanger("DTTrueTox", "The probabilities must increase with each subsequent dose")
    }
    else if (decimalCheck(input$DTTrueTox)==FALSE) {
      showFeedbackDanger("DTTrueTox", "The probabilities must be a decimal")
    }
  })
  
  # True Tox 2 
  observeEvent(list(input$DTTrueTox2, input$DTNumDoses), {
    req(input$DTTrueTox2)
    hideFeedback("DTTrueTox2")
    if (length(unlist(strsplit(input$DTTrueTox2, ",")))!= input$DTNumDoses) {
      showFeedbackDanger("DTTrueTox2", "The length must match the number of doses chosen above")
    }
    else if (incrementCheck(input$DTTrueTox2)==FALSE) {
      showFeedbackDanger("DTTrueTox2", "The probabilities must increase with each subsequent dose")
    }
    else if (decimalCheck(input$DTTrueTox2)==FALSE) {
      showFeedbackDanger("DTTrueTox2", "The probabilities must be a decimal")
    }
  })
  
  # Prior Tox
  observeEvent(list(input$DTPriorTox, input$DTNumDoses), {
    req(input$DTPriorTox)
    hideFeedback("DTPriorTox")
    if (length(unlist(strsplit(input$DTPriorTox, ",")))!= input$DTNumDoses) {
      showFeedbackDanger("DTPriorTox", "The length must match the number of doses chosen above")
    }
    else if (incrementCheck(input$DTPriorTox)==FALSE) {
      showFeedbackDanger("DTPriorTox", "The probabilities must increase with each subsequent dose")
    }
    else if (decimalCheck(input$DTPriorTox)==FALSE) {
      showFeedbackDanger("DTPriorTox", "The probabilities must be a decimal")
    }
  })
  
  # Main Plotting UI
  output$DTPlotsUI <- renderUI({
    req(DTSelectedDesignsLength() > 0)
    hidden(
      div(id="DTUIPlots",
          column(6,
                 plotOutput("DTPlot1", width = "100%", height = "100%"),
                 br(),
                 plotOutput("DTPlot2", width = "100%", height = "100%")
          ),
          column(6,
                 plotOutput("DTPlot3", width = "100%", height = "100%"),
                 br(),
                 plotOutput("DTPlot4", width = "100%", height = "100%")
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
    reset("DTSelectorCRM")
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
      
      TCRM <- my.target.crm(prior = numerizer(input$DTPriorTox), target.tox = input$DTTargetTox2, 
                         number.trials = input$DTNumTrials2, true.tox = numerizer(input$DTTrueTox2), 
                         arrival.rate = input$DTArrivalRate2, prop.B = input$DTPropB2, min.cohortB = input$DTMinCohortB, cycle.length = input$DTCycleLength2, 
                         cohort.size = input$DTCohortSize, max.N = input$DTMaxN, start.level = as.numeric(input$DTStartLevel))
      
    }
    
    # Other
    if(input$DTSelectorCRM == TRUE) {
      
      CRM <- my.crm(prior = numerizer(input$DTPriorTox), target.tox = input$DTTargetTox2, 
                          number.trials = input$DTNumTrials2, true.tox = numerizer(input$DTTrueTox2), 
                          arrival.rate = input$DTArrivalRate2, prop.B = input$DTPropB2, min.cohortB = input$DTMinCohortB, cycle.length = input$DTCycleLength2, 
                          cohort.size = input$DTCohortSize, max.N = input$DTMaxN, start.level = as.numeric(input$DTStartLevel))
    }
    
    all <- list(get0("TPT"), get0("TCRM"), get0("CRM"))
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
      finaldf$Design <- as.factor(finaldf$design)
      
      return(finaldf)
    }
    
    # Only 1 Design Selected
    else if(DTSelectedDesignsLength() == 1){
      
      df <- DTFunctionOutputs()[[1]]$df
      df$DoseLevel <- unlist(strsplit(input$DTDoseLabels, ","))
      df$Design <- as.factor(df$design)
      return(df)
    }
  })
  
  # DF2 for Plot
  DTPlotDF2 <- reactive({
    funLength <- DTSelectedDesignsLength()
    funList <- list()
    
    for (v in seq(1, funLength)) {
      df <- data.frame("Design"=DTFunctionOutputs()[[v]]$df$design[1], "MeanDuration"=DTFunctionOutputs()[[v]]$mean.duration, 
                       "SDDuration"=DTFunctionOutputs()[[v]]$sd.duration)
      funList[[v]] <- df
      
    }
    finaldf <- bind_rows(funList)
    finaldf$Design <- as.factor(finaldf$design)
    return(finaldf)
  })
  
  
  
  # Plot1
  output$DTPlot1 <- renderPlot({
    
    if(DTSelectedDesignsLength() > 1){
      
      DTPlotDF() %>% mutate(MTD.Prop=MTD.Freq/input$DTNumTrials2) %>%
        ggplot(aes(x=DoseLevel, y=MTD.Prop, fill=Design)) + 
        geom_bar(stat="identity", position="dodge") + xlab("Dose Level") + ylab("Proportion of Simulated Trials") +
        ggtitle("Proportion of Simulated Trials Selecting\nEach Dose Level as True MTD") + theme(plot.title = element_text(hjust = 0.5))
    }
    
    else if (DTSelectedDesignsLength() == 1){
      
      DTPlotDF() %>% mutate(MTD.Prop = MTD.Freq/input$DTNumTrials) %>%
        ggplot(aes(x=DoseLevel, y=MTD.Prop)) + 
        geom_bar(stat='identity') + xlab("Dose Level") + ylab("Proportion of Simulated Trials") + 
        ggtitle("Proportion of Simulated Trials Selecting\nEach Dose Level as True MTD") + theme(plot.title = element_text(hjust = 0.5))
    }
    
  })
  
  # Plot2
  output$DTPlot2 <- renderPlot({
    
    if (DTSelectedDesignsLength() > 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=obs.tox.table, fill=Design)) + 
        geom_bar(stat="identity", position="dodge") + geom_hline(aes(yintercept=input$DTTargetTox2), linetype="dashed") +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + 
        ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + theme(plot.title = element_text(hjust = 0.5))
    }
    
    else if (DTSelectedDesignsLength() == 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=obs.tox.table)) + 
        geom_bar(stat="identity") + geom_hline(aes(yintercept=input$DTTargetTox), linetype="dashed") +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + 
        ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  # Plot3
  output$DTPlot3 <- renderPlot({
    
    if (DTSelectedDesignsLength() > 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=patient.allocation.table, fill=Design)) + 
        geom_bar(stat="identity", position="dodge") + xlab("Dose Level") + ylab("Proportion of Patients Allocated") + 
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5))
    }
    
    else if (DTSelectedDesignsLength() == 1){
      DTPlotDF() %>%
        ggplot(aes(x=DoseLevel, y=patient.allocation.table)) + 
        geom_bar(stat="identity") + xlab("Dose Level") + ylab("Proportion of Patients Allocated") + 
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  # Plot4
  output$DTPlot4 <- renderPlot({
    
    DTPlotDF2() %>%
      ggplot(aes(x=Design, y=MeanDuration)) + 
      geom_point(size = 5) + geom_errorbar(aes(ymin= MeanDuration - SDDuration, ymax = MeanDuration + SDDuration), width = 0.3) + xlab("Design") +
      ylab("Mean Study Duration (Days)") + ggtitle("Mean Study Duration in Days (+/- 1 SD)") + theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Table DF
  DTTable1DF <- reactive({
    
    tableList <- list()
    
    for (v in seq(1, DTSelectedDesignsLength())) {
      if (DTFunctionOutputs()[[v]]$df$design[1] == "3+3") {
        
        x <- round(unname(c(nullToNA(DTFunctionOutputs()[[v]]$PCS), nullToNA(DTFunctionOutputs()[[v]]$true.MTD), 
                            DTFunctionOutputs()[[v]]$MTD.selection.table/DTFunctionOutputs()[[v]]$number.trials, 
                            nullToNA(DTFunctionOutputs()[[v]]$obs.tox.overall), DTFunctionOutputs()[[v]]$obs.tox.table, 
                            nullToNA(DTFunctionOutputs()[[v]]$mean.obs.N), nullToNA(DTFunctionOutputs()[[v]]$min.obs.N), 
                            nullToNA(DTFunctionOutputs()[[v]]$max.obs.N), DTFunctionOutputs()[[v]]$patient.allocation.table, 
                            DTFunctionOutputs()[[v]]$mean.duration, DTFunctionOutputs()[[v]]$sd.duration, NA, NA)), 3)
        xName <- DTFunctionOutputs()[[v]]$df$design[1]
      }
      else {
        x <- round(unname(c(nullToNA(DTFunctionOutputs()[[v]]$PCS), nullToNA(DTFunctionOutputs()[[v]]$true.MTD), 
                            DTFunctionOutputs()[[v]]$MTD.selection.table/DTFunctionOutputs()[[v]]$number.trials, 
                            nullToNA(DTFunctionOutputs()[[v]]$obs.tox.overall), DTFunctionOutputs()[[v]]$obs.tox.table, 
                            nullToNA(DTFunctionOutputs()[[v]]$mean.obs.N), nullToNA(DTFunctionOutputs()[[v]]$min.obs.N), 
                            nullToNA(DTFunctionOutputs()[[v]]$max.obs.N), DTFunctionOutputs()[[v]]$patient.allocation.table, 
                            DTFunctionOutputs()[[v]]$mean.duration, DTFunctionOutputs()[[v]]$sd.duration, 
                            nullToNA(DTFunctionOutputs()[[v]]$mean.cohortB), nullToNA(DTFunctionOutputs()[[v]]$sd.cohortB))), 3)
        xName <- DTFunctionOutputs()[[v]]$df$design[1]
      }
      
      tableList[[v]] <- x
      names(tableList)[v] <- xName
      
    }
    df <- as.data.frame(do.call(cbind, tableList))
    opChars <- c("Proportion of correct selection (PCS)", "True MTD", sprintf("Proportion of trials selecting dose %d as true MTD", 1:length(numerizer(input$DTDoseLabels))),
                 "Proportion of patients experiencing a DLT overall", sprintf("Proportion of patients experiencing a DLT at dose %d", 1:length(numerizer(input$DTDoseLabels))),
                 "Mean total sample size", "Minimmum total sample size", "Maximum total sample size", 
                 sprintf("Proportion of patients enrolled at dose %d", 1:length(numerizer(input$DTDoseLabels))), "Mean study duration in days", 
                 "Standard deviation of study duration in days", "Mean # of cohort B patients enrolled during DTL observation period (TARGET-CRM only)",
                 "Standard deviation of # of cohort B patients enrolled during DLT observation period (TARGET-CRM only)")
    df <- cbind(opChars, df)
    colnames(df)[1] <- "Operating Characteristic"
    return(df)
  })
  
  # Table Output
  output$DTTable1 <- DT::renderDataTable(DTTable1DF(), extensions = c('Buttons', 'Scroller'), 
                                         options = list(dom = 'Brtip', scrollY = 400, scroller = TRUE, deferRender = TRUE, buttons = c('csv', 'excel', 'pdf')),
                                         rownames = FALSE)
}

shinyApp(ui, server)