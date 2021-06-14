library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(waiter)
library(tidyverse)
library(rmarkdown)

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

designInputs <- function(x){
  
  trues <- which(x==TRUE)
  
  for (i in trues) {
    if (i == 1) {
      x[i] <-"3+3"
    }
    else if (i == 2) {
      x[i] <- "TARGET-CRM"
    }
    
    else{
      x[i] <- "CRM"
    }
  }
  
  x <- x[trues]
  return(x)
}

ui <- dashboardPage(title = "DEDUCE", skin = "black",
                    dashboardHeader(title = strong("DEDUCE")),
                    dashboardSidebar(
                      useShinyjs(), includeCSS("www/style.css"), useShinyFeedback(), use_waiter(),
                                     sidebarMenu(id='tabs',
                                                 menuItem("Home", tabName = "Home", icon = icon("home")),
                                                 menuItem("Design", tabName = "Design", icon = icon("pen")),
                                                 menuItem("Conduct", tabName = "Conduct", icon = icon("dolly-flatbed")),
                                                 menuItem("Help", href="https://drive.google.com/file/d/18MGkaaIYFfJ5gqi1vGqnf7Myy0QjLs-i/view", 
                                                          icon = icon("external-link-alt")),
                                                 menuItem("About", tabName = "About", icon = icon("info-circle"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                tags$script(HTML("$('body').addClass('fixed');")),
                                img(id="homeimg", src = "home.png"),
                                p(id="hometagline", "DEsign and conDUCt of dose Escalation trials (DEDUCE) platform - a unified resource for clinical investigators and statisticians to design and conduct more efficient 
                                  and accurate phase 1 trials."),
                                h2(id="homeh4", "Overview"),
                                p("The DEDUCE platform is an interactive, web-based resource to design and conduct 
                                  phase 1 dose escalation trials using rule-based and Bayesian adaptive designs. Our goal in developing this application is to raise 
                                  awareness, educate, and provide open access to investigators for alternative, improved methods and tools to design and conduct phase 
                                  1 dose escalation trials."),
                                h2(id="homeh4", "DEDUCE Modules:"),
                                tags$ul(
                                  tags$li(
                                    h5(class="home", "Trial Design"),
                                    p("Users can specify and compare the operating characteristics for hypothetical phase 1 designs through trial simulations, 
                                    and select an optimal design for the needs of the trial.")
                                  ),
                                  tags$li(
                                    h5(class="home", "Trial Conduct"),
                                    p("Users can implement the adaptive trial, and determine the recommended dose level each time a new patient enrolls.")
                                  )    
                                ),
                                
                                h2("Available Designs:"),
                                tags$ul(
                                  tags$li(
                                    a("Continual Reassessment Method (CRM) [O'Quigley et al. Biometrics, 1990]", href="https://pubmed.ncbi.nlm.nih.gov/2350571/", 
                                      target="_blank", rel="noopener noreferrer")
                                  ),
                                  tags$li("TARGETed-Agent Continual Reassessment Method (TARGET-CRM)"
                                  ), 
                                  tags$li(
                                    a("3+3 [Storer. Biometrics, 1989]", href="https://pubmed.ncbi.nlm.nih.gov/2790129/", target="_blank", rel="noopener noreferrer")
                                  )   
                                ),
                                
                                h2("Key Features of DEDUCE:"),
                                tags$ul(
                                  tags$li("Permits simultaneous comparison of multiple trial designs for the same set of simulation parameters"
                                  ),
                                  tags$li("Dynamically generates a written report summarizing simulation results"
                                  )
                                ),
                                a(href="https://www.danafarberbostonchildrens.org", img(id="DFLogo", src = "danafarber_bostonchildrens_logo.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer"),
                                a(href="https://www.NorthwesternMutual.com", img(id="NMLogo", src = "NMLogo.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer"),
                                a(href="https://hms.harvard.edu/", img(id="HMSLogo", src = "HMS.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer")
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
                                         prettyCheckbox("DTSelectorCRM", "CRM", value = FALSE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("DTSelectorCRM", "Select to run the CRM Design", 
                                                   "top", options = list(container = "body")),
                                         prettyCheckbox("DTSelectorTCRM", "TARGET-CRM", value = FALSE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("DTSelectorTCRM", "Select to run the TARGET-CRM Design", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("DTNumDoses", "Number of Dose Levels", min = 3, max = 10, value = 4, width = "100%", ticks = FALSE),
                                         bsTooltip("DTNumDoses", "Please enter the number of doses that will be used", 
                                                   "top", options = list(container = "body")),
                                         textInput("DTDoseLabels", "Dose Level Labels", value = "-1,1,2,3", width = "100%"),
                                         bsTooltip("DTDoseLabels", "Please enter the dose level labels (separated by commas) for each dose level evaluated in the trial", 
                                                   "top", options = list(container = "body")),
                                         selectInput("DTStartLevel", "Starting Dose Level", choices = c(-1,1,2,3), selected = 1, width = "100%"),
                                         bsTooltip("DTStartLevel", "Please enter the starting dose level from the dose level labels above", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("DTNumTrials", "Number of Simulated Trials", min = 0, max = 10000, value = 100, width = "100%", step = 100, ticks = FALSE),
                                         bsTooltip("DTNumTrials", "Please enter the number of simulated trials. A larger number of simulations increases the precision of simulation results and computation time", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("DTTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.01, width = "100%", ticks = FALSE),
                                         bsTooltip("DTTargetTox", "Please enter the target toxicity probability of the study agent", 
                                                   "top", options = list(container = "body")),
                                         textInput("DTTrueTox", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3", width = "100%"),
                                         bsTooltip("DTTrueTox", "Please enter the true toxicity probabilities for each dose level (separated by commas). Toxicity probabilities must increase with each subsequent dose level", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("DTArrivalRate", "Average Time Between Patient Enrollments (In Days)", min = 0, max = 180, value = 15, width = "100%", ticks = FALSE),
                                         bsTooltip("DTArrivalRate", "Please enter the average time between enrolling patients (In Days)", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("DTPropB", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.01, width = "100%", ticks = FALSE),
                                         bsTooltip("DTPropB", "Patients belong to either Cohort A (general enrollment) or Cohort B (enrichment cohort). Please enter the proportion of enrolled patients belonging to Cohort B. Enter a proportion of 0 if no enrichment cohort is needed.", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("DTCycleLength", "Duration of DLT Observation Period (In Days)", min = 0, max = 365, value = 28, width = "100%", ticks = FALSE),
                                         bsTooltip("DTCycleLength", "Please enter the duration of the DLT observation period (In Days)", 
                                                   "top", options = list(container = "body")),
                                         conditionalPanel(
                                           condition = "input.DTSelectorTCRM == 1 || input.DTSelectorCRM == 1",
                                           textInput("DTPriorTox", "Prior Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3", width = "100%"),
                                           bsTooltip("DTPriorTox", "Please enter the prior toxicity probabilities for each dose level (separated by commas). Toxicity probabilities must increase with each subsequent dose level", 
                                                     "top", options = list(container = "body")),
                                           sliderInput("DTMaxN", "Maximum Sample Size", min = 1, max = 200, value = 18, width = "100%", ticks = FALSE),
                                           bsTooltip("DTMaxN", "Please enter the maximum number of patients to be enrolled per trial", 
                                                     "top", options = list(container = "body")),
                                           sliderInput("DTMinCohortB", "Minimum Enrollment of Cohort B Patients (Optional)", min = 0, max = 100, value = 0, width = "100%", ticks = FALSE),
                                           bsTooltip("DTMinCohortB", "An optional feature is to require a trial to enroll a minimum number of Cohort B patients. Once the maximum N is attained, enrollment of Cohort A patients will be suspended and only Cohort B patients may enroll until the minimum number has been attained. Please enter the minimum number of Cohort B patients to be enrolled in a trial. Enter 0 if no minimum number is required.", 
                                                     "top", options = list(container = "body")),
                                           sliderInput("DTCohortSize", "Cohort Size", min = 1, max = 9, value = 3, width = "100%", ticks = FALSE),
                                           bsTooltip("DTCohortSize", "Please enter the cohort size. The cohort size is the number of patients to be treated at the current dose level before a dose escalation decision is made", 
                                                     "top", options = list(container = "body"))
                                         ),
                                         splitLayout(cellWidths = c("50%", "25%", "25%"),
                                                     actionButton("DTSimulate", "Simulate", width = "100%", style = "font-weight: bold;"),
                                                     downloadButton("DTResults", "", style = "font-weight: bold; width: 100%;"),
                                                     actionButton("DTReset", "Reset", width = "100%", style = "font-weight: bold;")
                                         ),
                                         bsTooltip("DTSimulate", "Simulates the selected design(s) using the values of the above inputs", 
                                                   "top", options = list(container = "body")),
                                         bsTooltip("DTResults", "Download the full report of plots, tables, and summaries", 
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
                                h1("Coming Soon...")
                        ),
                        
                        tabItem(tabName = "Help"
                        ),
                        
                        tabItem(tabName = "About",
                                h2("DEDUCE Leadership: Dana-Farber/Boston Children's Cancer and Blood Disorders Center"),
                                tags$ul(
                                  tags$li("Clement Ma, PhD"),
                                  tags$li("Wendy B. London, PhD")
                                ),
                                h2("Development Team: Northwestern Mutual"),
                                tags$ul(
                                  tags$li("Judy Berdan"),
                                  tags$li("Laure Borchardt"),
                                  tags$li("Audra Brennan"),
                                  tags$li("Stan Crane"),
                                  tags$li("Ben Garski"),
                                  tags$li("Nanette Jamel"),
                                  tags$li("Lori Kiraly"),
                                  tags$li("Danielle Pankey"),
                                  tags$li("Susan Stegman, MD")
                                ),
                                h2("Contact:"),
                                p("For assistance, please contact:"),
                                tags$ul(
                                  tags$li("Drs. Clement Ma and Wendy B. London")
                                ),
                                h2("Citation:"),
                                p("To cite the DEDUCE platform please use:"),
                                tags$ul(
                                  tags$li("[Insert citation to published paper]")
                                ),
                                h2(id="homeh4", "Acknowledgements:"),
                                p("We would like to thank the Northwestern Mutual Tech for Good team for their pro-bono development, design, and project management 
                                support for the DEDUCE platform. We would also like to thank our test users, Drs. Steven G. DuBois, Karen D. Wright, 
                                and David S. Shulman for their helpful feedback."),
                                h2("References:"),
                                tags$ul(
                                  tags$li(
                                    a("Storer BE. Design and Analysis of Phase I Clinical Trials. ", em("Biometrics. "), "1989;45(3):925-37.",
                                      href="https://pubmed.ncbi.nlm.nih.gov/2790129/", target="_blank", rel="noopener noreferrer")
                                  ),
                                  tags$li(
                                    a("O'Quigley J, Pepe M, Fisher L. Continual reassessment method: a practical design for phase 1 Clinical trials in cancer. ", 
                                      em("Biometrics. "), "1990;46(1):33-48.", href="https://pubmed.ncbi.nlm.nih.gov/2350571/", 
                                      target="_blank", rel="noopener noreferrer")
                                  )
                                ),
                                a(href="https://www.danafarberbostonchildrens.org", img(id="DFLogo", src = "danafarber_bostonchildrens_logo.png"), target="_blank", rel="noopener noreferrer"),
                                a(href="https://www.NorthwesternMutual.com", img(id="NMLogo", src = "NMLogo.png"), target="_blank", rel="noopener noreferrer"),
                                a(href="https://hms.harvard.edu/", img(id="HMSLogo", src = "HMS.png"), target="_blank", rel="noopener noreferrer")
                                
                        )
                      )
                    )
)

server <- function(input, output, session) {
  
  # Disable Results Button Until Design(s) Ran
  disable("DTResults")
  
  # Get the Design Names That Are Selected
  DTSelectedDesignNames <- reactive({
    designInputs(c(input$DTSelectorTPT, input$DTSelectorTCRM, input$DTSelectorCRM))
  })
  
  # Update Start Level Based on Dose Labels
  observe({
    updateSelectInput(session, "DTStartLevel", choices = unlist(strsplit(input$DTDoseLabels, ",")), selected = unlist(strsplit(input$DTDoseLabels, ","))[2])
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
    req(length(DTSelectedDesignNames()) > 0)
    hidden(
      div(id="DTUIPlots",
          column(6,
                 plotOutput("DTPlot1"),
                 br(),
                 plotOutput("DTPlot2")
          ),
          column(6,
                 plotOutput("DTPlot3"),
                 br(),
                 plotOutput("DTPlot4")
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
    reset("DTNumDoses")
    reset("DTDoseLabels")
    reset("DTStartLevel")
    reset("DTNumTrials")
    reset("DTTargetTox")
    reset("DTTrueTox")
    reset("DTArrivalRate")
    reset("DTPropB")
    reset("DTCycleLength")
    reset("DTPriorTox")
    reset("DTMaxN")
    reset("DTMinCohortB")
    reset("DTCohortSize")
    disable("DTResults")
  })
  
  # UI if No Design Selected
  output$DTNoneUI <- renderUI({
    req(length(DTSelectedDesignNames()) == 0)
    tagList(
      fluidRow(
        h2("Please select a design to begin", style="color: #ff0033")
      )
    )
  })
  
  # Disable Simulate Button if No Designs Selected
  observe({
    if(input$DTSelectorTPT == 0 & input$DTSelectorTCRM == 0 & input$DTSelectorCRM == 0){
      disable("DTSimulate")
    }
    else{
      enable("DTSimulate")
    }
  })
  
  # Running the Design(s)
  DTFunctionOutputs <- eventReactive(input$DTSimulate, {
    w <- Waiter$new(html = spin_heartbeat(), color = "black")
    w$show()
    
    # 3+3
    if (input$DTSelectorTPT == TRUE) {
      
      TPT <- three.plus.three(target.tox = input$DTTargetTox, number.trials = input$DTNumTrials, 
                              true.tox = numerizer(input$DTTrueTox), arrival.rate = input$DTArrivalRate, 
                              prop.B = input$DTPropB, cycle.length = input$DTCycleLength, start.level = match(input$DTStartLevel, unlist(strsplit(input$DTDoseLabels, ","))))
    }
    
    # TARGET-CRM
    if(input$DTSelectorTCRM == TRUE) {
      
      TCRM <- my.target.crm(prior = numerizer(input$DTPriorTox), target.tox = input$DTTargetTox, 
                            number.trials = input$DTNumTrials, true.tox = numerizer(input$DTTrueTox), 
                            arrival.rate = input$DTArrivalRate, prop.B = input$DTPropB, min.cohortB = input$DTMinCohortB, cycle.length = input$DTCycleLength, 
                            cohort.size = input$DTCohortSize, max.N = input$DTMaxN, start.level = match(input$DTStartLevel, unlist(strsplit(input$DTDoseLabels, ","))))
      
    }
    
    # Other
    if(input$DTSelectorCRM == TRUE) {
      
      CRM <- my.crm(prior = numerizer(input$DTPriorTox), target.tox = input$DTTargetTox, 
                    number.trials = input$DTNumTrials, true.tox = numerizer(input$DTTrueTox), 
                    arrival.rate = input$DTArrivalRate, prop.B = input$DTPropB, min.cohortB = input$DTMinCohortB, cycle.length = input$DTCycleLength, 
                    cohort.size = input$DTCohortSize, max.N = input$DTMaxN, start.level = match(input$DTStartLevel, unlist(strsplit(input$DTDoseLabels, ","))))
    }
    
    all <- list(get0("TPT"), get0("TCRM"), get0("CRM"))
    w$hide()
    return(all[lengths(all) != 0])
    
    
  })
  
  # DF for Design Results Used in Report
  DTResultsDF <- reactive({
    req(DTFunctionOutputs())
    funList <- list()
    
    for (v in seq(1, length(DTSelectedDesignNames()))) {
      df <- data.frame("Design"=DTSelectedDesignNames()[v], "PCS"=DTFunctionOutputs()[[v]]$PCS, "TrueMTD"=DTFunctionOutputs()[[v]]$true.MTD, 
                       "ObsTox"=DTFunctionOutputs()[[v]]$obs.tox.overall, "TargetTox"=DTFunctionOutputs()[[v]]$target.tox, 
                       "PATMTD"=DTFunctionOutputs()[[v]]$patient.allocation.table[DTFunctionOutputs()[[v]]$true.MTD], 
                       "MeanDuration"=DTFunctionOutputs()[[v]]$mean.duration, "SDDuration"=DTFunctionOutputs()[[v]]$sd.duration, 
                       "MeanObsN"=DTFunctionOutputs()[[v]]$mean.obs.N, "MinObsN"=DTFunctionOutputs()[[v]]$min.obs.N, "MaxObsN"=DTFunctionOutputs()[[v]]$max.obs.N, 
                       "PropB"=DTFunctionOutputs()[[v]]$prop.B, "MeanCohortB"=DTFunctionOutputs()[[v]]$mean.cohortB, "SDCohortB"=DTFunctionOutputs()[[v]]$sd.cohortB)
      funList[[v]] <- df
    }
    
    finaldf <- bind_rows(funList)
    return(finaldf)
  })
  
  # DF for Plots
  DTPlotDF <- reactive({
    
    # Multiple Designs Selected
    if (length(DTSelectedDesignNames()) > 1) {
      funLength <- length(DTSelectedDesignNames())
      funList <- list()
      
      for (v in seq(1, funLength)) {
        df <- DTFunctionOutputs()[[v]]$df
        funList[[v]] <- df
      }
      
      finaldf <- bind_rows(funList)
      finaldf$DoseLevel <- factor(unlist(strsplit(input$DTDoseLabels, ",")), levels=unlist(strsplit(input$DTDoseLabels, ",")))
      finaldf$Design <- as.factor(finaldf$design)
      finaldf$doseNum <- rep(seq(1, length(unlist(strsplit(input$DTDoseLabels, ",")))), length(DTSelectedDesignNames()))
      
      return(finaldf)
    }
    
    # Only 1 Design Selected
    else if(length(DTSelectedDesignNames()) == 1){
      
      df <- DTFunctionOutputs()[[1]]$df
      df$DoseLevel <- factor(unlist(strsplit(input$DTDoseLabels, ",")), levels=unlist(strsplit(input$DTDoseLabels, ",")))
      df$Design <- as.factor(df$design)
      df$doseNum <- seq(1, length(unlist(strsplit(input$DTDoseLabels, ","))))
      return(df)
    }
  })
  
  # DF2 for Plot
  DTPlotDF2 <- reactive({
    funLength <- length(DTSelectedDesignNames())
    funList <- list()
    
    for (v in seq(1, funLength)) {
      df <- data.frame("Design"=DTFunctionOutputs()[[v]]$df$design[1], "MeanDuration"=DTFunctionOutputs()[[v]]$mean.duration, 
                       "SDDuration"=DTFunctionOutputs()[[v]]$sd.duration)
      funList[[v]] <- df
      
    }
    finaldf <- bind_rows(funList)
    finaldf$Design <- as.factor(finaldf$Design)
    return(finaldf)
  })
  
  
  
  # Plot1
  DTPlot1 <- reactive({
    if(length(DTSelectedDesignNames()) > 1){
      ggplot() + 
        geom_bar(data = DTPlotDF() %>% 
                   mutate(MTD.Prop=MTD.Freq/input$DTNumTrials), aes(x=DoseLevel, y=MTD.Prop, fill=Design), stat="identity", position="dodge") + 
        geom_bar(data = DTPlotDF() %>% 
                   mutate(MTD.Prop=MTD.Freq/input$DTNumTrials) %>% 
                   filter(doseNum == trueMTD), aes(x=DoseLevel, y=MTD.Prop, fill=Design, color=as.factor(trueMTD)), stat="identity", position="dodge", size=2) +
        xlab("Dose Level") + ylab("Proportion of Simulated Trials") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Simulated Trials Selecting\nEach Dose Level as True MTD") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(DTSelectedDesignNames()) == 1){
      ggplot() + 
        geom_bar(data=DTPlotDF() %>% 
                   mutate(MTD.Prop = MTD.Freq/input$DTNumTrials), aes(x=DoseLevel, y=MTD.Prop), stat='identity', fill="#BEBEBE") + 
        geom_bar(data=DTPlotDF() %>% 
                   mutate(MTD.Prop = MTD.Freq/input$DTNumTrials) %>% 
                   filter(doseNum == trueMTD), aes(x=DoseLevel, y=MTD.Prop, color=as.factor(trueMTD)), stat="identity", fill="#BEBEBE", size=2) +
        xlab("Dose Level") + ylab("Proportion of Simulated Trials") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Simulated Trials Selecting\nEach Dose Level as True MTD") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  output$DTPlot1 <- renderPlot({
    DTPlot1()
  })
  
  # Plot2
  DTPlot2 <- reactive({
    if (length(DTSelectedDesignNames()) > 1){
      ggplot() + 
        geom_bar(data = DTPlotDF(), aes(x=DoseLevel, y=obs.tox.table, fill=Design), stat="identity", position="dodge") + 
        geom_bar(data = DTPlotDF() %>% 
                   filter(doseNum == trueMTD), aes(x=DoseLevel, y=obs.tox.table, fill=Design, color=as.factor(trueMTD)), stat="identity", position="dodge", size=2) +
        geom_hline(aes(yintercept=input$DTTargetTox), linetype="dashed") + guides(color = FALSE) +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(DTSelectedDesignNames()) == 1){
      ggplot() + 
        geom_bar(data = DTPlotDF(), aes(x=DoseLevel, y=obs.tox.table), stat="identity", position="dodge", fill="#BEBEBE") + 
        geom_bar(data = DTPlotDF() %>% 
                   filter(doseNum == trueMTD), aes(x=DoseLevel, y=obs.tox.table, color=as.factor(trueMTD)), stat="identity", position="dodge", fill="#BEBEBE", size=2) +
        geom_hline(aes(yintercept=input$DTTargetTox), linetype="dashed") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + 
        theme(plot.title = element_text(hjust = 0.5)) + guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  
  output$DTPlot2 <- renderPlot({
    DTPlot2()
  })
  
  # Plot3
  DTPlot3 <- reactive({
    if (length(DTSelectedDesignNames()) > 1){
      ggplot() + 
        geom_bar(data=DTPlotDF(), aes(x=DoseLevel, y=patient.allocation.table, fill=Design), stat="identity", position="dodge") +
        geom_bar(data=DTPlotDF() %>% 
                   filter(doseNum == trueMTD), aes(x=DoseLevel, y=patient.allocation.table, fill=Design, color=as.factor(trueMTD)), stat="identity", position="dodge", size=2) +
        xlab("Dose Level") + ylab("Proportion of Patients Allocated") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(DTSelectedDesignNames()) == 1){
      ggplot() + 
        geom_bar(data=DTPlotDF(), aes(x=DoseLevel, y=patient.allocation.table), stat="identity", position="dodge", fill="#BEBEBE") +
        geom_bar(data=DTPlotDF() %>% 
                   filter(doseNum == trueMTD), aes(x=DoseLevel, y=patient.allocation.table, color=as.factor(trueMTD)), stat="identity", position="dodge", fill="#BEBEBE", size=2) +
        xlab("Dose Level") + ylab("Proportion of Patients Allocated") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) + 
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5)) + 
        guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  
  output$DTPlot3 <- renderPlot({
    DTPlot3()
    
  })
  
  # Plot4
  DTPlot4 <- reactive({
    DTPlotDF2() %>%
      ggplot(aes(x=Design, y=MeanDuration)) + 
      geom_point(size = 5) + geom_errorbar(aes(ymin= MeanDuration - SDDuration, ymax = MeanDuration + SDDuration), width = 0.3) + xlab("Design") +
      ylab("Mean Study Duration (Days)") + ggtitle("Mean Study Duration in Days (+/- 1 SD)") + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$DTPlot4 <- renderPlot({
    DTPlot4()
  })
  
  # Table DF
  DTTable1DF <- reactive({
    
    tableList <- list()
    
    for (v in seq(1, length(DTSelectedDesignNames()))) {
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
    opChars <- c("Proportion of correct selection (PCS)", "True MTD", sprintf("Proportion of trials selecting dose %s as true MTD", unlist(strsplit(input$DTDoseLabels, ","))),
                 "Proportion of patients experiencing a DLT overall", sprintf("Proportion of patients experiencing a DLT at dose %s", unlist(strsplit(input$DTDoseLabels, ","))),
                 "Mean total sample size", "Minimmum total sample size", "Maximum total sample size", 
                 sprintf("Proportion of patients enrolled at dose %s", unlist(strsplit(input$DTDoseLabels, ","))), "Mean study duration in days", 
                 "Standard deviation of study duration in days", "Mean # of cohort B patients enrolled during DTL observation period (TARGET-CRM only)",
                 "Standard deviation of # of cohort B patients enrolled during DLT observation period (TARGET-CRM only)")
    df <- cbind(opChars, df)
    colnames(df)[1] <- "Operating Characteristic"
    return(df)
  })
  
  # Values for Rmd - Methods Section
  DTReportMethods <- reactive({
    req(input$DTSimulate > 0)
    
    if (input$DTSelectorTCRM == 1 | input$DTSelectorCRM == 1) {
      
      x1 <- input$DTNumTrials
      x2 <- input$DTNumDoses
      x3 <- input$DTDoseLabels
      x4 <- input$DTStartLevel
      x5 <- input$DTTrueTox
      x6 <- input$DTTargetTox
      x7 <- input$DTArrivalRate
      x8 <- input$DTCycleLength
      x9 <- input$DTPriorTox
      x10 <- input$DTCohortSize
      x11 <- input$DTMaxN
      x12 <- input$DTPropB
      x13 <- input$DTMinCohortB
      return(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13))
      
    }
    else{
      x1 <- input$DTNumTrials
      x2 <- input$DTNumDoses
      x3 <- input$DTDoseLabels
      x4 <- input$DTStartLevel
      x5 <- input$DTTrueTox
      x6 <- input$DTTargetTox
      x7 <- input$DTArrivalRate
      x8 <- input$DTCycleLength
      return(c(x1,x2,x3,x4,x5,x6,x7,x8))
      
    }
  })
  
  # Values for Rmd - Results Section
  DTReportResults <- reactive({
    
      # 1 Design
      if (length(DTSelectedDesignNames()) == 1) {
        x1 <- DTResultsDF()$PCS
        x2 <- round(DTResultsDF()$ObsTox, 2)
        x3 <- DTResultsDF()$TargetTox
        x4 <- ifelse(x2 > x3, "greater", "lower")
        x5 <- DTResultsDF()$TrueMTD
        x6 <- round(DTResultsDF()$PATMTD, 2)
        x7 <- round(DTResultsDF()$MeanDuration, 2)
        x8 <- round(DTResultsDF()$SDDuration, 2)
        x9 <- DTResultsDF()$MeanObsN
        x10 <- DTResultsDF()$MinObsN
        x11 <- DTResultsDF()$MaxObsN
        
        # Only Needed for TARGET-CRM
        x12 <- DTResultsDF()$PropB
        x13 <- DTResultsDF()$MeanCohortB
        x14 <- DTResultsDF()$SDCohortB
        return(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14))
      }
    
    # 2+ Designs
    else{
      x1 <- DTResultsDF() %>% slice_max(PCS) %>% select(Design) %>% pull()
      x2 <- DTResultsDF()$TrueMTD[1]
      x3 <- paste(sprintf("The proportion of correct selection (PCS) of the MTD for the %s design is %g.", DTResultsDF()$Design, DTResultsDF()$PCS), collapse = " ")
      x4 <- paste(sprintf("The proportion of patients experiencing a DLT for the %s design is %g, which %s the target toxicity probability of %g.", 
                    DTResultsDF()$Design, DTResultsDF()$ObsTox, ifelse(DTResultsDF()$ObsTox > DTResultsDF()$TargetTox, "is greater than", 
                    ifelse(DTResultsDF()$ObsTox == DTResultsDF()$TargetTox, "equals", "is lower than")), DTResultsDF()$TargetTox[1]), collapse = " ")
      x5 <- DTResultsDF() %>% slice_max(PATMTD) %>% select(Design) %>% pull()
      x6 <- paste(sprintf("The proportion of patients assigned to the true MTD for the %s design is %g.", DTResultsDF()$Design, DTResultsDF()$PATMTD), collapse = " ")
      x7 <- DTResultsDF() %>% slice_min(MeanDuration) %>% select(Design) %>% pull()
      x8 <- paste(sprintf("The mean study duration for the %s design is %g days(SD=%g).", DTResultsDF()$Design, DTResultsDF()$MeanDuration, 
                          DTResultsDF()$SDDuration), collapse = " ")
      x9 <- paste(sprintf("The mean total sample size for the %s design is %g (range=%g-%g).", DTResultsDF()$Design, 
                    DTResultsDF()$MeanObsN, DTResultsDF()$MinObsN, DTResultsDF()$MaxObsN), collapse = " ")
      
      # Only Needed for TARGET-CRM
      x10 <- DTResultsDF()$PropB[1]
      x11 <- ifelse(nrow(DTResultsDF() %>% filter(Design == 'TARGET-CRM'))==0, NA, 
                    DTResultsDF() %>% filter(Design == 'TARGET-CRM') %>% select(MeanCohortB) %>% pull())
      x12 <- ifelse(nrow(DTResultsDF() %>% filter(Design == 'TARGET-CRM'))==0, NA, 
                    DTResultsDF() %>% filter(Design == 'TARGET-CRM') %>% select(SDCohortB) %>% pull())
      
      return(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12))
    } 
  })
  
  # Observer to Activate Download Button
  observe({
    if (input$DTSimulate > 0) {
      enable("DTResults")
    }
  })
  
  # Download Results
  output$DTResults <- downloadHandler(
    filename = function(){
      paste0("DELPHI Results ", Sys.time(), ".docx")
    },
    content = function(file){
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(d = DTSelectedDesignNames(), m = DTReportMethods(), r = DTReportResults(), 
                     p1 = DTPlot1(), p2 = DTPlot2(), p3 = DTPlot3(), p4 = DTPlot4(), t = DTTable1DF())
      
      render(tempReport, output_file = file,
             params = params,
             envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)
