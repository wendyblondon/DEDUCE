library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)

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
                            column(4,
                                   panel(style="overflow-y:scroll; max-height: 700px;",
                                         radioButtons("designSelector", "Dose-Escalation Design", choices = c("3+3"=1, "TARGET-CRM"=2, "Both"=3), 
                                                      selected = 1, inline = TRUE),
                                         textInput("designDoseLabels", "Dose Level Labels", value = "-1,1,2,3"),
                                         selectInput("designStartLevel", "Starting Dose Level", choices = c(-1,1,2,3), selected = 1),
                                         uiOutput("designInputs"),
                                         actionButton("designSimulate", "Simulate")
                                   )
                            ),
                            tabBox(width=8,
                                   tabPanel("MTD Plot", withSpinner(plotlyOutput("designPlotly1"), type = 7, color = "#003087", size = 2)
                                   ),
                                   tabPanel("DLT Plot", withSpinner(plotlyOutput("designPlotly2"), type = 7, color = "#003087", size = 2)
                                   ),
                                   tabPanel("Patient Allocation Plot", withSpinner(plotlyOutput("designPlotly3"), type = 7, color = "#003087", size = 2)
                                   ),
                                   tabPanel("Study Duration Plot", withSpinner(plotlyOutput("designPlotly4"), type = 7, color = "#003087", size = 2)
                                   )
                            )
                          )
                 ),
                 tabPanel("Conduct"),
                 tabPanel("Help")
)

server <- function(input, output, session) {
  
  # Rendering UI Select Input Based on Dose Labels
  output$designInputs <- renderUI({
    
    # 3+3
    if (input$designSelector == 1) {
      tagList(
        sliderInput("designTargetTox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("designNumTrials", "Number of Simulated Trials", min = 0, max = 10000, value = 100),
        textInput("designTrueTox", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        sliderInput("designArrivalRate", "Patient Enrollment Rate", min = 0, max = 180, value = 15),
        sliderInput("designPropB", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("designCycleLength", "Duration of DLT Observation Period", min = 0, max = 365, value = 28)
        
      )
    }
    # TARGET-CRM or Both
    else {
      tagList(
        textInput("designPriorTox", "Prior Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        sliderInput("designTargetTox2", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("designNumTrials2", "Number of Simulated Trials", min = 0, max = 10000, value = 100),
        textInput("designTrueTox2", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3"),
        sliderInput("designArrivalRate2", "Patient Enrollment Rate", min = 0, max = 180, value = 15),
        sliderInput("designPropB2", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.1),
        selectInput("designTargetCRM", "Target-CRM Option", choices = c(0,1,2), selected = 1),
        sliderInput("designMaxN", "Maximum Sample Size", min = 1, max = 200, value = 18),
        sliderInput("designMinCohortB", "Minimum Enrollment of Cohort B Patients (Optional)", min = 0, max = 100, value = 2),
        sliderInput("designCycleLength2", "Duration of DLT Observation Period", min = 0, max = 365, value = 28),
        selectInput("designCohortSize", "Cohort Size", choices = c(seq(1,9)), selected = 3)
      )
    }
  })
  
  # Update Start Level Based on Dose Labels
  observe({
    updateSelectInput(session, "designStartLevel", choices = numerizer(input$designDoseLabels), selected = numerizer(input$designDoseLabels)[2])
  })
  
  # Update Max Depending on Previous Input
  observe({
    updateSliderInput(session, "designMinCohortB", max = input$designMaxN)
  })
  
  
  # Running the Design(s)
  designDesign <- eventReactive(input$designSimulate, {
    
    # 3+3
    if (input$designSelector == 1) {
      
      tpt <- three.plus.three(target.tox = input$designTargetTox, number.trials = input$designNumTrials, 
                              true.tox = numerizer(input$designTrueTox), arrival.rate = input$designArrivalRate, 
                              prop.B = input$designPropB, cycle.length = input$designCycleLength, start.level = as.numeric(input$designStartLevel))
      return(tpt)
    }
    
    # TARGET-CRM
    else if(input$designSelector == 2) {
      
      tcrm <- target.crm(prior = numerizer(input$designPriorTox), target.tox = input$designTargetTox2, 
                         number.trials = input$designNumTrials2, true.tox = numerizer(input$designTrueTox2), 
                         arrival.rate = input$designArrivalRate2, prop.B = input$designPropB2, target.crm = as.numeric(input$designTargetCRM), 
                         min.cohortB = input$designMinCohortB, cycle.length = input$designCycleLength2, 
                         cohort.size = as.numeric(input$designCohortSize), max.N = input$designMaxN, start.level = as.numeric(input$designStartLevel))
      
      return(tcrm)
    }
    
    # Both
    else {
      
      designTPT <- three.plus.three(target.tox = input$designTargetTox2, number.trials = input$designNumTrials2, 
                                    true.tox = numerizer(input$designTrueTox2), arrival.rate = input$designArrivalRate2, 
                                    prop.B = input$designPropB2, cycle.length = input$designCycleLength2, 
                                    start.level = as.numeric(input$designStartLevel))
      
      designTCRM <- target.crm(prior = numerizer(input$designPriorTox), target.tox = input$designTargetTox2, 
                               number.trials = input$designNumTrials2, true.tox = numerizer(input$designTrueTox2), 
                               arrival.rate = input$designArrivalRate2, prop.B = input$designPropB2, target.crm = as.numeric(input$designTargetCRM), 
                               min.cohortB = input$designMinCohortB, cycle.length = input$designCycleLength2, 
                               cohort.size = as.numeric(input$designCohortSize), max.N = input$designMaxN, 
                               start.level = as.numeric(input$designStartLevel))
      
      return(list(designTPT, designTCRM))
    }
    
  })
  
  # DF w/ Both Designs for Plotly
  designDFPlotly <- reactive({
    req(input$designSelector == 3)
    
    p1df1 <- designDesign()[[1]]$df
    p1df1$Design <- "3+3"
    p1df1$prior <- NA
    p1df1$DoseLevel <- seq(1, nrow(p1df1))
    p1df2 <- designDesign()[[2]]$df
    p1df2$Design <- "TARGET-CRM"
    p1df2$DoseLevel <- seq(1, nrow(p1df2))
    p1df <- rbind(p1df1, p1df2)
    p1df$Design <- as.factor(p1df$Design)
    return(p1df)
    
  })
  
  # Plot1
  output$designPlotly1 <- renderPlotly({
    
    if(input$designSelector == 3){
      
      p1 <- designDFPlotly() %>% mutate(MTD.Prop=MTD.Freq/designDesign()[[1]]$number.trials) %>%
        ggplot(aes(x=DoseLevel, y=MTD.Prop, fill=Design, text=paste0("Dose Level: ", DoseLevel, "\n", "MTD Proportion: ", MTD.Prop, "\n", "Design: ", Design))) + 
        geom_bar(stat="identity", position="dodge") + xlab("Dose Level") + ylab("Proportion of Simulated Trials") +
        ggtitle("Proportion of Simulated Trials Selecting Each Dose Level as True MTD")
      
      ggplotly(p1, tooltip="text") %>% config(displayModeBar = FALSE)
    }
    
    else{
      
      p1 <- designDesign()$df %>% mutate(MTD.Prop = MTD.Freq/designDesign()$number.trials) %>%
        ggplot(aes(x=seq(1,length(MTD.Freq)), y=MTD.Prop, text=paste0("Dose Level: ", seq(1,length(MTD.Freq)), "\n", "MTD Proportion: ", MTD.Prop))) + 
        geom_bar(stat='identity') + xlab("Dose Level") + 
        ylab("Proportion of Simulated Trials") + ggtitle("Proportion of Simulated Trials Selecting Each Dose Level as True MTD")
      
      ggplotly(p1, tooltip="text") %>% config(displayModeBar = FALSE)
    }
    
  })
  
  # Plot2
  output$designPlotly2 <- renderPlotly({
    
    if (input$designSelector ==3){
      p2 <- designDFPlotly() %>%
        ggplot(aes(x=DoseLevel, y=obs.tox.table, fill=Design, text=paste("Dose Level: ", DoseLevel, "\n", "DLT Proportion: ", round(obs.tox.table, 4), "\n", "Design: ", Design))) + 
        geom_bar(stat="identity", position="dodge") + geom_hline(aes(yintercept=input$designTargetTox2), linetype="dashed") +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + 
        ggtitle("Proportion of Patients Experiencing a DLT Per Dose Level")
      
      ggplotly(p2, tooltip="text") %>% config(displayModeBar = FALSE)
    }
    
    else{
      p2 <- designDesign()$df %>%
        ggplot(aes(x=seq(1,length(MTD.Freq)), y=obs.tox.table, text=paste("Dose Level: ", seq(1,length(MTD.Freq)), "\n", "DLT Proportion: ", round(obs.tox.table, 4)))) + 
        geom_bar(stat="identity") + geom_hline(aes(yintercept=input$designTargetTox), linetype="dashed") +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + 
        ggtitle("Proportion of Patients Experiencing a DLT Per Dose Level")
      
      ggplotly(p2, tooltip="text") %>% config(displayModeBar = FALSE)
    }
  })
  
  # Plot3
  output$designPlotly3 <- renderPlotly({
    
    if (input$designSelector ==3){
      p3 <- designDFPlotly() %>%
        ggplot(aes(x=DoseLevel, y=patient.allocation.table, fill=Design, text=paste("Dose Level: ", DoseLevel, "\n", "Patient Allocation: ", round(patient.allocation.table, 4), "\n", "Design: ", Design))) + 
        geom_bar(stat="identity", position="dodge") + xlab("Dose Level") + ylab("Proportion of Patients Allocated") + 
        ggtitle("Proportion of Patients Allocated to Each Dose Level")
      
      ggplotly(p3, tooltip="text") %>% config(displayModeBar = FALSE)
    }
    
    else{
      p3 <- designDesign()$df %>%
        ggplot(aes(x=seq(1,length(MTD.Freq)), y=patient.allocation.table, text=paste("Dose Level: ", seq(1,length(MTD.Freq)), "\n", "Patient Allocation: ", round(patient.allocation.table, 4)))) + 
        geom_bar(stat="identity") + xlab("Dose Level") + ylab("Proportion of Patients Allocated") + 
        ggtitle("Proportion of Patients Allocated to Each Dose Level")
      
      ggplotly(p3, tooltip="text") %>% config(displayModeBar = FALSE)
    }
  })
  
  # Plot4
  output$designPlotly4 <- renderPlotly({
    
    if (input$designSelector ==3){
      
      p4df <- data.frame("meanDuration"=c(designDesign()[[1]]$mean.duration, designDesign()[[2]]$meanDuration), 
                         "sdDuration"=c(designDesign()[[1]]$sd.duration, designDesign()[[2]]$sd.duration), "Design"=c("3+3", "TARGET-CRM"))
      
      p4 <- p4df %>%
        ggplot(aes(x=Design, y=meanDuration, text=paste("Design: ", Design, "\n", "Mean Study Duration: ", round(meanDuration, 2), "\n", "SD Study Duration: ", round(sdDuration, 2)))) + 
        geom_bar(stat="identity") + geom_errorbar(aes(ymin=meanDuration-sdDuration, ymax=meanDuration+sdDuration)) + 
        ylab("Mean Study Duration (Days)") + ggtitle("Mean Study Duration in Days (+/- 1 SD)")
      
      ggplotly(p4, tooltip="text") %>% config(displayModeBar = FALSE)
    }
    
    else{
      
      p4df <- data.frame("meanDuration"=designDesign()$mean.duration, "sdDuration"=designDesign()$sd.duration, "Design"="x")
      
      p4 <- p4df %>%
        ggplot(aes(x=Design, y=meanDuration, text=paste("Mean Study Duration: ", round(meanDuration, 2), "\n", "SD Study Duration: ", round(sdDuration, 2)))) + 
        geom_bar(stat="identity") + geom_errorbar(aes(ymin=meanDuration-sdDuration, ymax=meanDuration+sdDuration)) + 
        ylab("Mean Study Duration (Days)") + ggtitle("Mean Study Duration in Days (+/- 1 SD)") + 
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
      
      ggplotly(p4, tooltip="text") %>% config(displayModeBar = FALSE)
    }
  })
}

shinyApp(ui, server)