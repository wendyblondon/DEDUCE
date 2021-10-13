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
source("funs.R")

theme_set(theme_minimal(base_size = 15))


ui <- dashboardPage(title = "DEDUCE", skin = "black",
                    dashboardHeader(title = strong("DEDUCE")),
                    dashboardSidebar(
                      useShinyjs(), includeCSS("www/style.css"), useShinyFeedback(), use_waiter(),
                                     sidebarMenu(id='tabs',
                                                 menuItem("Home", tabName = "Home"),
                                                 menuItem("Design", tabName = "Design"),
                                                 menuItem("Conduct", tabName = "Conduct"),
                                                 menuItem("Help", href="https://drive.google.com/file/d/18MGkaaIYFfJ5gqi1vGqnf7Myy0QjLs-i/view"),
                                                 menuItem("About", tabName = "About")
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                tags$script(HTML("$('body').addClass('fixed');")),
                                img(id="homeimg", src = "home.jpg"),
                                p(id="hometagline", "DEsign and conDUCt of dose Escalation trials (DEDUCE) - A unified resource for clinical investigators and statisticians to design and conduct more efficient 
                                  and accurate phase 1 trials."),
                                h2("Overview"),
                                p(id="overview", "The DEDUCE platform is an interactive, web-based resource to design and conduct 
                                  phase 1 dose escalation trials using rule-based and Bayesian adaptive designs. Our goal in developing this application is to raise 
                                  awareness, educate, and provide open access to investigators for alternative, improved methods and tools to design and conduct phase 
                                  1 dose escalation trials."),
                                h2("DEDUCE Modules:"),
                                tags$ul(
                                  tags$li(
                                    h4(class="trialdc", "Trial Design"),
                                    p("Users can specify and compare the operating characteristics for hypothetical phase 1 designs through trial simulations, 
                                    and select an optimal design for the needs of the trial.")
                                  ),
                                  tags$li(
                                    h4(class="trialdc", "Trial Conduct"),
                                    p("Users can implement the adaptive trial, and determine the recommended dose level each time a new patient enrolls.")
                                  )    
                                ),
                                
                                h2("Available Designs:"),
                                tags$ul(
                                  tags$li(
                                    p("Continual Reassessment Method (CRM) ", a(href="https://pubmed.ncbi.nlm.nih.gov/2350571/", "[O'Quigley et al. ", em("Biometrics,"), " 1990]", target="_blank", rel="noopener noreferrer"))
                                  ),
                                  tags$li(
                                    p("TARGETed-Agent Continual Reassessment Method (TARGET-CRM)")
                                  ), 
                                  tags$li(
                                    p("3+3 ", a(href="https://pubmed.ncbi.nlm.nih.gov/2790129/", "[Storer. ", em("Biometrics,"), " 1989]", target="_blank", rel="noopener noreferrer"))
                                  )     
                                ),
                                
                                h2("Key Features of DEDUCE:"),
                                tags$ul(
                                  tags$li(
                                    p("Permits simultaneous comparison of multiple trial designs for the same set of simulation parameters")
                                  ),
                                  tags$li(
                                    p("Dynamically generates a written report summarizing simulation results")
                                  )
                                ),
                                fluidRow(
                                  column(12, align="center",
                                         a(href="https://www.danafarberbostonchildrens.org", img(id="df_logo", src = "danafarber_bostonchildrens_logo.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer"),
                                         a(href="https://www.NorthwesternMutual.com", img(id="nm_logo", src = "NMLogo.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer"),
                                         a(href="https://hms.harvard.edu/", img(id="hms_logo", src = "HMS.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer")
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "Design",
                                fluidRow(
                                  column(3, style="overflow-y:scroll; height: 70vh;",
                                         h1("Inputs", style="text-align: center; text-decoration: underline;"),
                                         br(),
                                         p("Designs:", style = "font-weight: 700; font-size: 18px;"),
                                         prettyCheckbox("dt_selector_tpt", "3+3", value = TRUE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("dt_selector_tpt", "Select to run the 3+3 Design", 
                                                   "top", options = list(container = "body")),
                                         prettyCheckbox("dt_selector_crm", "CRM", value = FALSE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("dt_selector_crm", "Select to run the CRM Design", 
                                                   "top", options = list(container = "body")),
                                         prettyCheckbox("dt_selector_tcrm", "TARGET-CRM", value = FALSE, icon = icon("check"), shape = "round", animation = "jelly", inline = TRUE),
                                         bsTooltip("dt_selector_tcrm", "Select to run the TARGET-CRM Design", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("dt_num_doses", "Number of Dose Levels", min = 3, max = 10, value = 4, width = "100%", ticks = FALSE),
                                         bsTooltip("dt_num_doses", "Please enter the number of doses that will be used", 
                                                   "top", options = list(container = "body")),
                                         textInput("dt_dose_labels", "Dose Level Labels", value = "-1,1,2,3", width = "100%"),
                                         bsTooltip("dt_dose_labels", "Please enter the dose level labels (separated by commas) for each dose level evaluated in the trial", 
                                                   "top", options = list(container = "body")),
                                         selectInput("dt_start_level", "Starting Dose Level", choices = c(-1,1,2,3), selected = 1, width = "100%"),
                                         bsTooltip("dt_start_level", "Please enter the starting dose level from the dose level labels above", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("dt_num_trials", "Number of Simulated Trials", min = 0, max = 10000, value = 100, width = "100%", step = 100, ticks = FALSE),
                                         bsTooltip("dt_num_trials", "Please enter the number of simulated trials. A larger number of simulations increases the precision of simulation results and computation time", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("dt_target_tox", "Target Toxicity Probability", min = 0, max = 1, value = 0.2, step = 0.01, width = "100%", ticks = FALSE),
                                         bsTooltip("dt_target_tox", "Please enter the target toxicity probability of the study agent", 
                                                   "top", options = list(container = "body")),
                                         textInput("dt_true_tox", "True Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3", width = "100%"),
                                         bsTooltip("dt_true_tox", "Please enter the true toxicity probabilities for each dose level (separated by commas). Toxicity probabilities must increase with each subsequent dose level", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("dt_arrival_rate", "Average Time Between Patient Enrollments (In Days)", min = 0, max = 180, value = 15, width = "100%", ticks = FALSE),
                                         bsTooltip("dt_arrival_rate", "Please enter the average time between enrolling patients (In Days)", 
                                                   "top", options = list(container = "body")),
                                         sliderInput("dt_cycle_length", "Duration of DLT Observation Period (In Days)", min = 0, max = 365, value = 28, width = "100%", ticks = FALSE),
                                         bsTooltip("dt_cycle_length", "Please enter the duration of the DLT observation period (In Days)", 
                                                   "top", options = list(container = "body")),
                                         
                                         conditionalPanel(condition = "input.dt_selector_tcrm == 1 || input.dt_selector_crm == 1",
                                           textInput("dt_prior_tox", "Prior Toxicity Probability Vector", value = "0.05,0.12,0.2,0.3", width = "100%"),
                                           bsTooltip("dt_prior_tox", "Please enter the prior toxicity probabilities for each dose level (separated by commas). Toxicity probabilities must increase with each subsequent dose level", 
                                                     "top", options = list(container = "body")),
                                           sliderInput("dt_max_n", "Maximum Sample Size", min = 1, max = 200, value = 18, width = "100%", ticks = FALSE),
                                           bsTooltip("dt_max_n", "Please enter the maximum number of patients to be enrolled per trial", 
                                                     "top", options = list(container = "body")),
                                           sliderInput("dt_cohort_size", "Cohort Size", min = 1, max = 9, value = 3, width = "100%", ticks = FALSE),
                                           bsTooltip("dt_cohort_size", "Please enter the cohort size. The cohort size is the number of patients to be treated at the current dose level before a dose escalation decision is made", 
                                                     "top", options = list(container = "body"))
                                         ),
                                         
                                         conditionalPanel(condition = "input.dt_selector_tcrm == 1",
                                            sliderInput("dt_prop_b", "Proportion of Patients from Cohort B", min = 0, max = 1, value = 0.1, step = 0.01, width = "100%", ticks = FALSE),
                                            bsTooltip("dt_prop_b", "Patients belong to either Cohort A (general enrollment) or Cohort B (enrichment cohort). Please enter the proportion of enrolled patients belonging to Cohort B. Enter a proportion of 0 if no enrichment cohort is needed.", 
                                                                    "top", options = list(container = "body")),
                                            sliderInput("dt_min_cohort_b", "Minimum Enrollment of Cohort B Patients (Optional)", min = 0, max = 100, value = 0, width = "100%", ticks = FALSE),
                                            bsTooltip("dt_min_cohort_b", "An optional feature is to require a trial to enroll a minimum number of Cohort B patients. Once the maximum N is attained, enrollment of Cohort A patients will be suspended and only Cohort B patients may enroll until the minimum number has been attained. Please enter the minimum number of Cohort B patients to be enrolled in a trial. Enter 0 if no minimum number is required.", 
                                                      "top", options = list(container = "body"))
                                         ),
                                         splitLayout(cellWidths = c("50%", "25%", "25%"),
                                                     actionButton("dt_simulate", "Simulate", width = "100%", style = "font-weight: bold;"),
                                                     downloadButton("dt_results", "", style = "font-weight: bold; width: 100%;"),
                                                     actionButton("dt_reset", "Reset", width = "100%", style = "font-weight: bold;")
                                         ),
                                         bsTooltip("dt_simulate", "Simulates the selected design(s) using the values of the above inputs", 
                                                   "top", options = list(container = "body")),
                                         bsTooltip("dt_results", "Download the full report of plots, tables, and summaries", 
                                                   "top", options = list(container = "body")),
                                         bsTooltip("dt_reset", "WARNING: Resets all of the inputs and results, cannot be undone", 
                                                   "top", options = list(container = "body"))
                                  ),
                                  column(9,
                                         uiOutput("dt_plots_ui"),
                                         uiOutput("dt_none_ui")
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
                                  tags$li(
                                    p("Clement Ma, PhD")
                                  ),
                                  tags$li(
                                    p("Wendy B. London, PhD")
                                  )
                                ),
                                h2("Development Team: Northwestern Mutual"),
                                tags$ul(
                                  tags$li(
                                    p("Judy Berdan")
                                  ),
                                  tags$li(
                                    p("Laure Borchardt")
                                  ),
                                  tags$li(
                                    p("Audra Brennan")
                                  ),
                                  tags$li(
                                    p("Stan Crane")
                                  ),
                                  tags$li(
                                    p("Ben Garski")
                                  ),
                                  tags$li(
                                    p("Nanette Jamel")
                                  ),
                                  tags$li(
                                    p("Lori Kiraly")
                                  ),
                                  tags$li(
                                    p("Danielle Pankey")
                                  ),
                                  tags$li(
                                    p("Susan Stegman, MD")
                                  )
                                ),
                                h2("Contact:"),
                                tags$ul(
                                  tags$li(
                                    p("For Assistance Please Contact: Drs. Clement Ma and Wendy B. London")
                                  )
                                ),
                                h2("Citation:"),
                                tags$ul(
                                  tags$li(
                                    p("To site DEDUCE please use: Insert link to citation")
                                  )
                                ),
                                h2("Acknowledgements:"),
                                p(id="acknowledgements", "We would like to thank the Northwestern Mutual Tech for Good team for their pro-bono development, design, and project management 
                                support for the DEDUCE platform. We would also like to thank our test users, Drs. Steven G. DuBois, Karen D. Wright, 
                                and David S. Shulman for their helpful feedback."),
                                h2("References:"),
                                tags$ul(
                                  tags$li(
                                    p(
                                      a("Storer BE. Design and Analysis of Phase I Clinical Trials. ", em("Biometrics. "), "1989;45(3):925-37.",
                                        href="https://pubmed.ncbi.nlm.nih.gov/2790129/", target="_blank", rel="noopener noreferrer")
                                    )
                                  ),
                                  tags$li(
                                    p(
                                      a("O'Quigley J, Pepe M, Fisher L. Continual reassessment method: a practical design for phase 1 Clinical trials in cancer. ", 
                                        em("Biometrics. "), "1990;46(1):33-48.", href="https://pubmed.ncbi.nlm.nih.gov/2350571/", 
                                        target="_blank", rel="noopener noreferrer")
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(12, align="center",
                                         a(href="https://www.danafarberbostonchildrens.org", img(id="df_logo", src = "danafarber_bostonchildrens_logo.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer"),
                                         a(href="https://www.NorthwesternMutual.com", img(id="nm_logo", src = "NMLogo.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer"),
                                         a(href="https://hms.harvard.edu/", img(id="hms_logo", src = "HMS.png", style="cursor: pointer;"), target="_blank", rel="noopener noreferrer")
                                  )
                                )
                                
                        )
                      )
                    )
)

server <- function(input, output, session) {
  
  # Disable Results Button Until Design(s) Ran
  disable("dt_results")
  
  # Get the Design Names That Are Selected
  dt_selected_design_names <- reactive({
    designInputs(c(input$dt_selector_tpt, input$dt_selector_tcrm, input$dt_selector_crm))
  })
  
  # Update Start Level Based on Dose Labels
  observe({
    updateSelectInput(session, "dt_start_level", choices = unlist(strsplit(input$dt_dose_labels, ",")), selected = unlist(strsplit(input$dt_dose_labels, ","))[2])
  })
  
  # Update Max Depending on Previous Input
  observe({
    updateSliderInput(session, "dt_min_cohort_b", max = input$dt_max_n)
  })
  
  ## Warnings for Invalid Inputs
  
  # Dose Labels
  observeEvent(list(input$dt_dose_labels, input$dt_num_doses), {
    req(input$dt_dose_labels)
    hideFeedback("dt_dose_labels")
    if (length(unlist(strsplit(input$dt_dose_labels, ",")))!= input$dt_num_doses) {
      showFeedbackDanger("dt_dose_labels", "The length must match the number of dose levels selected above. Be sure to use commas to separate each label.")
    }
  })
  
  
  # True Tox
  observeEvent(list(input$dt_true_tox, input$dt_num_doses), {
    req(input$dt_true_tox)
    hideFeedback("dt_true_tox")
    if (length(unlist(strsplit(input$dt_true_tox, ",")))!= input$dt_num_doses) {
      showFeedbackDanger("dt_true_tox", "The length must match the number of dose levels selected at the top. Be sure to use commas to separate each decimal.")
    }
    else if (incrementCheck(input$dt_true_tox)==FALSE) {
      showFeedbackDanger("dt_true_tox", "The probabilities must increase with each subsequent dose")
    }
    else if (decimalCheck(input$dt_true_tox)==FALSE) {
      showFeedbackDanger("dt_true_tox", "The probabilities must be a decimal")
    }
  })
  
  # Prior Tox
  observeEvent(list(input$dt_prior_tox, input$dt_num_doses), {
    req(input$dt_prior_tox)
    hideFeedback("dt_prior_tox")
    if (length(unlist(strsplit(input$dt_prior_tox, ",")))!= input$dt_num_doses) {
      showFeedbackDanger("dt_prior_tox", "The length must match the number of dose levels selected at the top. Be sure to use commas to separate each decimal.")
    }
    else if (incrementCheck(input$dt_prior_tox)==FALSE) {
      showFeedbackDanger("dt_prior_tox", "The probabilities must increase with each subsequent dose")
    }
    else if (decimalCheck(input$dt_prior_tox)==FALSE) {
      showFeedbackDanger("dt_prior_tox", "The probabilities must be a decimal")
    }
  })
  
  # Main Plotting UI
  output$dt_plots_ui <- renderUI({
    req(length(dt_selected_design_names()) > 0)
    hidden(
      div(id="dt_ui_plots",
          column(6,
                 plotOutput("dt_plot_1"),
                 br(),
                 plotOutput("dt_plot_2")
          ),
          column(6,
                 plotOutput("dt_plot_3"),
                 br(),
                 plotOutput("dt_plot_4")
          )
      )
    )
  })
  
  # Shows the Plots UI After Clicking Simulate
  observeEvent(input$dt_simulate, {
    show("dt_ui_plots")
  })
  
  # Hide/Reset the UI Elements
  observeEvent(input$dt_reset, {
    hide("dt_ui_plots")
    reset("dt_selector_tpt")
    reset("dt_selector_tcrm")
    reset("dt_selector_crm")
    reset("dt_num_doses")
    reset("dt_dose_labels")
    reset("dt_start_level")
    reset("dt_num_trials")
    reset("dt_target_tox")
    reset("dt_true_tox")
    reset("dt_arrival_rate")
    reset("dt_prop_b")
    reset("dt_cycle_length")
    reset("dt_prior_tox")
    reset("dt_max_n")
    reset("dt_min_cohort_b")
    reset("dt_cohort_size")
    disable("dt_results")
  })
  
  # UI if No Design Selected
  output$dt_none_ui <- renderUI({
    req(length(dt_selected_design_names()) == 0)
    tagList(
      fluidRow(
        h2("Please select a design to begin", style="color: #ff0033")
      )
    )
  })
  
  # Disable Simulate Button if No Designs Selected
  observe({
    if(input$dt_selector_tpt == 0 & input$dt_selector_tcrm == 0 & input$dt_selector_crm == 0){
      disable("dt_simulate")
    }
    else{
      enable("dt_simulate")
    }
  })
  
  # Running the Design(s)
  dt_function_outputs <- eventReactive(input$dt_simulate, {
    w <- Waiter$new(html = spin_heartbeat(), color = "black")
    w$show()
    
    # 3+3
    if (input$dt_selector_tpt == TRUE) {
      
      tpt <- three_plus_three(target_tox = input$dt_target_tox, number_trials = input$dt_num_trials, 
                              true_tox = numerizer(input$dt_true_tox), arrival_rate = input$dt_arrival_rate, cycle_length = input$dt_cycle_length, 
                              start_level = match(input$dt_start_level, unlist(strsplit(input$dt_dose_labels, ","))))
    }
    
    # TARGET-CRM
    if(input$dt_selector_tcrm == TRUE) {
      
      tcrm <- my_target_crm(prior = numerizer(input$dt_prior_tox), target_tox = input$dt_target_tox, 
                            number_trials = input$dt_num_trials, true_tox = numerizer(input$dt_true_tox), 
                            arrival_rate = input$dt_arrival_rate, prop_b = input$dt_prop_b, min_cohort_b = input$dt_min_cohort_b, cycle_length = input$dt_cycle_length, 
                            cohort_size = input$dt_cohort_size, max_n = input$dt_max_n, start_level = match(input$dt_start_level, unlist(strsplit(input$dt_dose_labels, ","))))
      
    }
    
    # Other
    if(input$dt_selector_crm == TRUE) {
      
      crm <- my_crm(prior = numerizer(input$dt_prior_tox), target_tox = input$dt_target_tox, 
                    number_trials = input$dt_num_trials, true_tox = numerizer(input$dt_true_tox), 
                    arrival_rate = input$dt_arrival_rate, min_cohort_b = input$dt_min_cohort_b, cycle_length = input$dt_cycle_length, 
                    cohort_size = input$dt_cohort_size, max_n = input$dt_max_n, start_level = match(input$dt_start_level, unlist(strsplit(input$dt_dose_labels, ","))))
    }
    
    all <- list(get0("tpt"), get0("tcrm"), get0("crm"))
    w$hide()
    return(all[lengths(all) != 0])
    
    
  })
  
  # DF for Design Results Used in Report
  dt_results_df <- reactive({
    req(dt_function_outputs())
    fun_list <- list()
    
    for (v in seq(1, length(dt_selected_design_names()))) {
      df <- data.frame("design"=dt_selected_design_names()[v], "pcs"=dt_function_outputs()[[v]]$pcs, "true_mtd"=dt_function_outputs()[[v]]$true_mtd, 
                       "obs_tox"=dt_function_outputs()[[v]]$obs_tox_overall, "target_tox"=dt_function_outputs()[[v]]$target_tox, 
                       "patmtd"=dt_function_outputs()[[v]]$patient_allocation_table[dt_function_outputs()[[v]]$true_mtd], 
                       "mean_duration"=dt_function_outputs()[[v]]$mean_duration, "sd_duration"=dt_function_outputs()[[v]]$sd_duration, 
                       "mean_obs_n"=dt_function_outputs()[[v]]$mean_obs_n, "min_obs_n"=dt_function_outputs()[[v]]$min_obs_n, "max_obs_n"=dt_function_outputs()[[v]]$max_obs_n, 
                       "prop_b"=dt_function_outputs()[[v]]$prop_b, "mean_cohort_b"=dt_function_outputs()[[v]]$mean_cohort_b, "sd_cohort_b"=dt_function_outputs()[[v]]$sd_cohort_b)
      fun_list[[v]] <- df
    }
    
    finaldf <- bind_rows(fun_list)
    return(finaldf)
  })
  
  # DF for Plots
  dt_plot_df <- reactive({
    
    # Multiple Designs Selected
    if (length(dt_selected_design_names()) > 1) {
      fun_length <- length(dt_selected_design_names())
      fun_list <- list()
      
      for (v in seq(1, fun_length)) {
        df <- dt_function_outputs()[[v]]$df
        fun_list[[v]] <- df
      }
      
      finaldf <- bind_rows(fun_list)
      finaldf$dose_level <- factor(unlist(strsplit(input$dt_dose_labels, ",")), levels=unlist(strsplit(input$dt_dose_labels, ",")))
      finaldf$design <- as.factor(finaldf$design)
      finaldf$dose_num <- rep(seq(1, length(unlist(strsplit(input$dt_dose_labels, ",")))), length(dt_selected_design_names()))
      
      return(finaldf)
    }
    
    # Only 1 Design Selected
    else if(length(dt_selected_design_names()) == 1){
      
      df <- dt_function_outputs()[[1]]$df
      df$dose_level <- factor(unlist(strsplit(input$dt_dose_labels, ",")), levels=unlist(strsplit(input$dt_dose_labels, ",")))
      df$design <- as.factor(df$design)
      df$dose_num <- seq(1, length(unlist(strsplit(input$dt_dose_labels, ","))))
      return(df)
    }
  })
  
  # DF2 for Plot
  dt_plot_df2 <- reactive({
    fun_length <- length(dt_selected_design_names())
    fun_list <- list()
    
    for (v in seq(1, fun_length)) {
      df <- data.frame("design"=dt_function_outputs()[[v]]$df$design[1], "mean_duration"=dt_function_outputs()[[v]]$mean_duration, 
                       "sd_duration"=dt_function_outputs()[[v]]$sd_duration)
      fun_list[[v]] <- df
      
    }
    finaldf <- bind_rows(fun_list)
    finaldf$design <- as.factor(finaldf$design)
    return(finaldf)
  })
  
  
  
  # Plot1
  dt_plot_1 <- reactive({
    if(length(dt_selected_design_names()) > 1){
      ggplot() + 
        geom_bar(data = dt_plot_df() %>% 
                   mutate(mtd_prop=mtd.Freq/input$dt_num_trials), aes(x=dose_level, y=mtd_prop, fill=design), stat="identity", position="dodge") + 
        geom_bar(data = dt_plot_df() %>% 
                   mutate(mtd_prop=mtd.Freq/input$dt_num_trials) %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=mtd_prop, fill=design, color=as.factor(true_mtd)), stat="identity", position="dodge", size=2) +
        xlab("Dose Level") + ylab("Proportion of Simulated Trials") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Simulated Trials Selecting\nEach Dose Level as True MTD") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(dt_selected_design_names()) == 1){
      ggplot() + 
        geom_bar(data=dt_plot_df() %>% 
                   mutate(mtd_prop = mtd.Freq/input$dt_num_trials), aes(x=dose_level, y=mtd_prop), stat='identity', fill="#BEBEBE") + 
        geom_bar(data=dt_plot_df() %>% 
                   mutate(mtd_prop = mtd.Freq/input$dt_num_trials) %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=mtd_prop, color=as.factor(true_mtd)), stat="identity", fill="#BEBEBE", size=2) +
        xlab("Dose Level") + ylab("Proportion of Simulated Trials") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Simulated Trials Selecting\nEach Dose Level as True MTD") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  output$dt_plot_1 <- renderPlot({
    dt_plot_1()
  })
  
  # Plot2
  dt_plot_2 <- reactive({
    if (length(dt_selected_design_names()) > 1){
      ggplot() + 
        geom_bar(data = dt_plot_df(), aes(x=dose_level, y=obs_tox_table, fill=design), stat="identity", position="dodge") + 
        geom_bar(data = dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=obs_tox_table, fill=design, color=as.factor(true_mtd)), stat="identity", position="dodge", size=2) +
        geom_hline(aes(yintercept=input$dt_target_tox), linetype="dashed") + guides(color = FALSE) +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(dt_selected_design_names()) == 1){
      ggplot() + 
        geom_bar(data = dt_plot_df(), aes(x=dose_level, y=obs_tox_table), stat="identity", position="dodge", fill="#BEBEBE") + 
        geom_bar(data = dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=obs_tox_table, color=as.factor(true_mtd)), stat="identity", position="dodge", fill="#BEBEBE", size=2) +
        geom_hline(aes(yintercept=input$dt_target_tox), linetype="dashed") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        xlab("Dose Level") + ylab("Proportion of Patients Experiencing a DLT ") + ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + 
        theme(plot.title = element_text(hjust = 0.5)) + guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  
  output$dt_plot_2 <- renderPlot({
    dt_plot_2()
  })
  
  # Plot3
  dt_plot_3 <- reactive({
    if (length(dt_selected_design_names()) > 1){
      ggplot() + 
        geom_bar(data=dt_plot_df(), aes(x=dose_level, y=patient_allocation_table, fill=design), stat="identity", position="dodge") +
        geom_bar(data=dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=patient_allocation_table, fill=design, color=as.factor(true_mtd)), stat="identity", position="dodge", size=2) +
        xlab("Dose Level") + ylab("Proportion of Patients Allocated") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(dt_selected_design_names()) == 1){
      ggplot() + 
        geom_bar(data=dt_plot_df(), aes(x=dose_level, y=patient_allocation_table), stat="identity", position="dodge", fill="#BEBEBE") +
        geom_bar(data=dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=patient_allocation_table, color=as.factor(true_mtd)), stat="identity", position="dodge", fill="#BEBEBE", size=2) +
        xlab("Dose Level") + ylab("Proportion of Patients Allocated") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) + 
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5)) + 
        guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  
  output$dt_plot_3 <- renderPlot({
    dt_plot_3()
    
  })
  
  # Plot4
  dt_plot_4 <- reactive({
    dt_plot_df2() %>%
      ggplot(aes(x=design, y=mean_duration)) + 
      geom_point(size = 5) + geom_errorbar(aes(ymin= mean_duration - sd_duration, ymax = mean_duration + sd_duration), width = 0.3) + xlab("Design") +
      ylab("Mean Study Duration (Days)") + ggtitle("Mean Study Duration in Days (+/- 1 SD)") + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$dt_plot_4 <- renderPlot({
    dt_plot_4()
  })
  
  # Table DF
  dt_table_1_df <- reactive({
    
    table_list <- list()
    
    for (v in seq(1, length(dt_selected_design_names()))) {
      if (dt_function_outputs()[[v]]$df$design[1] == "3+3") {
        
        x <- round(unname(c(nullToNA(dt_function_outputs()[[v]]$pcs), nullToNA(dt_function_outputs()[[v]]$true_mtd), 
                            dt_function_outputs()[[v]]$mtd_selection_table/dt_function_outputs()[[v]]$number_trials, 
                            nullToNA(dt_function_outputs()[[v]]$obs_tox_overall), dt_function_outputs()[[v]]$obs_tox_table, 
                            nullToNA(dt_function_outputs()[[v]]$mean_obs_n), nullToNA(dt_function_outputs()[[v]]$min_obs_n), 
                            nullToNA(dt_function_outputs()[[v]]$max_obs_n), dt_function_outputs()[[v]]$patient_allocation_table, 
                            dt_function_outputs()[[v]]$mean_duration, dt_function_outputs()[[v]]$sd_duration, NA, NA)), 3)
        x_name <- dt_function_outputs()[[v]]$df$design[1]
      }
      else {
        x <- round(unname(c(nullToNA(dt_function_outputs()[[v]]$pcs), nullToNA(dt_function_outputs()[[v]]$true_mtd), 
                            dt_function_outputs()[[v]]$mtd_selection_table/dt_function_outputs()[[v]]$number_trials, 
                            nullToNA(dt_function_outputs()[[v]]$obs_tox_overall), dt_function_outputs()[[v]]$obs_tox_table, 
                            nullToNA(dt_function_outputs()[[v]]$mean_obs_n), nullToNA(dt_function_outputs()[[v]]$min_obs_n), 
                            nullToNA(dt_function_outputs()[[v]]$max_obs_n), dt_function_outputs()[[v]]$patient_allocation_table, 
                            dt_function_outputs()[[v]]$mean_duration, dt_function_outputs()[[v]]$sd_duration, 
                            nullToNA(dt_function_outputs()[[v]]$mean_cohort_b), nullToNA(dt_function_outputs()[[v]]$sd_cohort_b))), 3)
        x_name <- dt_function_outputs()[[v]]$df$design[1]
      }
      
      table_list[[v]] <- x
      names(table_list)[v] <- x_name
      
    }
    df <- as.data.frame(do.call(cbind, table_list))
    op_chars <- c("Proportion of correct selection (PCS)", "True MTD", sprintf("Proportion of trials selecting dose %s as true MTD", unlist(strsplit(input$dt_dose_labels, ","))),
                 "Proportion of patients experiencing a DLT overall", sprintf("Proportion of patients experiencing a DLT at dose %s", unlist(strsplit(input$dt_dose_labels, ","))),
                 "Mean total sample size", "Minimmum total sample size", "Maximum total sample size", 
                 sprintf("Proportion of patients enrolled at dose %s", unlist(strsplit(input$dt_dose_labels, ","))), "Mean study duration in days", 
                 "Standard deviation of study duration in days", "Mean # of cohort B patients enrolled during DTL observation period (TARGET-CRM only)",
                 "Standard deviation of # of cohort B patients enrolled during DLT observation period (TARGET-CRM only)")
    df <- cbind(op_chars, df)
    colnames(df)[1] <- "Operating Characteristic"
    return(df)
  })
  
  # Values for Rmd - Methods Section
  dt_report_methods <- reactive({
    req(input$dt_simulate > 0)
    
    if (input$dt_selector_tcrm == 1 | input$dt_selector_crm == 1) {
      
      x1 <- input$dt_num_trials
      x2 <- input$dt_num_doses
      x3 <- input$dt_dose_labels
      x4 <- input$dt_start_level
      x5 <- input$dt_true_tox
      x6 <- input$dt_target_tox
      x7 <- input$dt_arrival_rate
      x8 <- input$dt_cycle_length
      x9 <- input$dt_prior_tox
      x10 <- input$dt_cohort_size
      x11 <- input$dt_max_n
      x12 <- input$dt_prop_b
      x13 <- input$dt_min_cohort_b
      return(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13))
      
    }
    else{
      x1 <- input$dt_num_trials
      x2 <- input$dt_num_doses
      x3 <- input$dt_dose_labels
      x4 <- input$dt_start_level
      x5 <- input$dt_true_tox
      x6 <- input$dt_target_tox
      x7 <- input$dt_arrival_rate
      x8 <- input$dt_cycle_length
      return(c(x1,x2,x3,x4,x5,x6,x7,x8))
      
    }
  })
  
  # Values for Rmd - Results Section
  dt_report_results <- reactive({
    
      # 1 Design
      if (length(dt_selected_design_names()) == 1) {
        x1 <- dt_results_df()$pcs
        x2 <- round(dt_results_df()$obs_tox, 2)
        x3 <- dt_results_df()$target_tox
        x4 <- ifelse(x2 > x3, "greater than", ifelse(x2 < x3, "lower than", "equal to"))
        x5 <- unlist(strsplit(input$dt_dose_labels, ","))[dt_results_df()$true_mtd]
        x6 <- round(dt_results_df()$patmtd, 2)
        x7 <- round(dt_results_df()$mean_duration, 2)
        x8 <- round(dt_results_df()$sd_duration, 2)
        x9 <- dt_results_df()$mean_obs_n
        x10 <- dt_results_df()$min_obs_n
        x11 <- dt_results_df()$max_obs_n
        
        # Only Needed for TARGET-CRM
        x12 <- dt_results_df()$prop_b
        x13 <- dt_results_df()$mean_cohort_b
        x14 <- dt_results_df()$sd_cohort_b
        return(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14))
      }
    
    # 2+ Designs
    else{
      x1 <- dt_results_df() %>% slice_max(pcs) %>% select(design) %>% pull()
      x2 <- unlist(strsplit(input$dt_dose_labels, ","))[dt_results_df()$true_mtd[1]]
      x3 <- paste(sprintf("The proportion of correct selection (PCS) of the MTD for the %s design is %g.", dt_results_df()$design, dt_results_df()$pcs), collapse = " ")
      x4 <- paste(sprintf("The proportion of patients experiencing a DLT for the %s design is %g, which %s the target toxicity probability of %g.", 
                    dt_results_df()$design, dt_results_df()$obs_tox, ifelse(dt_results_df()$obs_tox > dt_results_df()$target_tox, "is greater than", 
                    ifelse(dt_results_df()$obs_tox == dt_results_df()$target_tox, "equals", "is lower than")), dt_results_df()$target_tox[1]), collapse = " ")
      x5 <- dt_results_df() %>% slice_max(patmtd) %>% select(design) %>% pull()
      x6 <- paste(sprintf("The proportion of patients assigned to the true MTD for the %s design is %g.", dt_results_df()$design, dt_results_df()$patmtd), collapse = " ")
      x7 <- dt_results_df() %>% slice_min(mean_duration) %>% select(design) %>% pull()
      x8 <- paste(sprintf("The mean study duration for the %s design is %g days(SD=%g).", dt_results_df()$design, dt_results_df()$mean_duration, 
                          dt_results_df()$sd_duration), collapse = " ")
      x9 <- paste(sprintf("The mean total sample size for the %s design is %g (range=%g-%g).", dt_results_df()$design, 
                    dt_results_df()$mean_obs_n, dt_results_df()$min_obs_n, dt_results_df()$max_obs_n), collapse = " ")
      
      # Only Needed for TARGET-CRM
      x10 <- dt_results_df()$prop_b[1]
      x11 <- ifelse(nrow(dt_results_df() %>% filter(design == 'TARGET-CRM'))==0, NA, 
                    dt_results_df() %>% filter(design == 'TARGET-CRM') %>% select(mean_cohort_b) %>% pull())
      x12 <- ifelse(nrow(dt_results_df() %>% filter(design == 'TARGET-CRM'))==0, NA, 
                    dt_results_df() %>% filter(design == 'TARGET-CRM') %>% select(sd_cohort_b) %>% pull())
      
      return(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12))
    } 
  })
  
  # Observer to Activate Download Button
  observe({
    if (input$dt_simulate > 0) {
      enable("dt_results")
    }
  })
  
  # Download Results
  output$dt_results <- downloadHandler(
    filename = function(){
      paste0("DELPHI Results ", Sys.time(), ".docx")
    },
    content = function(file){
      temp_report <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", temp_report, overwrite = TRUE)
      
      params <- list(d = dt_selected_design_names(), m = dt_report_methods(), r = dt_report_results(), 
                     p1 = dt_plot_1(), p2 = dt_plot_2(), p3 = dt_plot_3(), p4 = dt_plot_4(), t = dt_table_1_df())
      
      render(temp_report, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
