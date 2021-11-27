library(shiny)
library(bslib)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(waiter)
library(dplyr)
library(ggplot2)
library(rmarkdown)
library(DT)

# Adding External Files
source("crm.R")
source("target_crm.R")
source("three_plus_three.R")
source("target_crm_conduct.R")
source("funs.R")

# Set ggplot2 Default Font Size
theme_set(theme_minimal(base_size = 15))

# UI ---------------------
ui <- page_navbar(title = "DEDUCE", theme = bs_theme(version = 3), bg = "white",
  
  ## Home Tab ---------------------               
  nav("HOME",
    useShinyjs(), includeCSS("www/style.css"), useShinyFeedback(), use_waiter(),
    img(id = "homeimg", src = "home.jpg"),
    fluidRow(class = "text-body",
      column(12,
        p(id = "hometagline", 
          "DEsign and conDUCt of dose Escalation trials (DEDUCE)"
        ),
        p(id = "hometagdesc", 
          "A unified resource for clinical investigators and statisticians to", br(),
          "design and conduct more efficient and accurate phase 1 trials."
        ),
        h2("Overview"),
        p(class = "main-text",
          "The DEDUCE platform is an interactive, web-based resource to design and conduct 
            phase 1 dose escalation trials using rule-based and Bayesian adaptive designs. 
            Our goal in developing this application is to raise awareness, educate, 
            and provide open access to investigators for alternative, improved methods and tools 
            to design and conduct phase 1 dose escalation trials."
        ),
        br(),
        h2("DEDUCE Modules"),
        p(class = "main-text", "DESIGN YOUR TRIAL"),
        p(class = "main-text",
          "Users can specify and compare the operating characteristics for hypothetical phase 1 designs 
            through trial simulations, and select an optimal design for the needs of the trial."
        ),
        br(),
        p(class = "main-text", "CONDUCT YOUR TRIAL"),
        p(class = "main-text", "Users can implement the adaptive trial, and determine the recommended dose level each time a new patient enrolls."),
        br(),
        h2("Available Designs"),
        tags$ul(
          tags$li(
            p(class = "main-text", "Continual Reassessment Method (CRM) ",
              a(
                href="https://pubmed.ncbi.nlm.nih.gov/2350571/", "[O'Quigley et al. ", em("Biometrics,"), " 1990]", 
                target="_blank", rel="noopener noreferrer"
              )
            )
          ),
          tags$li(
            p(class = "main-text", "TARGETed-Agent Continual Reassessment Method (TARGET-CRM)")
          ), 
          tags$li(
            p(class = "main-text", "3+3 ", 
              a(
                href="https://pubmed.ncbi.nlm.nih.gov/2790129/", "[Storer. ", em("Biometrics,"), " 1989]", 
                target="_blank", rel="noopener noreferrer"
              )
            )
          )     
        ),
        br(),
        h2("Key Features of DEDUCE"),
        tags$ul(
          tags$li(
            p(class = "main-text", "Permits simultaneous comparison of multiple trial designs for the same set of simulation parameters")
          ),
          tags$li(
            p(class = "main-text", "Dynamically generates a written report summarizing simulation results")
          )
        )
      )
    ),
    fluidRow(class = "footer",
      column(12,
        splitLayout(
          a(
            href="https://www.danafarberbostonchildrens.org",
            img(id="df_logo", src = "dfbc.png"),
            target="_blank", rel="noopener noreferrer"
          ),
          a(
            href="https://www.NorthwesternMutual.com",
            img(id="nm_logo", src = "nm.png"),
            target="_blank", rel="noopener noreferrer"
          )
        )
      )
    )
  ),
  
  ## Design Tab ---------------------
  nav("DESIGN YOUR TRIAL",
    div(class = "other_tabs",
      fluidRow(
        column(3, style="overflow-y:scroll; height: 80vh;",
          h3("Inputs", style="text-align: center;"),
          br(),
          
          ### Main Inputs ---------------------
          
          #### Designs ---------------------
          checkboxGroupInput(
            "dt_selectors",
            div(p(class = "help-p", "Designs"), HTML('<button id="dt-designs-help" class="help-btn" type="button">?</button>'), style = "font-weight: bold; font-size: 18px;"),
            c("3+3", "CRM", "TARGET-CRM"), selected = "3+3", inline = TRUE
          ),
          bsPopover(
            "dt-designs-help", "",
            "Select the design(s) to run in the model",
            placement = "top", trigger = "focus"
          ),
          
          #### Number of Doses ---------------------
          sliderInput(
            "dt_num_doses",
            div(p(class = "help-p", "Number of Dose Levels"), HTML('<button id="dt-num-doses-help" class="help-btn" type="button">?</button>')),
            min = 3, max = 10, value = 4, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "dt-num-doses-help", "",
            "Please enter the number of dose levels in your trial",
            placement = "top", trigger = "focus"
          ),
          
          #### Dose Labels ---------------------
          textInput(
            "dt_dose_labels",
            div(p(class = "help-p", "Dose Level Labels"), HTML('<button id="dt-dose-labels-help" class="help-btn" type="button">?</button>')), 
            value = "-1,1,2,3", width = "100%"
          ),
          bsPopover(
            "dt-dose-labels-help", "",
            "Please enter the dose level labels (separated by commas) for each dose level evaluated in the trial",
            placement = "top", trigger = "focus"
          ),
          
          #### Starting Dose ---------------------
          selectInput(
            "dt_start_level",
            div(p(class = "help-p", "Starting Dose Level"), HTML('<button id="dt-start-level-help" class="help-btn" type="button">?</button>')), 
            choices = c(-1,1,2,3), selected = 1, width = "100%"
          ),
          bsPopover(
            "dt-start-level-help", "",
            "Please enter the starting dose level from the dose level labels above",
            placement = "top", trigger = "focus"
          ),
          
          #### Number of Trials ---------------------
          sliderInput(
            "dt_num_trials",
            div(p(class = "help-p", "Number of Simulated Trials"), HTML('<button id="dt-num-trials-help" class="help-btn" type="button">?</button>')), 
            min = 0, max = 10000, value = 100, width = "100%", step = 100, ticks = FALSE
          ),
          bsPopover(
            "dt-num-trials-help", "",
            "Please enter the number of simulated trials. A larger number of simulations increases the precision of simulation results and computation time.",
            placement = "top", trigger = "focus"
          ),
          
          #### Target Toxicity ---------------------
          sliderInput(
            "dt_target_tox",
            div(p(class = "help-p", "Target Toxicity Probability"), HTML('<button id="dt-target-tox-help" class="help-btn" type="button">?</button>')),
            min = 0, max = 0.8, value = 0.2, step = 0.01, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "dt-target-tox-help", "",
            "Please enter the target toxicity probability of the study agent",
            placement = "top", trigger = "focus"
          ),
          
          #### True Toxicity ---------------------
          textInput(
            "dt_true_tox",
            div(p(class = "help-p", "True Toxicity Probability Vector Per Dose Level"), HTML('<button id="dt-true-tox-help" class="help-btn" type="button">?</button>')),
            value = "0.05,0.12,0.2,0.3", width = "100%"
          ),
          bsPopover(
            "dt-true-tox-help", "",
            "Please enter the true toxicity probabilities for each dose level (separated by commas). Toxicity probabilities must increase with each subsequent dose level.",
            placement = "top", trigger = "focus"
          ),
          
          #### Arrival Rate ---------------------
          sliderInput(
            "dt_arrival_rate",
            div(p(class = "help-p", "Average Time Between Patient Enrollments (in Days)"), HTML('<button id="dt-arrival-rate-help" class="help-btn" type="button">?</button>')),
            min = 0, max = 180, value = 15, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "dt-arrival-rate-help", "",
            "Please enter the average time (in days) between enrolling patients",
            placement = "top", trigger = "focus"
          ),
          
          #### Cycle Length ---------------------
          sliderInput(
            "dt_cycle_length",
            div(p(class = "help-p", "Duration of DLT Observation Period (in Days)"), HTML('<button id="dt-cycle-length-help" class="help-btn" type="button">?</button>')), 
            min = 0, max = 365, value = 28, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "dt-cycle-length-help", "",
            "Please enter the duration (in days) of the DLT observation period",
            placement = "top", trigger = "focus"
          ),
                 
          ### TARGET-CRM & CRM Inputs ---------------------
          conditionalPanel(
            condition = "input.dt_selectors.includes('TARGET-CRM') || input.dt_selectors.includes('CRM')",
            
            #### Prior Toxicity ---------------------
            textInput(
              "dt_prior_tox",
              div(p(class = "help-p", "Prior Toxicity Probability Vector Per Dose Level"), HTML('<button id="dt-prior-tox-help" class="help-btn" type="button">?</button>')),
              value = "0.05,0.12,0.2,0.3", width = "100%"
            ),
            bsPopover(
              "dt-prior-tox-help", "",
              "Please enter the prior toxicity probabilities for each dose level (separated by commas). Toxicity probabilities must increase with each subsequent dose level.",
              placement = "top", trigger = "focus"
            ),
            
            #### Max Sample  ---------------------
            sliderInput(
              "dt_max_n",
              div(p(class = "help-p", "Maximum Sample Size"), HTML('<button id="dt-max-n-help" class="help-btn" type="button">?</button>')),
              min = 1, max = 200, value = 18, width = "100%", ticks = FALSE
            ),
            bsPopover(
              "dt-max-n-help", "",
              paste(
                "Please enter the maximum number of patients to be enrolled per trial. Trial accuracy increases with a larger sample size.",
                "The selected sample size should balance trial accuracy with accrual feasibility."
              ),
              placement = "top", trigger = "focus"
            ),
            
            #### Cohort Size ---------------------
            sliderInput(
              "dt_cohort_size",
              div(p(class = "help-p", "Cohort Size"), HTML('<button id="dt-cohort-size-help" class="help-btn" type="button">?</button>')),
              min = 1, max = 9, value = 3, width = "100%", ticks = FALSE
            ),
            bsPopover(
              "dt-cohort-size-help", "",
              "Please enter the cohort size. The cohort size is the number of patients to be treated at the current dose level before a dose escalation decision is made.",
              placement = "top", trigger = "focus"
            )
          ),
                 
          ### TARGET-CRM Inputs ---------------------
          conditionalPanel(
            condition = "input.dt_selectors.includes('TARGET-CRM')",
            
            #### Prop B ---------------------
            sliderInput(
              "dt_prop_b",
              div(p(class = "help-p", "Proportion of Patients from Cohort B"), HTML('<button id="dt-prop-b-help" class="help-btn" type="button">?</button>')),
              min = 0, max = 1, value = 0.1, step = 0.01, width = "100%", ticks = FALSE
            ),
            bsPopover(
              "dt-prop-b-help", "",
              paste(
                "Patients belong to either Cohort A (general enrollment) or Cohort B (enrichment cohort).",
                "Please enter the proportion of enrolled patients belonging to Cohort B.",
                "Enter a proportion of 0 if no enrichment cohort is needed."
              ),
              placement = "top", trigger = "focus"
            ),
            
            #### Minimum Cohort B ---------------------
            sliderInput(
              "dt_min_cohort_b",
              div(p(class = "help-p", "Minimum Enrollment of Cohort B Patients (Optional)"), HTML('<button id="dt-min-cohort-b-help" class="help-btn" type="button">?</button>')),
              min = 0, max = 100, value = 0, width = "100%", ticks = FALSE
            ),
            bsPopover(
              "dt-min-cohort-b-help", "",
              paste(
                "An optional feature is to require a trial to enroll a minimum number of Cohort B patients.",
              "Once the maximum N is attained, enrollment of Cohort A patients will be suspended and only Cohort B patients may enroll",
              "until the minimum number has been attained. Please enter the minimum number of Cohort B patients to be enrolled in a trial.",
              "Enter 0 if no minimum number is required."
              ),
              placement = "top", trigger = "focus"
            )
          ),
          
          ### Simulate/Download/Reset Buttons ---------------------
          splitLayout(
            cellWidths = c("50%", "25%", "25%"),
            actionButton("dt_simulate", "Simulate"),
            downloadButton("dt_results", ""),
            actionButton("dt_reset", "Reset")
          ),
          bsPopover(
            "dt_simulate", "",
            "Simulates the selected design(s) using the values of the above inputs", 
            placement = "top"
          ),
          bsPopover(
            "dt_results", "",
            "Download a MS Word document with the full report of plots, tables, and summaries", 
            placement = "top"
          ),
          bsPopover(
            "dt_reset", "",
            "WARNING: Resets all of the inputs and results. Cannot be undone.", 
            placement = "top"
          )
        ),
        column(9,
            uiOutput("dt_plots_ui")
        )
      )
    )
  ),
  
  ## Conduct Tab ---------------------
  nav("CONDUCT YOUR TRIAL",
    div(class = "other_tabs",
      fluidRow(
        column(3, style="overflow-y:scroll; height: 80vh;",
          h3("Inputs", style="text-align: center;"),
          br(),
          
          ### Designs ---------------------
          radioButtons(
            "ct_selectors",
            div(p(class = "help-p", "Design"), HTML('<button id="ct-selectors-help" class="help-btn" type="button">?</button>')),
            c("CRM", "TARGET CRM"), inline = "TRUE"
          ),
          bsPopover(
            "ct-selectors-help", "",
            "Select the design to run in the model", 
            placement = "top", trigger = "focus"
          ),
          
          ### Number of Doses ---------------------
          sliderInput(
            "ct_num_doses",
            div(p(class = "help-p", "Number of Dose Levels"), HTML('<button id="ct-num-doses-help" class="help-btn" type="button">?</button>')),
            min = 3, max = 10, value = 4, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "ct-num-doses-help", "",
            "Please enter the number of dose levels in your trial",
            placement = "top", trigger = "focus"
          ),
          
          ### Dose Labels ---------------------
          textInput(
            "ct_dose_labels",
            div(p(class = "help-p", "Dose Level Labels"), HTML('<button id="ct-dose-labels-help" class="help-btn" type="button">?</button>')),
            value = "-1,1,2,3", width = "100%"
          ),
          bsPopover(
            "ct-dose-labels-help", "",
            "Please enter the dose level labels (separated by commas) for each dose level evaluated in the trial",
            placement = "top", trigger = "focus"
          ),
          
          ### Target Toxicity ---------------------
          sliderInput(
            "ct_target_tox",
            div(p(class = "help-p", "Target Toxicity Probability"), HTML('<button id="ct-target-tox-help" class="help-btn" type="button">?</button>')),
            min = 0, max = 0.8, value = 0.2, step = 0.01, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "ct-target-tox-help", "",
            "Please enter the target toxicity probability of the study agent", 
            placement = "top", trigger = "focus"
          ),
          
          ### Prior Toxicity ---------------------
          textInput(
            "ct_prior_tox",
            div(p(class = "help-p", "Prior Toxicity Probability Vector Per Dose Level"), HTML('<button id="ct-prior-tox-help" class="help-btn" type="button">?</button>')),
            value = "0.05,0.12,0.2,0.3", width = "100%"
          ),
          bsPopover(
            "ct-prior-tox-help", "",
            paste(
              "Please enter the estimated prior toxicity probabilities for each dose level", 
              "evaluated in the trial (separated by commas).",
              "Toxicity probabilities must increase with each subsequent dose level."
            ),
            placement = "top", trigger = "focus"
          ),
          
          ### Cohort Size ---------------------
          sliderInput(
            "ct_cohort_size",
            div(p(class = "help-p", "Cohort Size"), HTML('<button id="ct-cohort-size-help" class="help-btn" type="button">?</button>')),
            min = 1, max = 9, value = 3, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "ct-cohort-size-help", "",
            paste(
              "Please enter the cohort size. The cohort size is the number of patients to be treated",
              "at the current dose level before a dose escalation decision is made."
            ),
            placement = "top", trigger = "focus"
          ),
          
          ### Slots Remaining ---------------------
          sliderInput(
            "ct_slots",
            div(p(class = "help-p", "Number of Slots Remaining"), HTML('<button id="ct-slots-help" class="help-btn" type="button">?</button>')),
            min = 0, max = 8, value = 0, width = "100%", ticks = FALSE
          ),
          bsPopover(
            "ct-slots-help", "",
            paste(
              "Please enter the number of slots remaining to be enrolled for the current cohort of patients.",
              "Escalating to a higher dose level is not permitted until the current cohort is fully evaluated",
              "at the current dose level. Please update this input parameter for each successive patient enrolled in the trial."
            ),
            placement = "top", trigger = "focus"
          ),
          
          ### Current Dose ---------------------
          selectInput(
            "ct_current_dose",
            div(p(class = "help-p", "Dose level Currently Under Evaluation"), HTML('<button id="ct-current-dose-help" class="help-btn" type="button">?</button>')),
            choices = c(-1,1,2,3), selected = 1, width = "100%"
          ),
          bsPopover(
            "ct-current-dose-help", "",
            paste(
              "Please enter the dose level currently under evaluation.",
              "(TARGET-CRM permits enrolling patients at one dose level below the dose level under evaluation.)"
            ),
            placement = "top", trigger = "focus"
          )
        ),
        column(9,
          fluidRow(
            column(5,
              h3("Enter Patient Toxicity Data:", style = "text-align: center;"),
              
              ### Patient ID ---------------------
              textInput(
                "ct_pid",
                div(p(class = "help-p", "Patient ID"), HTML('<button id="ct-pid-help" class="help-btn" type="button">?</button>')),
                value = "C1", width = "100%"
              ),
              bsPopover(
                "ct-pid-help", "",
                paste(
                  "Please enter a patient identifier. This auto-populates if you choose not to enter an ID.",
                  "DO NOT ENTER PATIENT PROTECTED HEALTH INFORMATION (PHI) (e.g. MRNs, patient initials, cooperative group ID)."
                ),
                placement = "bottom", trigger = "focus"
              ),
              
              ### Dose Administered ---------------------
              selectInput(
                "ct_dose_adm",
                div(p(class = "help-p", "Administered Dose Level"), HTML('<button id="ct-dose-adm-help" class="help-btn" type="button">?</button>')),
                choices = c(-1,1,2,3), selected = 1, width = "100%"
              ),
              bsPopover(
                "ct-dose-adm-help", "",
                "Please select the dose level administered",
                placement = "top", trigger = "focus"
              ),
              
              ### DLT Observed ---------------------
              checkboxInput(
                "ct_dlt_obs", value = FALSE, width = "100%",
                label = div(class = "ct_checkbox", p(class = "help-p", "Experienced a DLT?"), HTML('<button id="ct-dlt-obs-help" class="help-btn" type="button">?</button>'))
              ),
              bsPopover(
                "ct-dlt-obs-help", "",
                "Please select if a dose limiting toxicity (DLT) was observed", 
                placement = "top", trigger = "focus"
              ),
              
              ### Include in Model ---------------------
              checkboxInput(
                "ct_include", value = TRUE, width = "100%",
                label = div(class = "ct_checkbox", p(class = "help-p", "Include Patient in Model?"), HTML('<button id="ct-include-help" class="help-btn" type="button">?</button>'))
              ),
              bsPopover(
                "ct-include-help", "",
                "Select to include this patient in the model", 
                placement = "top", trigger = "focus"
              ),
              
              ### Add/Remove Buttons ---------------------
              splitLayout(
                actionButton("ct_add", "Add New Patient"),
                actionButton("ct_remove", "Remove Selected Patient")
              ),
              bsPopover(
                "ct_add", "",
                "Add the chosen patient inputs to the table", 
                placement = "top",
              ),
              
              ### Patients Table ---------------------
              DTOutput("ct_patients_table")
            ),
            column(7,
                   
              ### Results UI ---------------------
              uiOutput("ct_patients_ui"),
              
              ### Simulate/Download/Reset Buttons ---------------------
              splitLayout(
                cellWidths = c("50%", "25%", "25%"), cellArgs = list(style = "padding: 5px"),
                actionButton("ct_simulate", "Run Model"),
                downloadButton("ct_results", ""),
                actionButton("ct_reset", "Reset")
              ),
              bsPopover(
                "ct_simulate", "",
                "Simulates the selected design using the chosen inputs and patients info", 
                placement = "bottom",
              ),
              bsPopover(
                "ct_results", "",
                "Download the dose escalation recommendations in a MS Word report", 
                placement = "bottom",
              ),
              bsPopover(
                "ct_reset", "",
                "WARNING: Resets all of the inputs and results. Cannot be undone.", 
                placement = "bottom",
              )
            )
          )
        )
      )
    )
  ),
  
  ## About Tab ---------------------
  nav("ABOUT",
    fluidRow(class = "text-body",
      h2(id = "about-top", "DEDUCE Leadership:"),
      p(class = "main-text", strong("Dana-Farber/Boston Children's Cancer and Blood Disorders Center")),
      br(),
      p(class = "main-text", "Clement Ma, PhD"),
      p(class = "main-text", "Wendy B. London, PhD"),
      br(),
      h2("Development Team:"),
      p(class = "main-text", strong("Northwestern Mutual")),
      br(),
      p(class = "main-text", "Judy Berdan"),
      p(class = "main-text", "Laure Borchardt"),
      p(class = "main-text", "Audra Brennan"),
      p(class = "main-text", "Stan Crane"),
      p(class = "main-text", "Ben Garski"),
      p(class = "main-text", "Nanette Jamel"),
      p(class = "main-text", "Lori Kiraly"),
      p(class = "main-text", "Danielle Pankey"),
      p(class = "main-text", "Susan Stegman, MD"),
      br(),
      h2("Contact:"),
      p(class = "main-text", "For assistance, please contact: Drs. Clement Ma and Wendy B. London."),
      br(),
      h2("Citation:"),
      p(class = "main-text", "To cite DEDUCE, please use: Ma C, Berdan J, Borchardt L, Crane S, Garski B, Jamel N, Kiraly L, Pankey D, 
        Stegman S, London WB (2021). DEsign and conDUCt of dose Escalation trials (DEDUCE). Available at:", 
        a(href="https://bengarski.shinyapps.io/DEDUCE/", "https://bengarski.shinyapps.io/DEDUCE/")
      ),
      br(),
      h2("Acknowledgements:"),
      p(class = "main-text",
        "We would like to thank the Northwestern Mutual Tech for Good team for their pro-bono development, design, and project management 
        support for the DEDUCE platform. We would also like to thank our test users, Drs. Steven G. DuBois, Karen D. Wright, 
        and David S. Shulman for their helpful feedback."
      ),
      br(),
      h2("References:"),
      tags$ul(
        tags$li(
          p(class = "main-text",
            a("Storer BE. Design and Analysis of Phase I Clinical Trials. ", em("Biometrics. "), "1989;45(3):925-37.",
              href="https://pubmed.ncbi.nlm.nih.gov/2790129/", target="_blank", rel="noopener noreferrer")
          )
        ),
        tags$li(
          p(class = "main-text",
            a("O'Quigley J, Pepe M, Fisher L. Continual reassessment method: a practical design for phase 1 Clinical trials in cancer. ", 
              em("Biometrics. "), "1990;46(1):33-48.", href="https://pubmed.ncbi.nlm.nih.gov/2350571/", 
              target="_blank", rel="noopener noreferrer")
          )
        )
      )
    ),
    fluidRow(class = "footer",
      column(12,
        splitLayout(
          a(
            href="https://www.danafarberbostonchildrens.org",
            img(id="df_logo", src = "dfbc.png"),
            target="_blank", rel="noopener noreferrer"
          ),
          a(
            href="https://www.NorthwesternMutual.com",
            img(id="nm_logo", src = "nm.png"),
            target="_blank", rel="noopener noreferrer"
          )
        )
      )
    )
  ),
  
  ## Help Link ---------------------
  nav_item(
    a("HELP", icon("external-link-alt"), href = "https://drive.google.com/file/d/1LDj36CAF3Hnf0r5KRgeU5QiiI9ET5G-R/view", target="_blank", rel="noopener noreferrer")
  ),
  nav_spacer(),
  
  ## Github Link --------------------- 
  nav_item(
    actionButton(
      "nav-git-btn", "", icon = icon("github"), onclick = "window.open('https://github.com/b-gar/DEDUCE', '_blank', 'noopener noreferrer')"
    )
  )
)

# Server ---------------------
server <- function(input, output, session) {

  ## Design Tab ---------------------
  
  ### Misc. ---------------------
  
  # Get the Design Names That Are Selected
  dt_selected_design_names <- reactive({
    input$dt_selectors
  })
  
  # Set Initial Reactive Value to Keep Track if Simulation Was Ran
  dt_sim_count <- reactiveVal(NULL)
  
  ### Warnings for Invalid Inputs ---------------------
  
  # Designs
  observe({
    if (is.null(input$dt_selectors)){
      shinyjs::alert("A design must be selected before running the simulation")
    }
  })
  
  # Dose Labels
  observeEvent(list(input$dt_dose_labels, input$dt_num_doses), {
    hideFeedback("dt_dose_labels")
    if (length(unlist(strsplit(input$dt_dose_labels, ",")))!= input$dt_num_doses) {
      showFeedbackDanger("dt_dose_labels", "The length must match the number of dose levels selected above. Be sure to use commas to separate each label.")
    }
  })
  
  # True Tox
  observeEvent(list(input$dt_true_tox, input$dt_num_doses), {
    hideFeedback("dt_true_tox")
    if (length(unlist(strsplit(input$dt_true_tox, ",")))!= input$dt_num_doses) {
      showFeedbackDanger("dt_true_tox", "The length must match the number of dose levels selected at the top. Be sure to use commas to separate each decimal.")
    }
    else if (increment_check(input$dt_true_tox)==FALSE) {
      showFeedbackDanger("dt_true_tox", "The probabilities must increase with each subsequent dose")
    }
    else if (decimal_check(input$dt_true_tox)==FALSE) {
      showFeedbackDanger("dt_true_tox", "The probabilities must be a decimal")
    }
  })
  
  # Prior Tox
  observeEvent(list(input$dt_prior_tox, input$dt_num_doses), {
    hideFeedback("dt_prior_tox")
    if (length(unlist(strsplit(input$dt_prior_tox, ",")))!= input$dt_num_doses) {
      showFeedbackDanger("dt_prior_tox", "The length must match the number of dose levels selected at the top. Be sure to use commas to separate each decimal.")
    }
    else if (increment_check(input$dt_prior_tox)==FALSE) {
      showFeedbackDanger("dt_prior_tox", "The probabilities must increase with each subsequent dose")
    }
    else if (decimal_check(input$dt_prior_tox)==FALSE) {
      showFeedbackDanger("dt_prior_tox", "The probabilities must be a decimal")
    }
  })
  
  ### Observers ---------------------
  
  # Update Start Level Based on Dose Labels
  observe({
    updateSelectInput(session, "dt_start_level", choices = unlist(strsplit(input$dt_dose_labels, ",")), selected = unlist(strsplit(input$dt_dose_labels, ","))[2])
  })
  
  # Update Max Depending on Previous Input
  observe({
    updateSliderInput(session, "dt_min_cohort_b", max = input$dt_max_n)
  })
  
  # Disable Simulate Button If Any Input Warnings Present or If No Designs Selected
  observe({
    if(length(unlist(strsplit(input$dt_dose_labels, ",")))!= input$dt_num_doses ||
       length(unlist(strsplit(input$dt_true_tox, ",")))!= input$dt_num_doses ||
       increment_check(input$dt_true_tox)==FALSE ||
       decimal_check(input$dt_true_tox)==FALSE ||
       length(unlist(strsplit(input$dt_prior_tox, ",")))!= input$dt_num_doses ||
       increment_check(input$dt_prior_tox)==FALSE ||
       decimal_check(input$dt_prior_tox)==FALSE ||
       is.null(input$dt_selectors)){
        disable("dt_simulate")
    } else{
      enable("dt_simulate")
    }
  })
  
  # Shows the Plots UI After Clicking Simulate
  observeEvent(input$dt_simulate, {
    show("dt_ui_plots")
  })
  
  # If a Simulation was Ran Already - Enable Results, Disable Inputs and Simulate - Forces User to Reset After Each Use
  observe({
    if (!is.null(dt_sim_count())) {
      enable("dt_results")
      disable("dt_selectors")
      disable("dt_num_doses")
      disable("dt_dose_labels")
      disable("dt_start_level")
      disable("dt_num_trials")
      disable("dt_target_tox")
      disable("dt_true_tox")
      disable("dt_arrival_rate")
      disable("dt_cycle_length")
      disable("dt_prior_tox")
      disable("dt_max_n")
      disable("dt_cohort_size")
      disable("dt_prop_b")
      disable("dt_min_cohort_b")
      disable("dt_simulate")
      removePopover(session, "dt_simulate")
    } else{
      disable("dt_results")
      enable("dt_simulate")
      addPopover(session, "dt_simulate", "", "Simulates the selected design(s) using the values of the above inputs", placement = "top")
    }
  })
  
  # Hide/Enable/Reset the UI Elements When Reset is Clicked
  observeEvent(input$dt_reset, {
    hide("dt_ui_plots")
    enable("dt_selectors")
    enable("dt_num_doses")
    enable("dt_dose_labels")
    enable("dt_start_level")
    enable("dt_num_trials")
    enable("dt_target_tox")
    enable("dt_true_tox")
    enable("dt_arrival_rate")
    enable("dt_cycle_length")
    enable("dt_prior_tox")
    enable("dt_max_n")
    enable("dt_cohort_size")
    enable("dt_prop_b")
    enable("dt_min_cohort_b")
    reset("dt_selectors")
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
    dt_sim_count(NULL)
    disable("dt_results")
  })
  
  ### UI ---------------------
  
  # Main Plotting UI
  output$dt_plots_ui <- renderUI({
    req(length(dt_selected_design_names()) > 0)
    hidden(
      div(id="dt_ui_plots",
        tagList(
          splitLayout(
            plotOutput("dt_plot_1", width = "90%", height = "40vh"),
            plotOutput("dt_plot_3", width = "90%", height = "40vh")
          ),
          br(),
          splitLayout(
            plotOutput("dt_plot_2", width = "90%", height = "40vh"),
            plotOutput("dt_plot_4", width = "90%", height = "40vh")
          )
        )
      )
    )
  })
  
  ### Running the Functions ---------------------
  dt_function_outputs <- eventReactive(input$dt_simulate, {
    w <- Waiter$new(html = spin_heartbeat(), color = "black")
    w$show()
    
    # Change Reactive Value When Design is Ran
    dt_sim_count(1)
    
    # 3+3
    if ("3+3" %in% input$dt_selectors) {
      
      tpt <- three_plus_three(target_tox = input$dt_target_tox, number_trials = input$dt_num_trials, 
                              true_tox = numerizer(input$dt_true_tox), arrival_rate = input$dt_arrival_rate, cycle_length = input$dt_cycle_length, 
                              start_level = match(input$dt_start_level, unlist(strsplit(input$dt_dose_labels, ","))))
    }
    
    # TARGET-CRM
    if("TARGET-CRM" %in% input$dt_selectors) {
      
      tcrm <- my_target_crm(prior = numerizer(input$dt_prior_tox), target_tox = input$dt_target_tox, 
                            number_trials = input$dt_num_trials, true_tox = numerizer(input$dt_true_tox), 
                            arrival_rate = input$dt_arrival_rate, prop_b = input$dt_prop_b, min_cohort_b = input$dt_min_cohort_b, cycle_length = input$dt_cycle_length, 
                            cohort_size = input$dt_cohort_size, max_n = input$dt_max_n, start_level = match(input$dt_start_level, unlist(strsplit(input$dt_dose_labels, ","))))
      
    }
    
    # CRM
    if("CRM" %in% input$dt_selectors) {
      
      crm <- my_crm(prior = numerizer(input$dt_prior_tox), target_tox = input$dt_target_tox, 
                    number_trials = input$dt_num_trials, true_tox = numerizer(input$dt_true_tox), 
                    arrival_rate = input$dt_arrival_rate, min_cohort_b = input$dt_min_cohort_b, cycle_length = input$dt_cycle_length, 
                    cohort_size = input$dt_cohort_size, max_n = input$dt_max_n, start_level = match(input$dt_start_level, unlist(strsplit(input$dt_dose_labels, ","))))
    }
    
    all <- list(get0("tpt"), get0("tcrm"), get0("crm"))
    w$hide()
    return(all[lengths(all) != 0])
    
    
  })
  
  ### Dataframes to Access Data ---------------------
  
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
  
  ### Plots ---------------------
  
  # Plot1
  dt_plot_1_val <- reactive({
    if(length(dt_selected_design_names()) > 1){
      ggplot() + 
        geom_bar(data = dt_plot_df() %>% 
                   mutate(mtd_prop=mtd.Freq/input$dt_num_trials), aes(x=dose_level, y=mtd_prop, fill=design), stat="identity", position="dodge") + 
        geom_bar(data = dt_plot_df() %>% 
                   mutate(mtd_prop=mtd.Freq/input$dt_num_trials) %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=mtd_prop, fill=design, color=as.factor(true_mtd)), stat="identity", position="dodge", size=2) +
        xlab("Dose Level") + ylab("") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
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
        xlab("Dose Level") + ylab("") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Simulated Trials Selecting\nEach Dose Level as True MTD") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  output$dt_plot_1 <- renderPlot({
    dt_plot_1_val()
  })
  
  # Plot2
  dt_plot_2_val <- reactive({
    if (length(dt_selected_design_names()) > 1){
      ggplot() + 
        geom_bar(data = dt_plot_df(), aes(x=dose_level, y=obs_tox_table, fill=design), stat="identity", position="dodge") + 
        geom_bar(data = dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=obs_tox_table, fill=design, color=as.factor(true_mtd)), stat="identity", position="dodge", size=2) +
        geom_hline(aes(yintercept=input$dt_target_tox), linetype="dashed") + guides(color = FALSE) +
        xlab("Dose Level") + ylab("") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(dt_selected_design_names()) == 1){
      ggplot() + 
        geom_bar(data = dt_plot_df(), aes(x=dose_level, y=obs_tox_table), stat="identity", position="dodge", fill="#BEBEBE") + 
        geom_bar(data = dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=obs_tox_table, color=as.factor(true_mtd)), stat="identity", position="dodge", fill="#BEBEBE", size=2) +
        geom_hline(aes(yintercept=input$dt_target_tox), linetype="dashed") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        xlab("Dose Level") + ylab("") + ggtitle("Proportion of Patients Experiencing\na DLT Per Dose Level") + 
        theme(plot.title = element_text(hjust = 0.5)) + guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  
  output$dt_plot_2 <- renderPlot({
    dt_plot_2_val()
  })
  
  # Plot3
  dt_plot_3_val <- reactive({
    if (length(dt_selected_design_names()) > 1){
      ggplot() + 
        geom_bar(data=dt_plot_df(), aes(x=dose_level, y=patient_allocation_table, fill=design), stat="identity", position="dodge") +
        geom_bar(data=dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=patient_allocation_table, fill=design, color=as.factor(true_mtd)), stat="identity", position="dodge", size=2) +
        xlab("Dose Level") + ylab("") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) +
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(order=1), color = guide_legend(override.aes = list(fill = "white"), order=2))
    }
    
    else if (length(dt_selected_design_names()) == 1){
      ggplot() + 
        geom_bar(data=dt_plot_df(), aes(x=dose_level, y=patient_allocation_table), stat="identity", position="dodge", fill="#BEBEBE") +
        geom_bar(data=dt_plot_df() %>% 
                   filter(dose_num == true_mtd), aes(x=dose_level, y=patient_allocation_table, color=as.factor(true_mtd)), stat="identity", position="dodge", fill="#BEBEBE", size=2) +
        xlab("Dose Level") + ylab("") + scale_color_manual(name="True MTD", values=c("black"), labels=NULL) + 
        ggtitle("Proportion of Patients Allocated\nto Each Dose Level") + theme(plot.title = element_text(hjust = 0.5)) + 
        guides(color = guide_legend(override.aes = list(fill = "white")))
    }
  })
  
  output$dt_plot_3 <- renderPlot({
    dt_plot_3_val()
    
  })
  
  # Plot4
  dt_plot_4_val <- reactive({
    dt_plot_df2() %>%
      ggplot(aes(x=design, y=mean_duration)) + 
      geom_point(size = 5) + geom_errorbar(aes(ymin= mean_duration - sd_duration, ymax = mean_duration + sd_duration), width = 0.3) + xlab("Design") +
      ylab("") + ggtitle("Mean Study Duration in Days (+/- 1 SD)") + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$dt_plot_4 <- renderPlot({
    dt_plot_4_val()
  })
  
  ### Dataframe and Variables for Report Generation ---------------------
  
  # Table DF for Report Results
  dt_table_1_df <- reactive({
    
    table_list <- list()
    
    for (v in seq(1, length(dt_selected_design_names()))) {
      if (dt_function_outputs()[[v]]$df$design[1] == "3+3") {
        
        x <- round(unname(c(null_to_na(dt_function_outputs()[[v]]$pcs), null_to_na(dt_function_outputs()[[v]]$true_mtd), 
                            dt_function_outputs()[[v]]$mtd_selection_table/dt_function_outputs()[[v]]$number_trials, 
                            null_to_na(dt_function_outputs()[[v]]$obs_tox_overall), dt_function_outputs()[[v]]$obs_tox_table, 
                            null_to_na(dt_function_outputs()[[v]]$mean_obs_n), null_to_na(dt_function_outputs()[[v]]$min_obs_n), 
                            null_to_na(dt_function_outputs()[[v]]$max_obs_n), dt_function_outputs()[[v]]$patient_allocation_table, 
                            dt_function_outputs()[[v]]$mean_duration, dt_function_outputs()[[v]]$sd_duration, NA, NA)), 3)
        x_name <- dt_function_outputs()[[v]]$df$design[1]
      }
      else {
        x <- round(unname(c(null_to_na(dt_function_outputs()[[v]]$pcs), null_to_na(dt_function_outputs()[[v]]$true_mtd), 
                            dt_function_outputs()[[v]]$mtd_selection_table/dt_function_outputs()[[v]]$number_trials, 
                            null_to_na(dt_function_outputs()[[v]]$obs_tox_overall), dt_function_outputs()[[v]]$obs_tox_table, 
                            null_to_na(dt_function_outputs()[[v]]$mean_obs_n), null_to_na(dt_function_outputs()[[v]]$min_obs_n), 
                            null_to_na(dt_function_outputs()[[v]]$max_obs_n), dt_function_outputs()[[v]]$patient_allocation_table, 
                            dt_function_outputs()[[v]]$mean_duration, dt_function_outputs()[[v]]$sd_duration, 
                            null_to_na(dt_function_outputs()[[v]]$mean_cohort_b), null_to_na(dt_function_outputs()[[v]]$sd_cohort_b))), 3)
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
    
    if ("TARGET-CRM" %in% input$dt_selectors || "CRM" %in% input$dt_selectors) {
      
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
  
  ### Download Results ---------------------
  output$dt_results <- downloadHandler(
    filename = function(){paste0("DEDUCE Design ", lubridate::with_tz(Sys.time(), "America/New_York"), " ET.docx")}, 
    content = function(file){
      
      temp_report <- file.path(tempdir(), "report.rmd")
      file.copy("report.rmd", temp_report, overwrite = TRUE)
      params <- list(d = dt_selected_design_names(), m = dt_report_methods(), r = dt_report_results(), 
                     p1 = dt_plot_1_val(), p2 = dt_plot_2_val(), p3 = dt_plot_3_val(), p4 = dt_plot_4_val(), t = dt_table_1_df())
      render(temp_report, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  ## Conduct Tab ---------------------
  
  # Set Initial Reactive Value to Keep Track If a Simulation Was Ran
  ct_sim_count <- reactiveVal(NULL)
  
  ### Warnings for Invalid Inputs ---------------------
  
  # Dose Labels
  observeEvent(list(input$ct_dose_labels, input$ct_num_doses), {
    hideFeedback("ct_dose_labels")
    if (length(unlist(strsplit(input$ct_dose_labels, ",")))!= input$ct_num_doses) {
      showFeedbackDanger("ct_dose_labels", "The length must match the number of dose levels selected above. Be sure to use commas to separate each label.")
    }
  })
  
  # Prior Tox
  observeEvent(list(input$ct_prior_tox, input$ct_num_doses), {
    hideFeedback("ct_prior_tox")
    if (length(unlist(strsplit(input$ct_prior_tox, ",")))!= input$ct_num_doses) {
      showFeedbackDanger("ct_prior_tox", "The length must match the number of dose levels selected at the top. Be sure to use commas to separate each decimal.")
    }
    else if (increment_check(input$ct_prior_tox)==FALSE) {
      showFeedbackDanger("ct_prior_tox", "The probabilities must increase with each subsequent dose")
    }
    else if (decimal_check(input$ct_prior_tox)==FALSE) {
      showFeedbackDanger("ct_prior_tox", "The probabilities must be a decimal")
    }
  })
  
  ### Observers ---------------------
  
  # Update Number of Slots Remaining Max Based off Cohort Size
  observe({
    updateSliderInput(session, "ct_slots", max = input$ct_cohort_size - 1, value = input$ct_cohort_size - 1)
  })
  
  # Update Current Dose Based on Dose Labels
  observe({
    updateSelectInput(session, "ct_current_dose", choices = unlist(strsplit(input$ct_dose_labels, ",")), selected = unlist(strsplit(input$ct_dose_labels, ","))[2])
  })
  
  # Update Dose Administered Based on Dose Labels
  observe({
    updateSelectInput(session, "ct_dose_adm", choices = unlist(strsplit(input$ct_dose_labels, ",")), selected = unlist(strsplit(input$ct_dose_labels, ","))[2])
  })
  
  # Update Patient ID Based on Length of Table
  observe({
    updateTextInput(session, "ct_pid", value = sprintf("C%d", nrow(ct_patients_df()) + 1))
  })
  
  # Disable Remove Patient Button if Row isn't Selected
  observe({
    if (!is.null(input$ct_patients_table_rows_selected)) {
      enable("ct_remove")
      addPopover(session, "ct_remove", "", "Remove the selected patient from the table below", placement = "top")
    } else{
      disable("ct_remove")
      removePopover(session, "ct_remove")
    }
  })
  
  # Disable Simulate Button If Patient Table is Empty, Max Rows Are Reached, or If Any Input Warnings Are Shown
  observe({
    if(length(which(ct_patients_df()$include == TRUE)) == 0 ||
       length(which(ct_patients_df()$include == TRUE)) == 50 ||
       length(unlist(strsplit(input$ct_dose_labels, ",")))!= input$ct_num_doses ||
       length(unlist(strsplit(input$ct_prior_tox, ",")))!= input$ct_num_doses ||
       increment_check(input$ct_prior_tox) == FALSE ||
       decimal_check(input$ct_prior_tox) == FALSE){
        disable("ct_simulate")
    }
    else{
      enable("ct_simulate")
    }
  })
  
  # Activate Download Button if a Simulation was Ran Already
  observe({
    if (!is.null(ct_sim_count())) {
      enable("ct_results")
    } else{
      disable("ct_results")
    }
  })
  
  # Show Table when Simulate is Pressed
  observeEvent(input$ct_simulate,{
    show("ct_ui_patients")
  })
  
  # Reset Inputs When Reset Button Clicked
  observeEvent(input$ct_reset, {
    reset("ct_selectors")
    reset("ct_num_doses")
    reset("ct_dose_labels")
    reset("ct_target_tox")
    reset("ct_prior_tox")
    reset("ct_cohort_size")
    reset("ct_current_dose")
    reset("ct_pid")
    reset("ct_dose_adm")
    reset("ct_dlt_obs")
    reset("ct_include")
    ct_patients_df(data.frame(patient_id=numeric(), dose_level=numeric(), dlt=numeric(), include=numeric()))
    hide("ct_ui_patients")
    disable("ct_results")
    ct_sim_count(NULL)
  })
  
  ### Patient Table ---------------------
  
  # Reactive Patients Table
  ct_patients_df <- reactiveVal(data.frame(patient_id=numeric(), dose_level=numeric(), dlt=numeric(), include=numeric()))
  
  # Add Patient Inputs to Table
  observeEvent(input$ct_add, {
    t <- rbind(ct_patients_df(), data.frame(patient_id=input$ct_pid, dose_level=input$ct_dose_adm, dlt=input$ct_dlt_obs, include=input$ct_include))
    ct_patients_df(t)
  })
  
  # Remove Selected Row of Table
  observeEvent(input$ct_remove, {
    t <- ct_patients_df()
    if (!is.null(input$ct_patients_table_rows_selected)) {
      t <- t[-as.numeric(input$ct_patients_table_rows_selected),]
    }
    ct_patients_df(t)
  })
  
  # Create the Patients Table
  output$ct_patients_table <- renderDT(ct_patients_df(), rownames = FALSE, 
                                       colnames = c("Patient ID", "Dose Level", "DLT Observed", "Include in Model"), selection = 'single', 
                                       options = list(dom = 't', scrollY = "25vh", ordering = FALSE, pageLength = nrow(ct_patients_df()),
                                                      initComplete = JS("function(settings, json) {","$(this.api().table().container()).css({'font-size': '18px'});","}"),
                                                      language = list(zeroRecords = "Add patient(s) to the table")
                                       )
                                       
  )
  
  ### Running the Function ---------------------
  ct_function_outputs <- eventReactive(input$ct_simulate, {
    
    # Set Reactive Value to Identify Simulation Ran
    ct_sim_count(1)
    
    tcrmc <- target_crm_conduct(prior = numerizer(input$ct_prior_tox), target_tox = input$ct_target_tox, tox = ct_patients_df()$dlt, 
                                dose_labels = unlist(strsplit(input$ct_dose_labels, ",")), 
                                level = match(ct_patients_df()$dose_level, unlist(strsplit(input$ct_dose_labels, ","))), 
                                pid = unlist(strsplit(ct_patients_df()$patient_id, ",")), include = which(ct_patients_df()$include), 
                                cohort_size = input$ct_cohort_size, num_slots_remain = input$ct_slots, 
                                current_dose = match(input$ct_current_dose, unlist(strsplit(input$ct_dose_labels, ","))))
    
    return(tcrmc)
  })
  
  ### Function Outputs ---------------------
  
  # UI Output for Patient Tables
  output$ct_patients_ui <- renderUI({
    hidden(
      div(id="ct_ui_patients",
        fluidRow(
          h3("Dose Escalation Recommendations", style = "text-align: center;"),
          DTOutput("ct_df"),
          textOutput("ct_next_dose")
        )
      )
    )
  })
  
  # DF
  output$ct_df <- renderDT(ct_function_outputs()$df2, rownames = FALSE,
                           colnames = c("Dose Level", "Prior Prob. of DLT", "# Patients", "# DLT's", "Posterior Prob. of DLT", "Lower Limit", "Upper Limit"),
                           options = list(dom = 't', scrollY = "30vh", ordering = FALSE, 
                                          initComplete = JS("function(settings, json) {","$(this.api().table().container()).css({'font-size': '18px'});","}")
                           )
  )
  
  # Recommended Dose
  output$ct_next_dose <- renderText({
    paste("Recommended Dose Level:", ct_function_outputs()$crm_out$mtd)
  })
  
  ### Download Results ---------------------
  output$ct_results <- downloadHandler(
    filename = function(){paste0("DEDUCE Conduct ", lubridate::with_tz(Sys.time(), "America/New_York"), " ET.docx")}, 
    content = function(file){
      
      temp_report <- file.path(tempdir(), "report_conduct.rmd")
      file.copy("report_conduct.rmd", temp_report, overwrite = TRUE)
      params <- list(d = input$ct_selectors, df1 = ct_function_outputs()$df1, df2 = ct_function_outputs()$df2, r1 = ct_function_outputs()$crm_out$mtd, 
                     r2 = ct_function_outputs()$crm_out$target, r3 = ct_function_outputs()$crm_out$prior, 
                     r4 = ct_function_outputs()$crm_out$prior.var, r5 = ct_function_outputs()$crm_out$estimate, 
                     r6 = ct_function_outputs()$crm_out$post.var, r7 = ct_function_outputs()$crm_out$dosename[ct_function_outputs()$current_dose], 
                     r8 = ct_function_outputs()$cohort_size, r9 = ct_function_outputs()$num_slots_remain)
      render(temp_report, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
