library(shiny)

source("target_crm.R")

ui <- navbarPage("DELPHI",
                 tabPanel("Home"),
                 tabPanel("Design",
                          column(4,
                            selectizeInput("designPrior", "Prior Toxicity Probability", choices = NULL, multiple = TRUE, options = list(create = TRUE))
                          ),
                          column(8,
                                 
                          )
                  ),
                 tabPanel("Conduct"),
                 tabPanel("Help")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
