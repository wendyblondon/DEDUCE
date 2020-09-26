library(shiny)

ui <- navbarPage("DELPHI",
                 tabPanel("Home"),
                 tabPanel("Design"),
                 tabPanel("Conduct"),
                 tabPanel("Help")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
