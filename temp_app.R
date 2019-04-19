library(shiny)
library(tidyverse)

ui <- navbarPage("Lab4, Team1",
                 tabPanel("Temporal"),
                 tabPanel("Spatial")
)

server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)

