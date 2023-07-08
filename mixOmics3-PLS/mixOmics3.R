library(shiny)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(mixOmics)


# Definition de l'interface utilisateur
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinytheme("lumen"),
  titlePanel("PLS-Omics"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        id = "myTabset",
        tabPanel("HOME", textOutput("ncomp")),
        
      )
    )
  )
)

# Definition du serveur
server <- function(input, output, session) {
  
  data <- reactive({
    data(liver.toxicity)
  })
  
  X <- reactive({
    data()$gene
  })  
  
  Y <- reactive({
    data()$clinic
  }) 
  
  
}

# Run the Shiny app
shinyApp(ui, server)