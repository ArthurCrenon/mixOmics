library(shiny)
library(mixOmics)
library(tidyverse)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(readxl)
library(data.table)
library(igraph)


# Definition de l'interface utilisateur
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  #theme = shinytheme("lumen"),
  titlePanel("Multi-Omics"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # FILE IMPORT
      fileInput("files", "Import File", multiple = TRUE, accept = ".csv"),
      
      # FILE SELECTOR
      checkboxGroupInput("fileselector", "Files to treat", inline = TRUE),
      
      # KEEPX / Y VALUES
      # conditionalPanel(
      #   condition = "(input.myTabset == 'Graphique')",
      #   HTML("<hr>"),
      #   numericInput("keepx1", "Min KeepX", "20", "1", NA,"1"),
      #   numericInput("keepx2", "Max KeepX", "20", "1", NA,"1"),
      #   numericInput("keepy1", "Min KeepY", "20", "1", NA,"1"),
      #   numericInput("keepy2", "Max KeepY", "20", "1", NA,"1"),
      # ),
      
      # CUTOFF VALUE
      # conditionalPanel(
      #   condition = "input.myTabset == 'Graphique'",
      #   HTML("<hr>"),
      #   numericInput("cutoff", "CutOff", "0.9", "0", "0.99", "0.05"), 
      # ),
    ),
    
    mainPanel(
      tabsetPanel(
        id = "myTabset",
        tabPanel("HOME", tableOutput("contents"), tableOutput("contents2"), textOutput("test1"), textOutput("test2")),
        tabPanel("PLSDA", plotOutput("plot1"), plotOutput("plotV")),
        tabPanel("Perf DIABLO", plotOutput("plot2")),
        tabPanel("SelectVar", tableOutput("plot3")),
        tabPanel("DIABLO", plotOutput("plot4")),
        tabPanel("Plot Indiv", plotOutput("plot5")),
        tabPanel("Plot Arrow", plotOutput("plot6")),
        tabPanel("Circos Plot", plotOutput("plot7")),
        tabPanel("CIM", plotOutput("plot8")),
      )
    )
  )
)

# Definition du serveur
server <- function(input, output, session) {
  
  # MAKE FILE LIST FROM UPLOADED FILES
  fileList <- reactive({
    req(input$files)
    upload = list()
    for(nr in 1:length(input$files[, 1])){
      upload[[nr]] <- read.csv(
        header = TRUE,
        file = input$files[[nr, 'datapath']]
      )
    }
    upload
  })
  
  choiceList <- reactive({
    res <- NULL
    for (x in 1 : length(fileList())) {
      z <- strsplit(colnames(fileList()[[x]]), "_")
      name <- unique(sapply(z, '[[',1))[1]
      res <- append(res, name)
    }
    res
  })
  
  # DEFINE CLASSES
  Y <- reactive({
    (fileList()[[1]] %>% dplyr::select(c('Class')))$Class
  })
  
  
  X <- reactive({
    list = list()
    for (x in 1 : (length(fileList()))) {
      z <- strsplit(colnames(fileList()[[x]]), "_")
      name <- unique(sapply(z, '[[',1))[1]
      res <- fileList()[[x]] %>% dplyr::select_if(is.numeric)
      list[[name]] = res 
    }
    list
  })
  
  selectedFiles <- reactive({
    list = list()
    for(x in 1:length(input$fileselector)) {
      list[input$fileselector[x]] <- X()[input$fileselector[x]]
    }
    list
  })
  
  # DISPLAY FILE SELECTOR OPTIONS
  observe({
    updateCheckboxGroupInput(session, "fileselector", choices = choiceList(), inline = TRUE)
  })
  
  # VARIABLES
  
  #Launch DIABLO
  result.diablo.tcga <- reactive({
    block.plsda(selectedFiles(), Y())
  })
  
  # form basic DIABLO model
  basic.diablo.model <- reactive({
    block.splsda(X = selectedFiles(), Y = Y(), design = design()) 
  })
  
  #Select number of Component
  perf.diablo <- reactive({
    perf(basic.diablo.model(), validation = 'loo', folds = 3, nrepeat = 1)
  }) 
  
  ncomp <- reactive({
    perf.diablo()$choice.ncomp$WeightedVote["Overall.BER", "centroids.dist"]
  })
  
  tune.TCGA <- reactive({
    tune.block.splsda(X = selectedFiles(), Y = Y(), ncomp = 1, 
                      test.keepX = test.keepX, design = design(),
                      validation = 'loo', folds = 3, nrepeat = 1,
                      dist = "centroids.dist")
  })
  
  list.keepX <- reactive({
    #tune.TCGA()$choice.keepX
    c(25,25)
  }) 
  
  test.keepX = list (C18 = c(5:9, seq(10, 18, 2), seq(20,30,5)), 
                     Hilic = c(5:9, seq(10, 18, 2), seq(20,30,5)),
                     Lipid = c(5:9, seq(10, 18, 2), seq(20,30,5)))
  
  final.diablo.model <- reactive({
    block.splsda(X = selectedFiles(), Y = Y(), ncomp = 2, 
                 keepX = list.keepX(), design = design())
  })
  
  # For square matrix filled with 0.1s
  design <- reactive({
    mat <- matrix(1, ncol = length(selectedFiles()), nrow = length(selectedFiles()), 
                  dimnames = list(names(selectedFiles()), names(selectedFiles())))
    diag(mat) <- 0 
    mat
  }) 
  
  # KEEPX
  list.keepX <- reactive({
    c(input$keepx1, input$keepx2)
  })
  
  list.keepY <- reactive({
    c(input$keepy1, input$keepy2)
  })
  
  # set grid of values for each component to test
  test.keepX = list (mRNA = c(5:9, seq(10, 18, 2), seq(20,30,5)), 
                     miRNA = c(5:9, seq(10, 18, 2), seq(20,30,5)),
                     proteomics = c(5:9, seq(10, 18, 2), seq(20,30,5)))
  
  PPARAM <- reactive({
    BiocParallel::SnowParam(workers = parallel::detectCores()-1)
  })
  
  tune.diablo.splsda <- reactive({
    tune.block.splsda(selectedFiles(), Y(), 
                      ncomp = 2, 
                      #test.keepX = list.keepX(),# erreur avec test.keepX
                      validation = 'loo', 
                      folds = 4,
                      nrepeat = 50,
                      design = design(),
                      measure = "BER",
                      progressBar = TRUE,
                      dist = "mahalanobis.dist",
                      BPPARAM = PPARAM())
  })
  
  list.keepX.tune <- reactive({
    tune.diablo.splsda()$choice.keepX
  })
  
  diablo.lcms <- reactive({
    block.splsda(selectedFiles(), Y(),
                 ncomp= 2,
                 keepX = list.keepX.tune(),
                 design = design())
  })
  
  
  # OUTPUT PLOTS
  
  # PLOT 1 : PLSDA
  output$plot1 <- renderPlot({
    plotIndiv(result.diablo.tcga(), ellipse = TRUE)
  })
  
  # PLOT THE PLSDA VARIABLES
  output$plotV <- renderPlot({
    plotVar(result.diablo.tcga())
  })
  
  #PLOT 2 : PERF DIABLO
  output$plot2 <- renderPlot({
    plot(perf.diablo())
  })
  
  #PLOT 3 : SELECT VAR
  output$plot3 <- renderTable({
    selectVar(diablo.lcms(), 
              block = 'C18', 
              comp = 1)
  })
  
  #PLOT 4 : PLOT DIABLO
  output$plot4 <- renderPlot({
    plotDiablo(diablo.lcms(), ncomp = 2)
  })
  
  #PLOT 5 : PLOT INDIV
  output$plot5 <- renderPlot({
    plotIndiv(diablo.lcms(), 
              ncomp = 2,
              ind.names = FALSE, 
              blocks = 'weighted.average',
              ellipse = TRUE,
              legend = TRUE, title = 'Diablo.lcms comp 1-2')
  })
  
  #PLOT 6 : PLOT ARROW
  output$plot6 <- renderPlot({
    plotArrow(diablo.lcms(),
              ncomp = 2,
              ind.names = FALSE, 
              legend = TRUE, 
              save = 'pdf',
              name.save = 'arrowplot',
              title = 'Diablo.lcms comp1-2')
  })
  
  #PLOT 7 : CIRCOS PLOT
  output$plot7 <- renderPlot({
    circosPlot(diablo.lcms(), cutoff = 0.8,
               line = TRUE)
  })
  
  #PLOT 8 : CIM
  output$plot8 <- renderPlot({
    legend=list(legend = unique(factor(Y())), # set of classes
                title = "line",
                col = color.mixo(unique(factor(Y()))),# legend title
                cex = 0.8)
    cimDiablo(diablo.lcms(),
              comp = c(1,2,3),
              color.blocks = c('lightblue','lightgreen', 'red3'),
              margins = c(7,7),
              legend.position = 'topright',
              row.names = Y(),
              size.legend = 2,
              transpose = TRUE,
              save = 'pdf', 
              trim = TRUE,
              legend = legend,
              name.save = 'DiablocimTop5')
  })
  
  
  # INFOS OUTPUTS
  output$test1 <- renderText({
  })
  output$test2 <- renderText({
  })
  
  # TEST OUTPUTS
  output$contents <- renderTable({
    selectedFiles()
  })
  output$contents2 <- renderTable({
    X()
  })
  
}

# Run the Shiny app
shinyApp(ui, server)