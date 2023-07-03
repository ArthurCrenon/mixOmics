library(shiny)
library(mixOmics)
library(tidyverse)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(readxl)

# Définition de l'interface utilisateur
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("lumen"),
  titlePanel("Analyse mixOmics"),
  
  sidebarLayout(
    # PARAMETER LAYOUT
    sidebarPanel(
      
      textOutput("meta"),
      #tableOutput("tab"),
      
      # FILE IMPORT
      textInput("file_separator", "CSV Separator", ";"),
      fileInput("file", "Import file", accept = ".csv"),
      
      
      # CONDITIONNAL PARAMETERS
      
      # PCA
      conditionalPanel(
        condition = "input.myTabset == 'PCA'",
        HTML("<hr>"),
        textInput("title1", "Title", "PCA"), 
        textInput("legend1", "Legend", "Species"),
      ),
      # PCA MULTI
      conditionalPanel(
        condition = "input.myTabset == 'PCA multi'",
        HTML("<hr>"),
        textInput("title2", "Title", "PCA multi"), 
        textInput("legend2", "Legend", "Species"),
      ),
      # PLOT 7
      conditionalPanel(
        condition = "input.myTabset == 'PLOT 7'",
        HTML("<hr>"),
        textInput("legend4", "Legend", "Species"),
      ),
      # VARIANCE
      conditionalPanel(
        condition = "input.myTabset == 'Variance' | input.myTabset == 'Contribution' | input.myTabset == 'PLOT 13'",
        HTML("<hr>"),
      ),
      # PLSDA
      conditionalPanel(
        condition = "input.myTabset == 'PLSDA'",
        HTML("<hr>"),
        textInput("title3", "Title", "PLS-DA"), 
        textInput("legend3", "Legend", "Species"),
      ),
      # MAX DISTANCE
      conditionalPanel(
        condition = "input.myTabset == 'Maximum Distance'",
        HTML("<hr>"),
      ),
      # BOXPLOT
      conditionalPanel(
        condition = "input.myTabset == 'BoxPlot' | input.myTabset == 'Error Rate' | input.myTabset == 'HeatMap'",
        HTML("<hr>"),
      ),
      # DOWNLOAD BUTTON
      conditionalPanel(
        condition = "input.myTabset != 'Home'",
        actionLink("dlPlot", "Download this plot", icon("arrow-up-from-bracket"), style="margin-top:25px;margin-bottom:15px;color: #fff; background-color: #337ab7; border-color: #2e6da4; padding: 10px; text-decoration:none; border-radius: 4px;display:block;text-align:center"),
      ),
      # BOXPLOT
      conditionalPanel(
        condition = "input.myTabset == 'BoxPlot'",
        HTML("<hr>"),
        numericInput("selected_variable", "Compound", "1", "1", NA, "1"),
      ),
      # PLOT 7
      conditionalPanel(
        condition = "input.myTabset == 'PLOT 7'",
        HTML("<hr>"),
        numericInput("cutoff", "CutOff", "0.9", "0", "0.99", "0.05"), 
      ),
      # CLASS PARAMETER
      conditionalPanel(
        condition = "input.myTabset == 'PLSDA' | input.myTabset == 'Maximum Distance' | input.myTabset == 'Error Rate' | input.myTabset == 'PLOT 13' | input.myTabset == 'Contribution' | input.myTabset == 'HeatMap'",
        HTML("<hr>"),
        checkboxGroupInput(
          inputId = "select",
          label = "Classes", 
          inline = TRUE
        ),
      ),
      # NUMB OF COMPONENT PARAMETER
      conditionalPanel(
        condition = "input.myTabset == 'PCA multi' | input.myTabset == 'PLOT 7' | input.myTabset == 'Variance' | input.myTabset == 'PLSDA' | input.myTabset == 'Maximum Distance' | input.myTabset == 'Error Rate' | input.myTabset == 'PLOT 13' | input.myTabset == 'Contribution' | input.myTabset == 'HeatMap'",
        HTML("<hr>"),
        numericInput("ncomp", "Number of components", "2", "2", NA,"1"),
      ),
      # PLSDA
      conditionalPanel(
        condition = "input.myTabset == 'PLSDA'",
        numericInput("plsda_comp1", "Component 1 (X axis)", "1", "1", NA, "1"), 
        numericInput("plsda_comp2", "Component 2 (Y axis)", "2", "1", NA, "1"), 
      ),
      # SHOW PARAMETERS
      conditionalPanel(
        condition = "input.myTabset == 'PCA' | input.myTabset == 'PCA multi' | input.myTabset == 'PLOT 7' | input.myTabset == 'PLSDA'",
        HTML("<hr>"),
        h3("Show :"),
        checkboxInput("ind_names", "Ind Names", TRUE),
        checkboxInput("ellipse", "Ellipse", TRUE),
        checkboxInput("abline", "Abline", TRUE),
        checkboxInput("star", "Star", TRUE),
        checkboxInput("legend", "Legend", TRUE),
      ),
      # GRAPHIC PARAMETERS
      conditionalPanel(
        condition = "input.myTabset == 'PCA multi' | input.myTabset == 'PLOT 7' | input.myTabset == 'Variance' | input.myTabset == 'BoxPlot' | input.myTabset == 'Maximum Distance'",
        HTML("<hr>"),
        h3("Graphic :"),
        checkboxInput("center", "Center", TRUE),
        checkboxInput("scale", "Scale", TRUE)
      ),
      # CONTRIBUTION
      conditionalPanel(
        condition = "input.myTabset == 'Contribution'",
        numericInput("comp", "Selected component", "1", "1", NA,"1"),
      ),
      # HEATMAP
      conditionalPanel(
        condition = "input.myTabset == 'HeatMap'",
      ),
      # KEEPX TRUE / FALSE
      conditionalPanel(
        condition = "input.myTabset == 'Error Rate' | input.myTabset == 'PLOT 13' | input.myTabset == 'Contribution' | input.myTabset == 'HeatMap'",
        HTML("<hr>"),
        checkboxInput("keepxc", "Edit KeepX", FALSE),
      ),
      # KEEPX VALUES
      conditionalPanel(
        condition = "(input.myTabset == 'Error Rate' | input.myTabset == 'PLOT 13' | input.myTabset == 'Contribution' | input.myTabset == 'HeatMap') && input.keepxc == 1",
        numericInput("keepx1", "Min KeepX", "20", "1", NA,"1"),
        numericInput("keepx2", "Max KeepX", "20", "1", NA,"1"),
      ),
      # VALIDATION
      conditionalPanel(
        condition = "input.myTabset == 'Maximum Distance' | input.myTabset == 'Error Rate' | input.myTabset == 'PLOT 13' | input.myTabset == 'Contribution' | input.myTabset == 'HeatMap'",
        HTML("<hr>"),
        selectInput(inputId = "validation",
                    label = "Validation",
                    c("Mfold", "loo"),
                    selected = "Mfold")
      ),
      
      # DOWNLOAD HEATMAP BUTTON
      #actionLink("dlcim", "Generate HeatMap", icon("arrow-up-from-bracket"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding: 10px; text-decoration:none; border-radius: 4px;"),
    ),
    # MAIN LAYOUT
    mainPanel(
      tabsetPanel(
        id = "myTabset",
        
        # HOMEPAGE
        tabPanel("Home", br(), 
                 HTML("<div style='padding:20px'>
                  <br><img src='http://mixomics.org/wp-content/uploads/2015/05/cropped-MixOmics_Banner_2.jpg'/><br>
                  <br><br><h2>Welcome to mixOmics !</h2>
                  <p>mixOmics is collaborative project between Australia (Melbourne), France (Toulouse), and Canada (Vancouver).</p>
                  <p>The core team includes Kim Anh Le Cao (University of Melbourne), Florian Rohart (Brisbane) and Sebastien Dejean (Toulouse).</p>
                  <p>We also have key contributors, past (Benoit Gautier, Francois Bartolo) and present (Al Abadi, University of Melbourne) and several collaborators including Amrit Singh (University of British Columbia), Olivier Chapleur (INRA, Paris) - it could be you too if you wish to be involved: we host many visitors with computational, statistical and biological backgrounds !</p>
                  </br>
                  <h3>Which type of data ?</h3>
                  <p>The data analysed with mixOmics may come from high throughput sequencing technologies, such as 'omics data (transcriptomics, metabolomics, proteomics, microbiome/metagenomics .) but also beyond the realm of  'omics (e.g. spectral imaging). We are currently developing new methods to integrate time-course or longitudinal omics data. Other avenues are investigated to integrate genotype data.</p>
                  </br>
                  <h3>New to mixOmics ?</h3>
                  <p>Have a look at this <a href='http://mixomics.org/2019/09/webinar-mixomics-in-50-minutes/'>webinar</a> and this <a href='https://mixomicsteam.github.io/mixOmics-Vignette/'>bookdown document</a> which present our key methods, step-by-step.</p>
                  <br></br><a style='justify-content:center;display:flex;width:20%; margin: auto;color:#fff; background-color: #337ab7;padding: 10px;border-radius: 4px; text-decoration:none' href='http://mixomics.org/'>Go to website</a>
                </div>")), 
        
        # PCA PAGE
        tabPanel("PCA", br(), 
                 HTML("<div style='padding:20px'>
                        <h2>Principal Component Analysis (PCA)</h2>
                        <p>Principal Component Analysis [1] is primarily used for the exploration and identification of the largest sources of variation within omics datasets. The aim of PCA is to reduce the dimensionality of the inputted data, while retaining as much information as possible, to allow for visualisation. PCA is a mathematical procedure that constructs novel, orthogonal axes which are linear combinations of the original axes. These new axes are the Principal Components (PCs). PCs are selected based on their explained variance and ordered in descending order. Hence, the first PC will always capture the most variance from the original data, with each subsequent PC capturing less than the one before it.</p>
                        <br> 
                      </div>"),
                 plotOutput("plot1")),
        
        # PCA MULTI PAGE
        tabPanel("PCA multi", br(),
                 HTML("<div style='padding:20px'>
                        <h2>Principal Component Analysis (PCA)</h2>
                        <p>Principal Component Analysis [1] is primarily used for the exploration and identification of the largest sources of variation within omics datasets. The aim of PCA is to reduce the dimensionality of the inputted data, while retaining as much information as possible, to allow for visualisation. PCA is a mathematical procedure that constructs novel, orthogonal axes which are linear combinations of the original axes. These new axes are the Principal Components (PCs). PCs are selected based on their explained variance and ordered in descending order. Hence, the first PC will always capture the most variance from the original data, with each subsequent PC capturing less than the one before it.</p>
                        <br>"),
                 plotOutput("plot5"), br(),
                 HTML("<h4>"),
                 textOutput("total_variance"), br(), 
                 textOutput("total_variance_percomp"),
                 HTML("</h4>"),  
                 HTML("</div>")),
        
        tabPanel("PLOT 7", br(), plotOutput("plot7")),
        
        #tabPanel("Correlation circles", br(),plotOutput("plot2"), br(), plotOutput("plot6")),
        
        tabPanel("Variance", br(),plotOutput("plot4")),
        
        tabPanel("BoxPlot", br(), plotOutput("plot8")),
        
        # PLSDA PAGE
        tabPanel("PLSDA", br(),
                 HTML("<div style='padding:20px'>
                        <h2>PLS Discriminant Analysis (PLS-DA)</h2>
                        <p>PLS was designed with a canonical (exploratory) approach and a regression (explanatory) approach in mind. Partial Least Squares - Discriminant Analysis (PLS-DA) was hence developed to allow the powerful PLS algorithm to be used for classification [1, 2]. It performs very similarly to PLS, just that the response vector y contains categorical vectors rather than continuous vectors. PLS-DA has the same advantages that PLS does, such that it operates efficiently over large dataframes and is not negatively influenced by collinearity. </p>
                        <br>"),
                 plotOutput("plot9"),
                 HTML("</div>")),
        
        tabPanel("Maximum Distance", br(),plotOutput("plot10"), br(), plotOutput("plot11"), textOutput("max_dist")),
        
        tabPanel("Error Rate", br(),plotOutput("plot12"), textOutput("error_rate_class")),
        
        tabPanel("PLOT 13", br(),plotOutput("plot13")),
        
        tabPanel("Contribution", br(),plotOutput("plot14"), br(), tableOutput("contribTab"), downloadButton("downloadContribTab", "Download as CSV")),
        
        tabPanel("HeatMap", br(), br(), 
                 HTML("<div>"),
                 plotOutput("plot15"),
                 HTML("</div>")),
      )
    )
  ),
)

# Dfinition du serveur
server <- function(input, output, session) {
  
  # GET DATA FROM FILE
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, row.names = 1, header = TRUE, sep = input$file_separator)
  })
  
  All <- reactive({
    data()
  })
  
  meta <- reactive({
    req(All())
    All() %>% dplyr::select(c('Class'))
  })
  
  X <- reactive({
    req(All())
    All() %>% dplyr::select_if(is.numeric)
  })
  
  Y <- reactive({
    req(meta())
    meta()$Class
  })
  
  result.pca <- reactive({
    req(X())
    pca(X())
  })
  
  
  All.x <- reactive({
    subset(All(), Class == input$select[1] | Class == input$select[2] | Class == input$select[3] | Class == input$select[4])
  })
  
  
  meta.x <- reactive({
    req(All.x())
    All.x() %>% dplyr::select(c('Class'))
  })
  
  X.x <- reactive({
    req(All.x())
    All.x() %>% dplyr::select_if(is.numeric)
  })
  
  Y.x <- reactive({
    req(meta.x())
    meta.x()$Class
  })
  
  final.plsda.mushroom <- reactive({
    req(X.x(), Y.x())
    plsda(X.x(),Y.x(), ncomp = input$ncomp, scale = input$scale)
  })
  
  perf.final.plsda.mushroom <- reactive({
    perf(final.plsda.mushroom(),
         validation = input$validation,
         folds = 3,
         progressBar = TRUE,
         nrepeat = 50)
  })
  
  background.max <- reactive({
    background.predict(final.plsda.mushroom(), comp.predicted = 2, dist = 'max.dist')
  })
  
  list.keepX <- reactive({
    c(input$keepx1, input$keepx2) #retain only max 50 features, minimum 5 per component
  })
  
  tune.splsda.mushroom <- reactive({
    tune.splsda(X.x(), Y.x(), ncomp = input$ncomp,
                validation = input$validation,
                folds = 3,
                progressBar = TRUE,
                dist = 'max.dist',
                nrepeat = 10,
                
                if (input$keepxc == TRUE) {
                  test.keepX = list.keepX()
                } else {
                  test.keepX = c(5, 10, 15)
                }
    )
  })
  
  splsda.mushroom <- reactive({
    splsda(X.x(), Y.x(), ncomp = input$ncomp,keepX = select.keepX())
  })
  
  final.pca.multi <- reactive({
    pca(X(), ncomp = input$ncomp,center = input$center, scale = input$scale)
  })
  
  result.pca.multi2 <- reactive({
    pca(X(), center = input$center, scale = input$scale, ncomp = input$ncomp)
  })
  
  result.pca.multi2 <- reactive({
    pca(X(), center = input$center, scale = input$scale, ncomp = input$ncomp)
  })
  
  tune.pca.multi <- reactive({
    tune.pca(X(), ncomp = input$ncomp,scale = input$scale, center = input$center)
  })
  
  perf.splsda.mushroom <- reactive({
    perf(splsda.mushroom(),
         folds = 3,
         validation = input$validation,
         dist = "max.dist",
         progressBar = TRUE,
         nrepeat = 50)
  })
  
  ncomp <- reactive({
    tune.splsda.mushroom()$choice.ncomp$ncomp()
  })
  
  select.keepX <- reactive({
    tune.splsda.mushroom()$choice.keepX[1:2]
  })
  
  contribTable <- reactive({
    plotLoadings(splsda.mushroom(),
                 comp = input$comp,
                 method = 'mean',
                 contrib = 'max')
  })
  
  
  # DOWNLOADS
  observeEvent(input$dlcim, {
    legend=list(legend = levels(Y.x()),  # set of classes
                title = "line",          # legend title
                cex = 0.8)               # legend size
    pdf(file = "C:/Users/Administrateur/Downloads/HeatmapKeepXoff.pdf", width = 7.3, height = 7.3)
    cim <- cim(splsda.mushroom(), row.sideColors = color.mixo(as.factor(Y.x())),
               legend = legend, center = input$center, scale = input$scale, transpose = TRUE)
    dev.off()
    shinyalert("HeatMap downloaded to C:/Users/Administrateur/Downloads/")
  })
  
  output$downloadContribTab <- downloadHandler(
    filename = function(){"contribTab.csv"},
    content = function(filename){
      write.csv(contribTable(), filename)
    }
  )
  
  # DOWNLOAD PLOT
  observeEvent(input$dlPlot, {
    filename = paste("C:/Users/Administrateur/Downloads/", input$myTabset, ".pdf", sep="")
    pdf(file = filename, width = 7.3, height = 7.3)
    if (input$myTabset == "PCA") {
      plotIndiv(result.pca(), 
                ind.names = input$ind_names,
                group = Y(), 
                ellipse = input$ellipse, 
                abline = input$abline, 
                star = input$star, 
                legend = input$legend,
                title = input$title1,
                legend.title = input$legend1)
    } else if (input$myTabset == "PCA multi") {
      plotIndiv(final.pca.multi(),
                comp = c(1, input$ncomp), #show component to display
                ind.names = input$ind_names, #tag individual names
                group = Y(),# color individu according to group (cell line here)
                ellipse = input$ellipse,#circle group in ellipse
                abline = input$abline,#coordinates
                star = input$star, 
                title = input$title2,
                legend = input$legend, 
                legend.title = input$legend2)
    } else if (input$myTabset == "PLOT 7") {
      biplot(final.pca.multi(), group = Y(), legend.title = input$legend4, cutoff = input$cutoff, ind.names = input$ind_names)
    } else if (input$myTabset == "Variance") {
      plot(tune.pca.multi())
    } else if (input$myTabset == "BoxPlot") {
      selectedX <- X()[paste("X", input$selected_variable, sep="")]
      X.scale <- scale(selectedX, center = input$center, scale = input$scale)
      boxplot(X.scale ~ Y(),
              col = color.mixo(1:9),
              xlab = 'Groups',
              ylab = 'Mean peak Area',
              par(cex.axis = 0.5), # Font size
              main = paste("X", input$selected_variable, sep=""))
    } else if (input$myTabset == "PLSDA") {
      plotIndiv(final.plsda.mushroom(),
                ind.names = input$ind_names,
                legend=input$legend,
                comp=c(input$plsda_comp1, input$plsda_comp2),
                ellipse = input$ellipse,
                title = input$title3,
                abline = input$abline,
                legend.title = input$legend3)
    } else if (input$myTabset == "Maximum Distance") {
      barplot(perf.final.plsda.mushroom()$error.rate.class$max.dist)
      plotIndiv(final.plsda.mushroom(),
                comp = 1:(input$ncomp), 
                group = Y.x(),
                ind.names = input$ind_names,
                title = 'Maximum distance',
                legend = input$legend,
                background = background.max())
    } else if (input$myTabset == "Error Rate") {
      plot(tune.splsda.mushroom(), sd = TRUE)
    } else if (input$myTabset == "PLOT 13") {
      barplot(perf.splsda.mushroom()$error.rate.class$max.dist)
    } else if (input$myTabset == "Contribution") {
      plotLoadings(splsda.mushroom(),
                   comp = input$comp,
                   method = 'mean',
                   contrib = 'max')
    } else if (input$myTabset == "HeatMap") {
      legend=list(legend = levels(Y.x()),  # set of classes
                  title = "Species",          # legend title
                  cex = 0.8) 
      cim(splsda.mushroom(), row.sideColors = color.mixo(as.factor(Y.x())),
          legend = legend, 
          center = input$center, scale = input$scale, 
          transpose = TRUE)
    }
    
    dev.off()
    shinyalert(paste("Downloaded to ", filename))
  })
  
  
  
  # UPDATE CLASSES
  observe({
    updateCheckboxGroupInput(session, "select", choices = classess(), inline = TRUE)
  })
  
  classess <- reactive({
    c(unique(meta()$Class))
  })
  
  observe({
    updateNumericInput(session, "comp", max = input$ncomp, value=1)
  })
  
  # OUTPUTS VALUES
  output$max_dist <- renderText({
    paste("Max dist : ", perf.final.plsda.mushroom()$error.rate$BER[, 'max.dist'])
  })
  
  output$error_rate_class <- renderText({
    paste("Error rate class : ", perf.splsda.mushroom()$error.rate.class)
  })
  
  output$total_variance <- renderText({
    paste("Total Variance : ", final.pca.multi()$var.tot)
  })
  
  output$keepxc <- renderText({
    input$keepxc
  })
  
  output$total_variance_percomp <- renderText({
    var_text <- "" 
    for (x in 1:length(final.pca.multi()$prop_expl_var$X)) {
      var_text <- paste(var_text, "Component",x," variance :", final.pca.multi()$prop_expl_var$X[x], "--") 
    }
    var_text 
  })
  
  output$contribTab <- renderTable(contribTable(),
                                   striped = TRUE,
                                   bordered = TRUE,
                                   align = 'c',
                                   width = '100%')
  
  
  # SORTIES TEST
  output$meta <- renderText({
    input$keepxc
  })
  #output$tab <- renderTable()
  
  # HOME
  output$home <- renderText({
    h1("Welcome to Mixomics")
  })
  
  # OUTPUTS PLOTS
  # PLOT 1 : PCA
  output$plot1 <- renderPlot({
    plotIndiv(result.pca(), 
              ind.names = input$ind_names,
              group = Y(), 
              ellipse = input$ellipse, 
              abline = input$abline, 
              star = input$star, 
              legend = input$legend,
              title = input$title1,
              legend.title = input$legend1)
  })
  
  # PLOT 2
  output$plot2 <- renderPlot({
    plotVar(result.pca(), comp = c(1, 2))
  })
  
  # PLOT 4 : Variance
  output$plot4 <- renderPlot({
    plot(tune.pca.multi())
  })
  
  # PLOT 5 : PCA MULTI
  output$plot5 <- renderPlot({
    
    plotIndiv(final.pca.multi(),
              comp = c(1, input$ncomp), #show component to display
              ind.names = input$ind_names, #tag individual names
              group = Y(),# color individu according to group (cell line here)
              ellipse = input$ellipse,#circle group in ellipse
              abline = input$abline,#coordinates
              star = input$star, 
              title = input$title2,
              legend = input$legend, 
              legend.title = input$legend2)
  })
  
  # PLOT 6
  output$plot6 <- renderPlot({
    
    plotVar(final.pca.multi(), comp = c(1, input$ncomp),
            var.names = TRUE,
            cex = 3, # To change the font size
            cutoff = input$cutoff, # For further cutoff
            title = 'PCA comp 1 - 2')
  })
  
  # PLOT 7
  output$plot7 <- renderPlot({
    
    biplot(final.pca.multi(), group = Y(), legend.title = input$legend4, cutoff = input$cutoff, ind.names = input$ind_names)
  })
  
  # PLOT 8 : BOXPLOT
  output$plot8 <- renderPlot({
    selectedX <- X()[paste("X", input$selected_variable, sep="")]
    X.scale <- scale(selectedX, center = input$center, scale = input$scale)
    boxplot(X.scale ~ Y(),
            col = color.mixo(1:9),
            xlab = 'Groups',
            ylab = 'Mean peak Area',
            par(cex.axis = 0.5), # Font size
            main = paste("X", input$selected_variable, sep=""))
  })
  
  # PLOT 9 : PLSDA
  output$plot9 <- renderPlot({
    if (length(input$select) == 0) {
      text("Nope")
    }
    plotIndiv(final.plsda.mushroom(),
              ind.names = input$ind_names,
              legend=input$legend,
              comp=c(input$plsda_comp1, input$plsda_comp2),
              ellipse = input$ellipse,
              title = input$title3,
              abline = input$abline,
              legend.title = input$legend3)
  })
  
  # PLOT 10 : MAX DIST#1
  output$plot10 <- renderPlot({
    barplot(perf.final.plsda.mushroom()$error.rate.class$max.dist)
  })
  
  # PLOT 11 : MAX DIST#2
  output$plot11 <- renderPlot({
    
    plotIndiv(final.plsda.mushroom(),
              comp = 1:(input$ncomp), 
              group = Y.x(),
              ind.names = input$ind_names,
              title = 'Maximum distance',
              legend = input$legend,
              background = background.max())
    
    list.keepX
  })
  
  # PLOT 12 : ERROR RATE
  output$plot12 <- renderPlot({
    plot(tune.splsda.mushroom(), sd = TRUE)
  })
  
  # PLOT 13
  output$plot13 <- renderPlot({
    barplot(perf.splsda.mushroom()$error.rate.class$max.dist)
  })
  
  # PLOT 14 CONTRIBUTION
  output$plot14 <- renderPlot({
    
    plotLoadings(splsda.mushroom(),
                 comp = input$comp,
                 method = 'mean',
                 contrib = 'max')
  })
  
  # PLOT 15 HEATMAP
  output$plot15 <- renderPlot({
    legend=list(legend = levels(Y.x()),  # set of classes
                title = "Species",          # legend title
                cex = 0.8) 
    #png(file = "C:/Users/Administrateur/Desktop/Arthur/R/big.png", width = 800, height = 800)
    cim(splsda.mushroom(), row.sideColors = color.mixo(as.factor(Y.x())),
        legend = legend, 
        center = input$center, scale = input$scale, 
        transpose = TRUE)
    #dev.off()
  })
  
}

# Run the Shiny app
shinyApp(ui, server)

