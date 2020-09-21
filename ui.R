#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        #theme = shinytheme("cyborg"),
        useShinyjs(),
        # title displaying my name
        title = "Modeling COVID Data",
        # Application title
        tabsetPanel(
            tabPanel("EDA",
                     h3("Dataset - Covid Data before cleaning"),
                     tabsetPanel(
                         tabPanel("Summary",
                                  h4("Description of Covid Data"),
                                  verbatimTextOutput(outputId="description"),
                                  HTML("<br/>"),
                                  h4("Listed Below is the Summary Of Covid Data:"),
                                  htmlOutput(outputId = "Summary"),
                         ),
                         tabPanel("Raw Data",
                                  h4("Listed Below is the Entire Covid Data:"),
                                  DT::dataTableOutput(outputId = "RawData")
                         ),
                         tabPanel("Mosaic Plot",
                                  h4("A Mosaic Plot:"),
                                  selectizeInput(inputId = "Variables", label = "Show variables:", choices = factorVariables, multiple = TRUE, selected = sample(factorVariables, 3)),
                                  plotOutput(outputId = "Mosaic")
                         ),
                         tabPanel("Pairs Plot", 
                                  h3("A ggplot2 Generalized Pairs Plot:"),
                                  selectizeInput(inputId = "Pairs", label = "Select Variables:", choices = continuousVariables, multiple = TRUE, selected = sample(continuousVariables, 5)),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "ggPairs"))
                         ),
                         tabPanel("Correlation Chart", 
                                  h4("A corrgram::corrgram Correlation Chart:"),
                                  selectizeInput(inputId = "Correlation", label = "Select Variables:", choices = continuousVariables, multiple = TRUE, selected = sample(continuousVariables, 5)),
                                  checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                                  selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                                  selectInput(inputId = "Group", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO"),
                                  plotOutput(outputId = "Corrgram"),
                                  
                         ),
                         tabPanel("Missing Data",
                                  h4("Missing Data"),
                                  selectizeInput(inputId = "MissingInput", label = "Show Variables:", choices = colnames(dat), multiple = TRUE, selected = colnames(dat)),
                                  checkboxInput(inputId = "Cluster", label = "Cluster Missingness", value = FALSE),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "Missing")),
                                  HTML("<br/>"),
                                  h4("Missingness of Variables "),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "MissingVariables")),
                                  HTML("<br/>"),
                                  h4("Missingness Of Rows"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "MissingCase")),
                                  HTML("<br/>"),
                                  h4("Pattern Of Missingness Using An Upset Plot"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "MissingUpset")),
                         ),
                         tabPanel("Box Plot",
                                  h4 = "Box Plot of the Data:",
                                  plotOutput(outputId = "Boxplot"),
                                  selectInput(inputId = "box", label = "Select Variables:", choices = continuousVariables, multiple = FALSE, selectize = TRUE, selected = continuousVariables[1]),
                                  checkboxInput(inputId = "standardise", label = "Center and Standardize", value = FALSE),
                                  checkboxInput(inputId = "outliers", label = "Show Outliers", value = TRUE),
                                  sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
                         ),
                         tabPanel("Rising Value Chart",
                                  h4("Rising Value Chart:"),
                                  selectizeInput(inputId = "RisingInput", label = "Select Variables:", choices = continuousVariables, multiple = TRUE, selected = sample(continuousVariables, 5)),
                                  plotOutput(outputId = "RisingValue")
                         ),
                         tabPanel("Time Series",
                                  h3("Time Series Data"),
                                  selectizeInput(inputId = "TimeSeriesInput", label = "Select Variables:", choices = continuousVariables, multiple = TRUE, selected = sample(continuousVariables, 2)),
                                  shinycssloaders::withSpinner(plotlyOutput(outputId = "TimeSeries"))
                         ),
                         tabPanel("Strategy",
                                  h2("The Strategy to Deal with Missing Data"),
                                  HTML("<br/>"),
                                  h3("Remove Variables That Is Missing > 50%"),
                                  verbatimTextOutput(outputId="remVariables"),
                                  HTML("<br/>"),
                                  h3("Remove Observations That Is Missing > 50%"),
                                  verbatimTextOutput(outputId="remObservations")
                                  
                         )
                     )
                     
            ),
            tabPanel("Modeling",
                     h3("Dataset - Covid Data"),
                     tabsetPanel(
                         tabPanel("Summary",
                                  h4("Description of Covid Data After Cleaning"),
                                  verbatimTextOutput(outputId="cdescription"),
                                  HTML("<br/>"),
                                  h4("Listed Below is the Summary Of Covid Data After Cleaning:"),
                                  htmlOutput(outputId = "cSummary"),
                         ),
                         tabPanel("Raw Data",
                                  h4("Listed Below is the Entire Covid Data:"),
                                  DT::dataTableOutput(outputId = "cRawData")
                         ),
                         tabPanel("Missing Variable Data & Variable Importance",
                                  h4("Variable Missingness"),
                                  sliderInput(inputId = "sliVariableMissing", label = "Variables With % Missing Greater Than", min = 0, max = 100, step = 5, value = 50),
                                  verbatimTextOutput(outputId = "VariableMissingness"),
                                  checkboxInput(inputId = "chkRemoveMissingVariables", label = "Remove These Missing Variables?", value = FALSE),
                                  HTML("</br>"),
                                  h4("Variable Importance"),
                                  verbatimTextOutput(outputId = "VariableImportance"),
                                  HTML("</br>"),
                                  h4("Plot of Variable Importance"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "variableImportanceGB"))
                         ),
                         tabPanel("Observations Missingness",
                                  h4("Observations Missingness"),
                                  sliderInput(inputId = "sliObservationMissing", label = "Observations With % Missing Greater Than", min = 0, max = 100, step = 5, value = 50),
                                  verbatimTextOutput(outputId = "ObservationMissingness"),
                                  checkboxInput(inputId = "chkRemoveMissingObservations", label = "Remove These Missing Observations?", value = FALSE),
                         ),
                         tabPanel("Missing Value COrrelation",
                                  h4("Missing Value Correlation"),
                                  checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                                  selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                                  selectInput(inputId = "Group", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "MissingCorrelation"))
                         ),
                         tabPanel("Informative Missingness",
                                  h4("Informative Missingness"),
                                  selectizeInput(inputId = "sipInformedMissing", label = "Look at the Plots Below and Select Variables That Have Informed Missingness:", choices = names(which(sapply(cdat, class) != 'factor')), multiple = TRUE, selected = ""),
                                  HTML("</br>"),
                                  h3("A ggplot2 Generalized Pairs Plot:"),
                                  selectizeInput(inputId = "PairsVariables", label = "Select Variables:", choices = names(which(sapply(cdat, class) != 'factor')), multiple = TRUE, selected = "DEATHRATE"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "correlationPairs"))
                         ),
                         tabPanel("Predicting Missingness",
                                  h4("Predicting Missingness"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "Missingness"))
                         ),
                         tabPanel("Train/Test Split",
                                  h4("Splitting the Train  data"),
                                  DT::dataTableOutput(outputId = "SplitTrainData"),
                                  HTML("<br/>"),
                                  h4("Splitting the  Test data"),
                                  DT::dataTableOutput(outputId = "SplitTestData"),
                         ),
                         tabPanel("Model & Predict",
                                  h4("Train Model and Predict"),
                                  shinycssloaders::withSpinner(verbatimTextOutput(outputId = "ModelPredict")),
                         ),
                         tabPanel("Model Accuracy & Outliers",
                                  h4(" Model Accuracy Information"),
                                  verbatimTextOutput(outputId = "ModelInfo"),
                                  HTML("</br>"),
                                  h4("Model Outliers"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "ModelOutliers"))
                         ),
                         tabPanel("Residuals Plot",
                                  h4("Residuals Histogramm Plot"),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "ResidualsHistPlot")),
                                  HTML("</br>"),
                                  h4("Residuals Box Plot"),
                                  sliderInput(inputId = "IQRrange", label = "IQR Multiplier", min = 0, max = 10, step = 0.1, value = 1.5),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "ResidualsBoxPlot"))
                         )
                         
                     )
            )
        )
    )
)