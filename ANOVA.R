# Load the required libraries
library(shiny)
library(DT)
library(car)
library(multcomp)
library(tidyr)
library(nortest)
library(lmtest)

# Define the UI
ui <- fluidPage(
  titlePanel("Tugas UAS BI ANNOVA"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Pilih Data"),
      textAreaInput("pasteData", "Salin Data", rows = 10),
      actionButton("loadData", "Muat Data"),
      
      # ANOVA options
      selectInput("factorVar", "Variabel Faktor", ""),
      selectInput("responseVar", "Variabel Respon", ""),
      actionButton("runANOVA", "Uji ANOVA"),
    ),
    mainPanel(
      DTOutput("dataTable"),
      verbatimTextOutput("anovaSummary"),
      verbatimTextOutput("assumptionsSummary"),
      verbatimTextOutput("tukeySummary")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  anovaModel <- reactiveVal(NULL)
  factorLevels <- reactiveVal(NULL)
  tukeyResult <- reactiveVal(NULL)
  
  # Set the default dataset
  defaultData <- as.data.frame(read.csv("data//ads.csv", sep = ";"))
  defaultDataLong <- as.data.frame(pivot_longer(defaultData, cols = 2:4, names_to = "AdPlacement",
                                                values_to = "CTR", names_repair = "unique"))
  names(defaultDataLong) <- c("Day", "AdPlacement", "CTR")
  data(defaultDataLong)
  
  updateSelectInput(session, "factorVar", choices = colnames(defaultDataLong))
  updateSelectInput(session, "responseVar", choices = colnames(defaultDataLong))
  
  observeEvent(input$file, {
    req(input$file)
    data(read.csv(input$file$datapath))
    updateSelectInput(session, "factorVar", choices = colnames(data()))
    updateSelectInput(session, "responseVar", choices = colnames(data()))
  })
  
  observeEvent(input$pasteData, {
    req(input$pasteData)
    newData <- read.table(text = input$pasteData, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    newDataLong <- as.data.frame(pivot_longer(newData, cols = 2:4, names_to = "AdPlacement",
                                              values_to = "CTR", names_repair = "unique"))
    names(newDataLong) <- c("Day", "AdPlacement", "CTR")
    data(newDataLong)
    updateSelectInput(session, "factorVar", choices = colnames(newDataLong))
    updateSelectInput(session, "responseVar", choices = colnames(newDataLong))
  })
  
  observeEvent(input$runANOVA, {
    req(data(), input$factorVar, input$responseVar)
    
    # Check if the selected variables are valid
    if (!all(c(input$factorVar, input$responseVar) %in% colnames(data()))) {
      cat("Error: Selected variables are not valid.")
      return(NULL)
    }
    
    # Check for missing or infinite values in the selected variables
    if (any(is.na(data()[[input$factorVar]])) || any(is.infinite(data()[[input$factorVar]])) ||
        any(is.na(data()[[input$responseVar]])) || any(is.infinite(data()[[input$responseVar]]))) {
      cat("Error: Missing or infinite values detected in the selected variables.")
      return(NULL)
    }
    
    # Check if there is enough variation in the response variable
    if (length(unique(data()[[input$responseVar]])) <= 1) {
      cat("Error: Insufficient variation in the response variable for ANOVA.")
      return(NULL)
    }
    
    # Perform ANOVA
    formula <- as.formula(paste(input$responseVar, "~", input$factorVar))
    anovaResult <- tryCatch(
      aov(formula, data = data()),
      error = function(e) {
        cat("Error: Unable to run ANOVA. Check the data and try again.")
        return(NULL)
      }
    )
    
    # Check if ANOVA was successful
    if (is.null(anovaResult)) {
      return(NULL)
    }
    
    # Save the ANOVA model and factor levels for Tukey's HSD
    anovaModel(anovaResult)
    factorLevels(levels(data()[[input$factorVar]]))
    
    # Display ANOVA summary
    output$anovaSummary <- renderPrint({
      summary(anovaResult)
    })
    
    # Assumptions tests
    assumptionsText <- character()
    
    # Check residuals for normality
    lillieTestResult <- lillie.test(residuals(anovaResult))
    if (lillieTestResult$p.value >= 0.05) {
      assumptionsText <- c(assumptionsText, paste("Lilliefors p-value for residuals =", lillieTestResult$p.value, "Residuals are normally distributed"))
    } else {
      assumptionsText <- c(assumptionsText, paste("Lilliefors p-value for residuals =", lillieTestResult$p.value, "Residuals are not normally distributed"))
    }
    
    # Check residuals for homoscedasticity
    bpTestResult <- bptest(anovaResult, studentize = TRUE)
    if (bpTestResult$p.value >= 0.05) {
      assumptionsText <- c(assumptionsText, paste("Breusch Pagan test p-value for residuals =", bpTestResult$p.value, "Residuals exhibit homoscedasticity"))
    } else {
      assumptionsText <- c(assumptionsText, paste("Breusch Pagan test p-value for residuals =", bpTestResult$p.value, "Residuals exhibit heteroscedasticity"))
    }
    
    # Display assumptions summary
    output$assumptionsSummary <- renderPrint({
      cat(assumptionsText, sep = "\n")
    })
    
    # Conduct Tukey's HSD test
    tryCatch(
      {
        tukeyResult(TukeyHSD(anovaResult))
        
        # Display Tukey's HSD summary
        output$tukeySummary <- renderPrint({
          cat("Tukey's HSD Test:\n")
          print(tukeyResult())
        })
      },
      error = function(e) {
        cat("Error: Unable to run Tukey's HSD. Check the ANOVA model and try again.\n")
      }
    )
  })
  
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
}

# Run the app
shinyApp(ui, server)