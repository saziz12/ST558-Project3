# Load required libraries
library(shiny)
library(caret)
library(rpart)
library(randomForest)
library(ggplot2)
library(plotly)

# Load Titanic dataset
titanic <- read.csv('../dataset/titanic.csv')
titanic$pclass1 <- as.character(titanic$pclass) 
titanic$survived1 <- as.character(titanic$survived) 
titanic$sex1 <- as.character(titanic$sex) 
titanic$embarked1 <- as.character(titanic$embarked) 

# Server
shinyServer(function(input, output, session) {
  
  # Data Exploration Page
  # Create a reactive function for selected variables
  selectedVars <- eventReactive(input$runGraph, {
    if (input$GraphType == "box") {
      return(input$charvar)
    } else if (input$GraphType == "hist") {
      return(input$numvar)
    } else if (input$GraphType == "scatter") {
      return(list(input$numvar1, input$numvar2))
    } else {
      return(NULL)
    }
  })
  
  # Generate the selected plot using plotly
  output$final_plot <- renderPlotly({
    if (input$GraphType == "box") {
      # Box plot
      if (input$charvar %in% colnames(titanic)) {
        filtered_data <- titanic[!is.na(titanic[[input$charvar]]), ]
        if (nrow(filtered_data) == 0) {
          return(NULL)
        }
        
        ggplot(data = filtered_data, aes_string(x = input$charvar, y = "age")) +
          geom_boxplot() +
          labs(title = paste("Box Plot of Age by", input$charvar),
               x = input$charvar,
               y = "Age") +
          theme_minimal()
        
      } else {
        return(NULL)
      }
      
    } else if (input$GraphType == "hist") {
      # Histogram
      filtered_data <- titanic[!is.na(titanic[[selectedVars()]]), ]
      if (nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      ggplot(data = filtered_data, aes_string(x = selectedVars())) +
        geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black") +
        labs(title = paste("Histogram of", selectedVars()),
             x = selectedVars(),
             y = "Frequency") +
        theme_minimal()
      
    } else if (input$GraphType == "scatter") {
      # Scatter plot
      filtered_data <- titanic[complete.cases(titanic[selectedVars()]), ]
      if (nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      ggplot(data = filtered_data, aes_string(x = selectedVars()[[1]], y = selectedVars()[[2]])) +
        geom_point(alpha = 0.6) +
        labs(title = paste("Scatter Plot of", selectedVars()[[1]], "vs", selectedVars()[[2]]),
             x = selectedVars()[[1]],
             y = selectedVars()[[2]]) +
        theme_minimal()
    } else {
      return(NULL)
    }
  })
  
  # Summary Statistics
  # Reactive expression for selected variable
  selectedVar <- reactive({
    input$sumvar
  })
  
  # Reactive expression for summary statistics
  summaryStats <- reactive({
    summary(titanic[[selectedVar()]])
  })
  
  # Observe event to compute summary statistics on button click
  observeEvent(input$runSum, {
    output$summarystat <- renderPrint({
      summaryStats()
    })
  })
  
  # Data Page
  # Display the Titanic dataset in a DataTable
  output$titanic_table <- renderDT({
    datatable(titanic)
  })
  
  # Download the dataset as a CSV file
  output$download_data <- downloadHandler(
    filename = function() { paste("titanic_dataset.csv") },
    content = function(file) { 
      write.csv(titanic, file) 
    }
  )
})
