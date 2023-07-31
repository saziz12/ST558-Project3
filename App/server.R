# Load required libraries
library(shiny)
library(caret)
library(rpart)
library(randomForest)
library(ggplot2)
library(plotly)

# Load Titanic dataset
titanic <- read.csv('../dataset/titanic.csv')
titanic$pclass1 <- as.factor(titanic$pclass) 
titanic$survived1 <- as.factor(titanic$survived) 
titanic$sex1 <- as.factor(titanic$sex) 
titanic$embarked1 <- as.factor(titanic$embarked) 

# Server
shinyServer(function(input, output, session) {
  
  # Data Exploration Page
  # Create a reactive function for selected variables
  selectedVars <- eventReactive(input$runGraph, {
    if (input$GraphType == "bar") {
      return(input$charvar)
    } else if (input$GraphType == "hist") {
      return(input$numvar)
    } else {
      return(c(input$numvar1, input$numvar2))
    }
  })
  
  # Generate the selected plot using plotly
  output$final_plot <- renderPlotly({
    if (input$GraphType == "bar") {
      # Pie chart
      filtered_data <- titanic[!is.na(titanic[[selectedVars()]]), ]
      if (nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      ggplot(data = filtered_data, aes_string(x = selectedVars())) +
        geom_bar(fill = "dodgerblue", color = "black") +
        labs(title = paste("Bar Graph of", selectedVars())) +
        theme_minimal()
      
    } else if (input$GraphType == "hist") {
      # Histogram
      filtered_data <- titanic[!is.na(titanic[[selectedVars()]]), ]
      if (nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      ggplot(data = filtered_data, aes_string(x = selectedVars())) +
        geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black") +
        labs(title = paste("Histogram of", selectedVars())) +
        theme_minimal()
      
    } else {
      # Scatter plot
      filtered_data <- titanic[complete.cases(titanic[selectedVars()]), ]
      if (nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      ggplot(data = filtered_data, aes_string(x = selectedVars()[1], y = selectedVars()[2])) +
        geom_point(alpha = 0.6) +
        labs(title = paste("Scatter Plot of", selectedVars()[1], "vs", selectedVars()[2])) +
        theme_minimal()
    }
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
