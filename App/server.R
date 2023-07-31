# Load required libraries
library(shiny)
library(caret)
library(rpart)
library(randomForest)

# Load Titanic dataset
titanic <- read.csv('../dataset/titanic.csv')

# Server
shinyServer(function(input, output, session) {
  
  
  
  # Data Page
  # Display the Titanic dataset in a DataTable
  output$titanic_table <- renderDT({
    datatable(titanic)
  })
  
  # Download the dataset as a CSV file
  output$download_data <- downloadHandler(
    filename = function() { paste("titanic_dataset.csv") },
    content = function(file) { write.csv(titanic, file) }
  )
})
