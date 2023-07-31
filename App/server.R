# Load required libraries
library(shiny)
library(caret)
library(rpart)
library(randomForest)
library(plotly)

# Load Titanic dataset
titanic <- read.csv('../dataset/titanic.csv')

# Server
shinyServer(function(input, output, session) {
  
  
  # Data Exploration Page
  # Generate dropdown menu options for numerical variables
  numeric_vars <- reactive({
    sapply(titanic, is.numeric)
  })
  
  observe({
    updateSelectInput(session, "selected_var", choices = names(numeric_vars())[numeric_vars()])
  })
  
  # Generate histogram of selected variable (using plotly for interactivity)
  output$var_histogram <- renderPlotly({
    selected_var <- input$selected_var
    gg_var_histogram <- ggplot(titanic, aes_string(x = selected_var)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
      labs(title = paste("Distribution of", selected_var), x = selected_var, y = "Count")
    
    # Convert ggplot to plotly
    ggplotly(gg_var_histogram)
  })
  
  # Generate table for categorical variable summaries
  output$categorical_summaries <- renderTable({
    summaries <- lapply(titanic, function(column) {
      if (is.factor(column) || is.character(column)) {
        table(column)
      } else {
        NULL
      }
    })
    summaries <- Filter(Negate(is.null), summaries) # Remove NULL elements
    summaries
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
      })
})
