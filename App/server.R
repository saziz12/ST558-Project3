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

titanic_clean <- titanic[complete.cases(titanic), ]

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
  
  
  # Modeling Page
  # Model Fitting
  observeEvent(input$fit_models_button, {
    # Split the data into training and testing sets
    set.seed(123) # For reproducibility
    train_indices <- createDataPartition(titanic_clean$survived, p = input$train_test_split, list = FALSE)
    train_data <- titanic_clean[train_indices, ]
    test_data <- titanic_clean[-train_indices, ]
    
    # Fit the models based on user selection
    model_fit_stats <- fit_models(train_data, test_data)
    
    # Store the model fit statistics for rendering
    output$model_fit_stats <- renderPrint({
      model_fit_stats
    })
  })
  
  # Function to fit models
  fit_models <- function(train_data, test_data) {
    model_fit_stats <- list()
    
    # Model 1: Multiple Linear Regression
    if (input$fit_model_mlr) {
      selected_vars_model1 <- input$selected_vars_model1
      if (length(selected_vars_model1) > 0) {
        lm_formula <- as.formula(paste("survived1 ~", paste(selected_vars_model1, collapse = " + ")))
        model_fit <- lm(lm_formula, data = train_data)
        predictions_train <- predict(model_fit, newdata = train_data)
        predictions_test <- predict(model_fit, newdata = test_data)
        rmse_train <- sqrt(mean((train_data$survived1 - predictions_train)^2))
        rmse_test <- sqrt(mean((test_data$survived1 - predictions_test)^2))
        model_fit_stats$model_mlr <- list(
          model_type = "Multiple Linear Regression",
          rmse_train = rmse_train,
          rmse_test = rmse_test,
          summary_stats_train = summary(model_fit)
        )
      }
    }
    
    # Model 2: Regression Tree
    if (input$fit_model_tree) {
      selected_vars_model2 <- input$selected_vars_model2
      if (length(selected_vars_model2) > 0) {
        formula <- as.formula(paste("survived1 ~", paste(selected_vars_model2, collapse = "+")))
        model_fit <- rpart(formula, data = train_data, method = "class")
        predictions_train <- predict(model_fit, newdata = train_data, type = "class")
        predictions_test <- predict(model_fit, newdata = test_data, type = "class")
        accuracy_train <- sum(diag(table(train_data$survived1, predictions_train))) / nrow(train_data)
        accuracy_test <- sum(diag(table(test_data$survived1, predictions_test))) / nrow(test_data)
        model_fit_stats$model_tree <- list(
          model_type = "Regression Tree",
          accuracy_train = accuracy_train,
          accuracy_test = accuracy_test,
          summary_stats_train = summary(model_fit)
        )
      }
    }
    
    # Model 3: Random Forest
    if (input$fit_model_rf) {
      selected_vars_model3 <- input$selected_vars_model3
      if (length(selected_vars_model3) > 0) {
        formula <- as.formula(paste("survived1 ~", paste(selected_vars_model3, collapse = "+")))
        model_fit <- randomForest(formula, data = train_data)
        predictions_train <- predict(model_fit, newdata = train_data)
        predictions_test <- predict(model_fit, newdata = test_data)
        accuracy_train <- sum(predictions_train == train_data$survived1) / nrow(train_data)
        accuracy_test <- sum(predictions_test == test_data$survived1) / nrow(test_data)
        model_fit_stats$model_rf <- list(
          model_type = "Random Forest",
          accuracy_train = accuracy_train,
          accuracy_test = accuracy_test,
          variable_importance = importance(model_fit),
          confusion_matrix_train = table(train_data$survived1, predictions_train),
          confusion_matrix_test = table(test_data$survived1, predictions_test)
        )
      }
    }
    
    return(model_fit_stats)
  }
  
  
  # Display fit statistics for each model
  observeEvent(input$fit_models_button, {
    output$model_fit_stats <- renderPrint({
      results <- fit_models(train_data, test_data)
      model_mlr <- results$model_mlr
      model_tree <- results$model_tree
      model_rf <- results$model_rf
      
      output_text <- ""
      
      if (!is.null(model_mlr)) {
        output_text <- paste(output_text, "Model 1 -", model_mlr$model_type, "\n")
        output_text <- paste(output_text, "Root Mean Squared Error (RMSE) - Training Set:", model_mlr$rmse_train, "\n")
        output_text <- paste(output_text, "Root Mean Squared Error (RMSE) - Test Set:", model_mlr$rmse_test, "\n")
        output_text <- paste(output_text, "Summary Statistics on Training Set:\n")
        output_text <- paste(output_text, capture.output(model_mlr$summary_stats_train), collapse = "\n")
        output_text <- paste(output_text, "\n\n")
      }
      if (!is.null(model_tree)) {
        output_text <- paste(output_text, "Model 2 -", model_tree$model_type, "\n")
        output_text <- paste(output_text, "Accuracy - Training Set:", model_tree$accuracy_train, "\n")
        output_text <- paste(output_text, "Accuracy - Test Set:", model_tree$accuracy_test, "\n")
        output_text <- paste(output_text, "Summary Statistics on Training Set:\n")
        output_text <- paste(output_text, capture.output(model_tree$summary_stats_train), collapse = "\n")
        output_text <- paste(output_text, "\n\n")
      }
      if (!is.null(model_rf)) {
        output_text <- paste(output_text, "Model 3 -", model_rf$model_type, "\n")
        output_text <- paste(output_text, "Accuracy - Training Set:", model_rf$accuracy_train, "\n")
        output_text <- paste(output_text, "Accuracy - Test Set:", model_rf$accuracy_test, "\n")
        output_text <- paste(output_text, "Variable Importance:\n")
        output_text <- paste(output_text, "Variable Importance:\n")
        output_text <- paste(output_text, capture.output(model_rf$variable_importance), collapse = "\n")
        output_text <- paste(output_text, "\n\n")
      }
      
      if (output_text == "") {
        output_text <- "No models selected for fitting."
      }
      
      cat(output_text)
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