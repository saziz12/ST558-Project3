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
titanic_clean$survived2 <- as.factor(titanic_clean$survived2)

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
        
        ggplot(data = filtered_data, aes(x = input$charvar, y = "age")) +
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
      
      ggplot(data = filtered_data, aes(x = selectedVars())) +
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
      
      ggplot(data = filtered_data, aes(x = selectedVars()[[1]], y = selectedVars()[[2]])) +
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
  
  
  # Model Fitting Page
  # Reactive values for storing fitted models and fit statistics
  model_fit_stats <- reactiveValues()
  
  # Function to fit models
  fit_models <- function(train_data, test_data) {
    model_fit_stats <- list()
    
    # Model 1: Multiple Linear Regression
    if (input$fit_model_mlr) {
      selected_vars_model1 <- input$selected_vars_model1
      if (length(selected_vars_model1) > 0) {
        lm_formula <- as.formula(paste("survived2 ~", paste(selected_vars_model1, collapse = " + ")))
        model_fit <- lm(lm_formula, data = train_data)
        model_fit_stats$model_mlr <- list(
          model_type = "Multiple Linear Regression",
          model_fit = model_fit,
          summary_stats_train = summary(model_fit)
        )
      }
    }
    # Model 2: Regression Tree
    if (input$fit_model_tree) {
      selected_vars_model2 <- input$selected_vars_model2
      if (length(selected_vars_model2) > 0) {
        formula <- as.formula(paste("survived2 ~", paste(selected_vars_model2, collapse = "+")))
        model_fit <- rpart(formula, data = train_data, method = "class")
        model_fit_stats$model_tree <- list(
          model_type = "Regression Tree",
          model_fit = model_fit,
          summary_stats_train = summary(model_fit)
        )
      }
    }
    # Model 3: Random Forest
    if (input$fit_model_rf) {
      selected_vars_model3 <- input$selected_vars_model3
      if (length(selected_vars_model3) > 0) {
        formula <- as.formula(paste("survived2 ~", paste(selected_vars_model3, collapse = "+")))
        model_fit <- randomForest(formula, data = train_data)
        model_fit_stats$model_rf <- list(
          model_type = "Random Forest",
          model_fit = model_fit,
          variable_importance = importance(model_fit),
          confusion_matrix_train = table(train_data$survived2, predictions_train),
          confusion_matrix_test = table(test_data$survived2, predictions_test)
        )
      }
    }
    
    return(model_fit_stats)
  }
  
  # Fit models on button click
  observeEvent(input$fit_models_button, {
    # Split the data into training and testing sets
    set.seed(123) # For reproducibility
    train_indices <- createDataPartition(titanic_clean$survived2, p = input$train_test_split, list = FALSE)
    train_data <- titanic_clean[train_indices, ]
    test_data <- titanic_clean[-train_indices, ]
    
    # Fit the models based on user selection
    model_fit_stats <- fit_models(train_data, test_data)
  })
  
  # Display fit statistics for each model
  output$model_fit_stats <- renderPrint({
    results <- model_fit_stats
    model_mlr <- results$model_mlr
    model_tree <- results$model_tree
    model_rf <- results$model_rf
    
    output_text <- ""
    
    if (!is.null(model_mlr)) {
      output_text <- paste(output_text, "Model 1 -", model_mlr$model_type, "\n")
      output_text <- paste(output_text, "Summary Statistics on Training Set:\n")
      output_text <- paste(output_text, capture.output(model_mlr$summary_stats_train), collapse = "\n")
      output_text <- paste(output_text, "\n\n")
    }
    if (!is.null(model_tree)) {
      output_text <- paste(output_text, "Model 2 -", model_tree$model_type, "\n")
      output_text <- paste(output_text, "Summary Statistics on Training Set:\n")
      output_text <- paste(output_text, capture.output(model_tree$summary_stats_train), collapse = "\n")
      output_text <- paste(output_text, "\n\n")
    }
    if (!is.null(model_rf)) {
      output_text <- paste(output_text, "Model 3 -", model_rf$model_type, "\n")
      output_text <- paste(output_text, "Variable Importance:\n")
      output_text <- paste(output_text, capture.output(model_rf$variable_importance), collapse = "\n")
      output_text <- paste(output_text, "\n\n")
    }
    
    if (output_text == "") {
      output_text <- "No models selected for fitting."
    }
    
    cat(output_text)
  })
  
  
  
  
  # Prediction Tab
  # Add a reactive function to make predictions based on selected model and input values
  make_prediction <- function(selected_model, input_values) {
    if (selected_model == "Multiple Linear Regression") {
      # Extract input values for MLR prediction
      age <- input_values$age_pred_mlr
      sex <- input_values$sex_pred_mlr
      pclass <- input_values$pclass_pred_mlr
      
      # Create a data frame with the input values
      prediction_data <- data.frame(age = age, sex = sex, pclass = pclass)
      
      # Make prediction using the MLR model
      prediction <- predict(model_fit_stats$model_mlr$model_fit, newdata = prediction_data)
      
    } else if (selected_model == "Regression Tree") {
      # Extract input values for Regression Tree prediction
      age <- input_values$age_pred_tree
      sex <- input_values$sex_pred_tree
      pclass <- input_values$pclass_pred_tree
      
      # Create a data frame with the input values
      prediction_data <- data.frame(age = age, sex = sex, pclass = pclass)
      
      # Make prediction using the Regression Tree model
      prediction <- predict(model_fit_stats$model_tree$model_fit, newdata = prediction_data, type = "class")
      
    } else if (selected_model == "Random Forest") {
      # Extract input values for Random Forest prediction
      age <- input_values$age_pred_rf
      sex <- input_values$sex_pred_rf
      pclass <- input_values$pclass_pred_rf
      
      # Create a data frame with the input values
      prediction_data <- data.frame(age = age, sex = sex, pclass = pclass)
      
      # Make prediction using the Random Forest model
      prediction <- predict(model_fit_stats$model_rf$model_fit, newdata = prediction_data)
      
    } else {
      prediction <- NULL
    }
    
    return(prediction)
  }
  
  # Observe event to trigger prediction on button click
  observeEvent(input$predict_button, {
    selected_model <- input$prediction_model
    
    # Input values for the selected model
    input_values <- reactive({
      input_values <- reactiveValues()
      if (selected_model == "Multiple Linear Regression") {
        input_values$age_pred_mlr <- input$age_pred_mlr
        input_values$sex_pred_mlr <- input$sex_pred_mlr
        input_values$pclass_pred_mlr <- input$pclass_pred_mlr
      } else if (selected_model == "Regression Tree") {
        input_values$age_pred_tree <- input$age_pred_tree
        input_values$sex_pred_tree <- input$sex_pred_tree
        input_values$pclass_pred_tree <- input$pclass_pred_tree
      } else if (selected_model == "Random Forest") {
        input_values$age_pred_rf <- input$age_pred_rf
        input_values$sex_pred_rf <- input$sex_pred_rf
        input_values$pclass_pred_rf <- input$pclass_pred_rf
      }
      return(input_values)
    })
    
    # Make prediction based on selected model and input values
    prediction_result <- make_prediction(selected_model, input_values())
    
    # Output the prediction result
    output$prediction_result <- renderPrint({
      if (!is.null(prediction_result)) {
        paste("Prediction:", prediction_result)
      } else {
        "Please select a model and enter predictor values to make a prediction."
      }
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