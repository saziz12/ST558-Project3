# Load required libraries
library(shiny)
library(shinydashboard)
library(DT) # For interactive data table
library(ggplot2)
library(plotly)

# Load Titanic dataset
titanic <- read.csv('../dataset/titanic.csv')

# UI
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Titanic Analysis"),
    dashboardSidebar(
      # Sidebar menu with tabs
      sidebarMenu(
        menuItem("About", tabName = "about"),
        menuItem("Data Exploration", tabName = "data_explore"),
        menuItem("Modeling", tabName = "modeling"),
        menuItem("Data", tabName = "data")
      )
    ),
    dashboardBody(
      tabItems(
        # About Page
        tabItem(tabName = "about",
                fluidPage(
                  h1("About the Titanic Analysis App"),
                  br(),
                  h2("Purpose:"),
                  h4("This Shiny app is designed to view, explore, and analyze the Titanic dataset and provide insights into the passengers' information and survival outcomes on the Titanic."),
                  br(),
                  h2("Data Source:"),
                  h4("The dataset used in this app is the famous Titanic dataset, which contains information about the passengers onboard the Titanic ship, including their age, gender, ticket class, fare, survival status, etc."),
                  h4("You can find more information about the Titanic dataset at the Kaggle website:"),
                  h4(a("Kaggle Titanic Dataset", href = "https://www.kaggle.com/c/titanic")),
                  br(),
                  h2("App Tabs:"),
                  h4("This app consists of four tabs:"),
                  h4("1. About: Provides an overview of the app, its structure, and the dataset."),
                  h4("2. Data Exploration: Allows users to explore the data through numerical and graphical summaries."),
                  h4("3. Modeling: Enables users to fit supervised learning models and make predictions."),
                  h4("4. Data: Allows users to view, subset, and save the Titanic dataset."),
                  br(),
                  br(),
                  div(tags$img(src = "titanic.png", width = "85%", height = "85%"),
                      align = "center")
                )
        ),
        
        # Data Exploration Page
        tabItem(tabName = "data_explore",
                fluidPage(
                  titlePanel("Data Exploration"),
                  br(),
                  tabsetPanel(
                    tabPanel("Graphical Summaries",
                             sidebarLayout(
                               sidebarPanel(
                                 h2("Select a Graph Type"),
                                 radioButtons("GraphType", 
                                              "Select Plot Type:",
                                              c("A Box Plot of a Single Categorical Variable" = "box",
                                                "A Histogram of a Single Numeric Variable" = "hist",
                                                "A Scatter Plot of Two Numerical Variables" = "scatter",
                                                selected = character(0))
                                 ),
                                 conditionalPanel(condition = "input.GraphType == 'box'",
                                                  radioButtons("charvar", 
                                                               "Select Categorical Variable:",
                                                               c("Ticket Class" = "pclass1",
                                                                 "Survival" = "survived1",
                                                                 "Sex" = "sex1",
                                                                 "Port of Embarkation" = "embarked1"
                                                               )
                                                  )),
                                 conditionalPanel(condition = "input.GraphType == 'hist'",
                                                  radioButtons("numvar",
                                                               "Select Numerical Variable:",
                                                               c("Age" = "age",
                                                                 "Number of Siblings/Spouses Aboard" = "sibsp",
                                                                 "Number of Parents/Children Aboard" = "parch",
                                                                 "Passenger Fare" = "fare"
                                                               )
                                                  )),
                                 conditionalPanel(condition = "input.GraphType == 'scatter'",
                                                  radioButtons("numvar1",
                                                               "Select First Numerical Variable:",
                                                               c("Age" = "age",
                                                                 "Number of Siblings/Spouses Aboard" = "sibsp",
                                                                 "Number of Parents/Children Aboard" = "parch",
                                                                 "Passenger Fare" = "fare"
                                                               )
                                                  ),
                                                  radioButtons("numvar2",
                                                               "Select Second Numerical Variable:",
                                                               c("Age" = "age",
                                                                 "Number of Siblings/Spouses Aboard" = "sibsp",
                                                                 "Number of Parents/Children Aboard" = "parch",
                                                                 "Passenger Fare" = "fare"
                                                               )
                                                  )),
                                 br(),
                                 actionButton("runGraph", strong("Create Plot"))
                                 ),
                               mainPanel(
                                 plotlyOutput("final_plot")
                               )
                             )
                    ), 
                    tabPanel("Numerical Summaries", 
                             sidebarLayout(
                               sidebarPanel(
                                 h2("Select a Summary Variable"),
                                 radioButtons("sumvar", 
                                              "Select a Variable to Receive Summary Statistics:",
                                              c("Age" = "age",
                                                "Passenger Fare" = "fare",
                                                "Survival" = "survived"
                                              )
                                 ),
                                 br(),
                                 actionButton("runSum", strong("Create Summary Statistic"))
                               ),
                               mainPanel(
                                 verbatimTextOutput("summarystat")
                               )
                             )
                    )
                  )
                )
        ),
        
        # Modeling Page
        tabItem(tabName = "modeling",
                # Three sub-tabs for Modeling Info, Model Fitting, and Prediction
                tabsetPanel(
                  # Modeling Info tab with model-specific sub-tabs
                  tabPanel("Modeling Info",
                           tabsetPanel(
                             # Sub-tab for Multiple Linear Regression
                             tabPanel("Multiple Linear Regression", tabName = "model_info_lm",
                                      fluidPage(
                                        titlePanel("Multiple Linear Regression"),
                                        h4("Multiple linear regression is a statistical method used to model the relationship between a dependent variable and two or more independent variables. It assumes that the relationship between the dependent variable and the independent variables is linear. The model can be represented as:"),
                                        h4(withMathJax(helpText("$$Y_i=\\beta_0+\\beta_1x_{1i}+\\beta_2x_{2i}+\\beta_3x_{1i}x_{2i}  +...+ E_i$$"))),
                                        br(),
                                        h4(strong("Benefits:")),
                                        h4("1. MLR is simple and easy to interpret."),
                                        h4("2. MLR can handle multiple predictors and can be extended to handle interactions between predictors."),
                                        h4("3. MLR provides estimates of the coefficients, whcih helps the user understand the impact of each predictor on the dependent variable."),
                                        br(),
                                        h4(strong("Drawbacks:")),
                                        h4("1. MLR assumes a linear relationship between the dependent and independent variables, which may not always be the case"),
                                        h4("2. MLR is sensitive to outliers, which can significantly impact the model's performance"),
                                        h4("3. MLR assumes the errors are independent and identically distributed, with may not always be the case.")
                                      )
                             ),
                             
                             # Sub-tab for Classification Tree
                             tabPanel("Classification Tree",
                                      fluidPage(
                                        titlePanel("Classification Tree"),
                                        h4("A decision tree is a flowchart-like tree structure where each internal node represents a test on an attribute, each branch represents the outcome of the test, and each leaf node represents a decision or a prediction. The tree is built by recursively splitting the data into subsets based on the best predictor that maximizes information gain or minimizes impurity for classification tasks."),
                                        br(),
                                        h4(strong("Benefits:")),
                                        h4("1. Decision Trees can handle non-linear relationships between the dependent and independent variables without any explicit assumptions."),
                                        h4("2. The decision tree structure is easy to understand and interpret, making it suitable for explaining the model's decision-making process."),
                                        h4("3. The decision tree model provides information about the importance of each predictor in making decisions."),
                                        br(),
                                        h4(strong("Drawbacks:")),
                                        h4("1. Decision trees can easily overfit the data, especially if the tree is deep and captures noise in the training data."),
                                        h4("2. Small changes in the data can lead to a different tree structure, making the model unstable."),
                                        h4("3. Decision trees may not capture complex relationships as well as other models.")
                                      )
                             ),
                             # Sub-tab for Random Forest
                             tabPanel("Random Forest",
                                      fluidPage(
                                        titlePanel("Random Forest"),
                                        h4("Random Forest is an ensemble learning method that constructs multiple decision trees during training and outputs the class that is the mode of the classes (classification) or the mean prediction (regression) of the individual trees. Each tree is built on a different subset of the data (bagging) and a random subset of the features, and the final prediction is obtained through voting or averaging."),
                                        br(),
                                        h4(strong("Benefits:")),
                                        h4("1. Random Forest reduces overfitting by averaging the predictions of multiple trees, which helps to improve generalization performance."),
                                        h4("2. The model is less sensitive to outliers and noisy data due to the averaging effect of multiple trees."),
                                        h4("3. Random Forest provides a measure of feature importance, helping to identify the most influential predictors."), 
                                        br(),
                                        h4(strong("Drawbacks:")),
                                        h4("1. The Random Forest model may be computationally expensive and challenging to interpret due to the ensemble of multiple trees."),
                                        h4("2. Random Forest has hyperparameters that need to be optimized to achieve better performance, making the tuning process more involved.")
                                        )
                             )
                           )
                  ),
                  
                  # Model Fitting tab
                  tabPanel("Model Fitting", tabName = "model_fitting",
                           sidebarLayout(
                             sidebarPanel(
                             h1("Model Fitting with Survival as the Dependent Variable"),
                             h2("Train/Test Split"),
                             sliderInput("train_test_split", "Proportion of Data for Training:", 
                                         min = 0.5, max = 0.9, value = 0.7, step = 0.05),
                             br(),
                             br(),
                             br(),
                             h3("Select Variables for Model 1:"),
                             checkboxGroupInput("selected_vars_model1", "Select Variables:",
                                                c("Age" = "age",
                                                  "Passenger Fare" = "fare",
                                                  "Ticket Class" = "pclass1",
                                                  "Sex" = "sex1",
                                                  "Port of Embarkation" = "embarked1",
                                                  "Number of Siblings/Spouses Aboard" = "sibsp",
                                                  "Number of Parents/Children Aboard" = "parch"
                                                )),
                             checkboxInput("fit_model_mlr", "Fit Multiple Linear Regression"),
                             br(),
                             br(),
                             br(),
                             h3("Select Variables for Model 2:"),
                             checkboxGroupInput("selected_vars_model2", "Select Variables:",
                                                c("Age" = "age",
                                                  "Passenger Fare" = "fare",
                                                  "Ticket Class" = "pclass1",
                                                  "Sex" = "sex1",
                                                  "Port of Embarkation" = "embarked1",
                                                  "Number of Siblings/Spouses Aboard" = "sibsp",
                                                  "Number of Parents/Children Aboard" = "parch"
                                                )),
                             checkboxInput("fit_model_tree", "Fit Classification Tree"),
                             br(),
                             br(),
                             br(),
                             h3("Select Variables for Model 3:"),
                             checkboxGroupInput("selected_vars_model3", "Select Variables:",
                                                c("Age" = "age",
                                                  "Passenger Fare" = "fare",
                                                  "Ticket Class" = "pclass1",
                                                  "Sex" = "sex1",
                                                  "Port of Embarkation" = "embarked1",
                                                  "Number of Siblings/Spouses Aboard" = "sibsp",
                                                  "Number of Parents/Children Aboard" = "parch"
                                                )),
                             checkboxInput("fit_model_rf", "Fit Random Forest"),
                             br(),
                             br(),
                             actionButton("fit_models_button", "Fit Models"),
                             br(),
                             br(),
                             br()
                             ),
                             mainPanel(
                             h2("Model Fit Statistics:"),
                             verbatimTextOutput("model_fit_stats"),
                             verbatimTextOutput("output_text")
                             )
                           )
                  ),
                  
                  # Prediction tab
                  tabPanel("Prediction", tabName = "prediction",
                           fluidPage(
                             titlePanel("Prediction"),
                             selectInput("prediction_model", "Select Model:",
                                         choices = c("Multiple Linear Regression", "Classification Tree", "Random Forest")),
                             conditionalPanel(condition = "input.prediction_model == 'Multiple Linear Regression'",
                                              numericInput("age_pred_mlr", "Age:", value = 25, min = 0, max = 100),
                                              radioButtons("sex_pred_mlr", "Sex:",
                                                           choices = c("Male" = "male", "Female" = "female"), selected = "Male"),
                                              radioButtons("pclass_pred_mlr", "Ticket Class:",
                                                           choices = c("1st" = 1 , "2nd" = 2, "3rd" = 3), selected = "3rd")
                             ),
                             conditionalPanel(condition = "input.prediction_model == 'Classification Tree'",
                                              numericInput("age_pred_tree", "Age:", value = 25, min = 0, max = 100),
                                              radioButtons("sex_pred_tree", "Sex:",
                                                           choices = c("Male" = "male", "Female" = "female"), selected = "Male"),
                                              radioButtons("pclass_pred_tree", "Ticket Class:",
                                                           choices = c("1st" = 1 , "2nd" = 2, "3rd" = 3), selected = "3rd")
                             ),
                             conditionalPanel(condition = "input.prediction_model == 'Random Forest'",
                                              numericInput("age_pred_rf", "Age:", value = 25, min = 0, max = 100),
                                              radioButtons("sex_pred_rf", "Sex:",
                                                           choices = c("Male" = "male", "Female" = "female"), selected = "Male"),
                                              radioButtons("pclass_pred_rf", "Ticket Class:",
                                                           choices = c("1st" = 1 , "2nd" = 2, "3rd" = 3), selected = "3rd")
                             ),
                             br(),
                             actionButton("predict_button", "Predict"),
                             br(),
                             br(),
                             h2("Prediction Result:"),
                             verbatimTextOutput("prediction_result")
                           )
                  )
                  
                )
        ),
        
        
        
        # Data Page
        tabItem(tabName = "data",
                tabsetPanel(
                  tabPanel("Data",
                           fluidPage(
                             h1("Data"),
                             br(),
                             # Display the Titanic dataset in a DataTable
                             DTOutput("titanic_table"),
                             br(),
                             br(),
                             br(),
                             # Button to download the dataset
                             downloadButton("download_data", strong("Download Dataset"))
                           )
                  ),
                  tabPanel("Variable Information",
                           fluidPage(
                             h1("Variable Information"),
                             br(),
                             h2("Some Information about the Variables:"),
                             h4("survival: Survival (0 = No, 1 = Yes)"),
                             h4("pclass: Ticket Class (1 = 1st, 2 = 2nd, 3 = 3rd)"),
                             h4("sex: Sex"),
                             h4("Age: Age in years"),
                             h4("sibsp: # of Siblings / Spouses aboard the Titanic"),
                             h4("parch: # of Parents / Children aboard the Titanic"),
                             h4("Ticket: Ticket Number"),
                             h4("fare: Passenger Fare"),
                             h4("cabin: Cabin Number"),
                             h4("embarked: Port of Embarkation (C = Cherbourg, Q = Queenstown, S = Southampton)"),
                             br(),
                             h2("Variable Notes"),
                             h4("pclass: A proxy for socio-economic status (SES)"),
                             h5("1st = Upper"),
                             h5("2nd = Middle"),
                             h5("3rd = Lower"),
                             br(),
                             h4("age: Age is fractional if less than 1. If the age is estimated, it is in the form of xx.5"),
                             br(),
                             h4("sibsp: The dataset defines family relations in the following way:"),
                             h5("Sibling = brother, sister, stepbrother, stepsister"),
                             h5("Spouse = husband, wife (mistresses and fiancés were ignored)"),
                             br(),
                             h4("parch: The dataset defines family relations in the following way:"),
                             h5("Parent = mother, father"),
                             h5("Child = daughter, son, stepdaughter, stepson"),
                             h6("Some children travelled only with a nanny, therefore parch=0 for them.")
                           )
                  )
                )
        )
      )
    )
  )
)
