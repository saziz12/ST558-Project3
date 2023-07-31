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
                  titlePanel("About Titanic Analysis App"),
                  br(),
                  h4("Purpose:"),
                  p("This Shiny app is designed to analyze the Titanic dataset and provide insights and analysis into the passengers' information and survival outcomes on the Titanic."),
                  br(),
                  h4("Data Source:"),
                  p("The dataset used in this app is the famous Titanic dataset, which contains information about the passengers onboard the Titanic ship, including their age, gender, ticket class, fare, and survival status."),
                  p("You can find more information about the Titanic dataset at the Kaggle website:"),
                  p(a("Kaggle Titanic Dataset", href = "https://www.kaggle.com/c/titanic")),
                  br(),
                  h4("App Tabs:"),
                  p("This app consists of four tabs:"),
                  tags$ol(
                    tags$li("About: Provides an overview of the app and dataset."),
                    tags$li("Data Exploration: Allows users to explore the data through numerical and graphical summaries."),
                    tags$li("Modeling: Enables users to fit supervised learning models and make predictions."),
                    tags$li("Data: Allows users to view and save the Titanic dataset.")
                  ),
                  br(),
                  h4("Image Related to Titanic:"),
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
                             # Sub-tab for Multiple Linear Regression / Generalized Linear Regression
                             tabPanel("Multiple Linear Regression / Generalized Linear Regression", tabName = "model_info_lm",
                                      fluidPage(
                                        titlePanel("Multiple Linear Regression / Generalized Linear Regression"),
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
                             
                             # Sub-tab for Regression Tree
                             tabPanel("Regression Tree",
                                      fluidPage(
                                        titlePanel("Regression Tree"),
                                        # ... (Modeling Info for Regression Tree content)
                                        # Example:
                                        h4("Regression Tree:"),
                                        p("A regression tree is a decision tree-based model used for regression tasks."),
                                        p("It recursively partitions the data into subsets and fits a simple model (e.g., mean or median) to each subset."),
                                        # ... (continue with explanations of regression tree)
                                      )
                             ),
                             # Sub-tab for Random Forest
                             tabPanel("Random Forest",
                                      fluidPage(
                                        titlePanel("Random Forest"),
                                        # ... (Modeling Info for Random Forest content)
                                        # Example:
                                        h4("Random Forest:"),
                                        p("Random Forest is an ensemble learning method that combines multiple decision trees."),
                                        p("It improves the accuracy and reduces overfitting compared to a single decision tree."),
                                        # ... (continue with explanations of random forest)
                                      )
                             )
                           )
                  ),
                  
                  # Model Fitting tab
                  tabPanel("Model Fitting", tabName = "model_fitting",
                           fluidPage(
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
                             checkboxInput("fit_model_tree", "Fit Regression Tree"),
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
                             br(),
                             h2("Model Fit Statistics:"),
                             verbatimTextOutput("model_fit_stats")
                           )
                  ),
                  
                  # Prediction tab
                  tabPanel("Prediction", tabName = "prediction",
                           fluidPage(
                             titlePanel("Prediction"),
                             # ... (Prediction content)
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
                             # Button to download the dataset
                             downloadButton("download_data", "Download Dataset")
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
