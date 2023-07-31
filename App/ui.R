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
                                 plotlyOutput("final_plot"),
                                 # Table to display categorical variable summaries
                                 dataTableOutput("categorical_summaries")
                               )
                             )
                    ), 
                    tabPanel("Numerical Summaries", 
                             sidebarLayout(
                               sidebarPanel(
                                 h2("Select a Summary Type"),
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
                # Add content 
                # Three sub-tabs for Modeling Info, Model Fitting, and Prediction
                tabsetPanel(
                  tabPanel("Modeling Info",
                           # Add content
                  ),
                  
                  tabPanel("Model Fitting",
                           # Add content
                  ),
                  
                  tabPanel("Prediction",
                           # Add content
                  )
                )
        ),
        
        # Data Page
        tabItem(tabName = "data",
                tabsetPanel(
                  tabPanel("Data",
                           fluidPage(
                             titlePanel("Data"),
                             br(),
                             # Display the Titanic dataset in a DataTable
                             DTOutput("titanic_table"),
                             # Button to download the dataset
                             downloadButton("download_data", "Download Dataset")
                           )
                  ),
                  tabPanel("Variable Information", 
                           fluidPage(
                             titlePanel("Variable Information"),
                             ## explain variables
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
                             
                             ## notes about the variables
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
