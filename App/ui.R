# Load required libraries
library(shiny)
library(shinydashboard)
library(DT) # For interactive data table
library(ggplot2)

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
                  img(src = "../images/titanic.jpeg", width = "50%")
                )
        ),
        
        # Data Exploration Page
        tabItem(tabName = "data_explore",
                # Add content
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
                fluidPage(
                  titlePanel("Data"),
                  br(),
                  # Display the Titanic dataset in a DataTable
                  DTOutput("titanic_table"),
                  # Button to download the dataset
                  downloadButton("download_data", "Download Dataset")
                )
        )
      )
    )
  )
)
