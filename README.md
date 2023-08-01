# ST558-Project3

## Brief Description of the App and its Purpose:  
This app allows the user to view, explore, and analyze the Titanic dataset. 
The user will be able to customize inputs while R code runs in the background to provide the desired output.  

The app consists of four tabs:
1. About: Provides an overview of the app, its structure, and the dataset.  
2. Data Exploration: Allows users to explore the data through numerical and graphical summaries.  
3. Modeling: Enables users to fit supervised learning models and make predictions.  
4. Data: Allows users to view, subset, and save the Titanic dataset.  

## List of Packages needed to run the App:  
* shiny  
* caret  
* rpart  
* randomForest  
* ggplot2  
* plotly  
* DT  

## Code to Install the Packages:
`install.packages(c("shiny", "caret", "rpart", "randomForest", "ggplot2", "plotly", "DT"))`  

## Code to Run the App:
`shiny::runGitHub("saziz12/ST558-Project3", subdir = "App/")`

