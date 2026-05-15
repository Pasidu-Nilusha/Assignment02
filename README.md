library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

gaming_data <- read.csv("Gaming.csv")

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "🎮 Gaming vs Academic Performance"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trend", tabName = "trend", icon = icon("chart-line")),
      menuItem("Impact", tabName = "impact", icon = icon("balance-scale")),
      menuItem("Growth", tabName = "growth", icon = icon("chart-bar")),
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    tabItems(
      

      tabItem(tabName = "trend",
              fluidRow(
                box(title = "Gaming Hours vs Grades", width = 12,
                    plotlyOutput("trendPlot"))
              )
      ),
      
      
      tabItem(tabName = "impact",
              fluidRow(
                box(title = "Correlation Analysis", width = 12,
                    plotlyOutput("impactPlot"))
              )
      ),
      
     
      tabItem(tabName = "growth",
              fluidRow(
                box(title = "Distribution of Grades", width = 12,
                    plotlyOutput("growthPlot"))
              )
      ),
      
      
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Regression Model", width = 12,
                    plotlyOutput("predictionPlot")),
               
              )
      )
    )
  )
)

server <- function(input, output) {
  
 
  output$trendPlot <- renderPlotly({
    p <- ggplot(gaming_data, aes(x = gaming_hours, y = grades)) +
      geom_point(aes(text = paste("Student ID:", student_id)), 
                 alpha = 0.4, color = "steelblue") +
      theme_minimal() +
      labs(title = "Gaming Hours vs Grades", 
           x = "Hours Gaming per Day", 
           y = "Grades")
    ggplotly(p)
  })
  
  
  output$impactPlot <- renderPlotly({
    p <- ggplot(gaming_data, aes(x = gaming_hours, y = grades)) +
      geom_point(alpha = 0.2, color = "blue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      theme_minimal() +
      labs(title = "Correlation Between Gaming and Grades",
           x = "Gaming Hours", y = "Grades")
    ggplotly(p)
  })
  

  output$growthPlot <- renderPlotly({
    p <- ggplot(gaming_data, aes(x = grades)) +
      geom_histogram(fill = "darkgreen", bins = 30, color = "white") +
      theme_minimal() +
      labs(title = "Distribution of Student Grades", x = "Grades", y = "Frequency")
    ggplotly(p)
  })
  
 
  output$predictionPlot <- renderPlotly({
    model <- lm(grades ~ gaming_hours, data = gaming_data)
    p <- ggplot(gaming_data, aes(x = gaming_hours, y = grades)) +
      geom_point(alpha = 0.2, color = "gray") +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      theme_minimal() +
      labs(title = "Regression Model: Predicting Grades", 
           x = "Hours Gaming per Day", 
           y = "Predicted Grades")
    ggplotly(p)
  })
  
  
  }


shinyApp(ui, server)
