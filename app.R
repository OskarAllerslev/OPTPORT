# app.R

# Load necessary libraries
library(shiny)
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)

# Source helper scripts
source("R/load_data.R")
source("R/calculate_portfolio.R")
source("R/plot_portfolio.R")

# Define UI
ui <- fluidPage(
  titlePanel("Mean-Variance Portfolio Optimizer with Benchmark"),
  sidebarLayout(
    sidebarPanel(
      numericInput("target_return", "Årligt målafkast (%)", value = 5, min = 0, max = 100, step = 0.1)
    ),
    mainPanel(
      h3("Portfolio vs Benchmark Cumulative Returns"),
      plotOutput("portfolio_plot"),
      h3("Portfolio Weights"),
      tableOutput("weights_table"),
      h3("Portfolio Metrics vs Benchmark"),
      tableOutput("metrics_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load data
  data <- reactive({
    load_data()
  })

  # Calculate portfolio and metrics
  portfolio <- reactive({
    tryCatch({
      # Konverter årligt afkast til dagligt
      daily_target_return <- (input$target_return / 100) / 252
      calculate_portfolio(data(), daily_target_return)
    }, error = function(e) {
      showNotification(e$message, type = "error")
      return(NULL)
    })
  })

  # Plot portfolio vs benchmark cumulative returns
  output$portfolio_plot <- renderPlot({
    if (!is.null(portfolio())) {
      plot_portfolio(portfolio()$cumulative_returns, portfolio()$weights)
    }
  })

  # Display portfolio weights
  output$weights_table <- renderTable({
    if (!is.null(portfolio())) {
      display_weights(portfolio()$weights, colnames(data())[-ncol(data())])
    }
  }, digits = 4)

  # Display metrics table
  output$metrics_table <- renderTable({
    if (!is.null(portfolio())) {
      portfolio()$metrics
    }
  }, digits = 4)
}

# Run the application
shinyApp(ui = ui, server = server)
