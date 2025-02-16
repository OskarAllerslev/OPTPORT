# ---- ui.R ----
library(shiny)

shinyUI(fluidPage(
  titlePanel("Mean-Variance Portefølje — Max Sharpe"),

  sidebarLayout(
    sidebarPanel(
      numericInput("lookbackYears",
                   label = "Tilbagekigningsperiode (år)",
                   value = 5,
                   min = 1,
                   max = 20,
                   step = 1),

      checkboxInput("shortAllowed",
                    label = "Tillad short selling (negative vægte)",
                    value = FALSE),

      actionButton("goButton", "Beregn portefølje"),

      br(), br(),
      uiOutput("explanation"),
      br(),
      tableOutput("resultsTable")
    ),

    mainPanel(
      plotOutput("plotAll", height = "600px")
    )
  )
))
