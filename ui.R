library(shiny)
library(shinyMatrix)

ui <- fluidPage(
  
  titlePanel("Bayesian linear regression"),
  br(),
  h4(
    "The application is a simple presentation of a bayesian approach to the linear regression.",
    "We assume that errors has normal distributions with known variation and prior distributions of parameters are also normal."
  ),
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        inputId = "sigma", label = "Sigma",
        min = 0, max = 2, value = 0.5, step = 0.1
      ),
      radioButtons(
        inputId = "N_par", label = "Parameters number",
        choices = c("1", "2", "3"), selected = "2"
      ),
      tags$b("Covariance matrix"),
      uiOutput("cov_mat"),
      br(),
      actionButton("reset", "Reset points")
    ),
    
    mainPanel(
      tags$b(textOutput(outputId = "sample_points")),
      br(),
      plotOutput("sample_space", click = "sample_pos"),
      br(),
      tags$b("Marginal distributions of parameters"),
      plotOutput("dist_prior")
    )
  )
  
)

# print(ui)