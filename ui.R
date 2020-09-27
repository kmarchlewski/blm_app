library(shiny)
library(shinyMatrix)

ui <- fluidPage(
  
  titlePanel("Bayesian linear regression"),
  br(),
  "The application is a simple presentation of a Bayesian approach
  to the linear regression model with Gaussian noise.",
  "We assume that:",
  tags$ul(
    tags$li(
      "errors are independent, identically distributed Gaussian distributions
      with zero mean and known variation (", tags$b("Sigma"), "),"
    ),
    tags$li(
      "prior distribution over parameters is Gaussian with zero mean
      and known", tags$b("Covariance matrix"), ","
    ),
    tags$li(
      "basis functions are polynomials of order 0, 1 or 2 (",
      tags$b("Parameters number"), ")."
    )
  ),
  br(),
  "Start playing with the model from selecting points in the sample space.",
  br(), br(),
  
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
      tags$b("Sample space"),
      br(),
      plotOutput("sample_space", click = "sample_pos"),
      br(),
      tags$b(textOutput(outputId = "sample_points")),
      br(),
      tags$b("Marginal distributions over parameters"),
      plotOutput("dist_prior")
    )
  )
  
)
