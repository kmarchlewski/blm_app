library(shiny)
library(shinyMatrix)

source("functions/utility_functions.R")

click_id <- 0

N_col <- 10
X <- matrix(NA, 1, N_col)
Y <- matrix(NA, 1, N_col)

N_b_plot <- 100
N_x_plot <- 100
x_plot <- matrix(seq(-2, 2, len = N_x_plot), 1, N_x_plot)

server <- function(input, output) {

  ## Reactive functions:
  N_par <- reactive({
    req(input$N_par)
    
    return (as.integer(input$N_par))
  })

  check_click <- reactive({
    if (length(input$sample_pos$x) != 0) {
      click_id <<- click_id + 1
      
      if (click_id > N_col) {
        N_col <<- N_col + 10
        
        X <<- cbind(X, matrix(NA, 1, 10))
        Y <<- cbind(Y, matrix(NA, 1, 10))
      }
      
      X[1, click_id] <<- input$sample_pos$x
      Y[1, click_id] <<- input$sample_pos$y
    }
    
    invisible(input$reset)
    
  })
  
  cov_mat <- reactive({
    req(input$cov_mat)
    
    return (as.matrix(input$cov_mat))
  })
  
  plot_height <- reactive({N_par() * 300})
  
  observeEvent(input$reset, {
    click_id <<- 0
    X <<- matrix(NA, 1, N_col)
    Y <<- matrix(NA, 1, N_col)
    check_click()
  })
  
  ## Custom output:
  output$cov_mat <- renderUI({
    matrixInput(
      inputId = "cov_mat",
      value = diag(0.2, N_par()), class = "numeric"
    )
  })
  
  ## Simple output:
  output$sample_points <- renderText({
    check_click()
    paste0("Sample points: ", click_id)
  })
  
  output$sample_space <- renderPlot({

    check_click()
    plot(
      NA,
      xlim = c(-2, 2), ylim = c(-2, 2),
      xlab = "X", ylab = "Y", xaxt = "n", yaxt = "n"
    )
    axis(1, tcl = 0.25, lwd = 2, las = 1)
    axis(2, tcl = 0.25, lwd = 2, las = 3)
    grid(col = "grey", lty = "dashed")

    lines(X, Y, col = "red", type = "p", pch = 19)
    
    if (click_id >= N_par() & N_par() == nrow(cov_mat())) {

      model <- predict_blm(
        x_plot, X[1, 1:click_id, drop = F], t(Y[1, 1:click_id, drop = F]),
        N_par()-1, input$sigma, cov_mat()
      )
      
      model$st_dev = sqrt(model$var)
      
      lines(x_plot, model$est, col = "blue")
      
      polygon(
        c(x_plot, rev(x_plot)),
        c(model$est + 2*model$st_dev, rev(model$est - 2*model$st_dev)),
        col = rgb(0, 0, 1, 0.2), border = "blue", lty = 2
      )
    }
    
  }, res = 100)
  
  output$dist_prior <- renderPlot({
    
    b_plot <- matrix(0, N_par(), N_b_plot)

    par(mfrow = c(N_par(), 1))
    
    for (i in 1:N_par()) {
      
      b_plot[i, ] <- seq(-2, 2, len = N_b_plot)
      
      if (N_par() != nrow(cov_mat())) {
        b_bef_dist <- rep(0, N_b_plot)
        b_aft_dist <- rep(0, N_b_plot)
      } else {
        b_bef_dist <- b_dist_bef(b_plot, cov_mat())
        b_aft_dist <- rep(0, N_b_plot)
        
        check_click()
        
        if (click_id >= N_par()) {
          
          H_samp <- H(h, X[1, 1:click_id, drop = F], N_par()-1)
          
          b_aft_dist <- b_dist_aft(
            b_plot, cov_mat(), input$sigma,
            t(Y[1, 1:click_id, drop = F]), H_samp
          )
          
        }
      }
      
      plot(
        NA,
        xlab = paste0("b", i), ylab = paste0("d(b", i, "| ...)"),
        xlim = c(b_plot[i, 1], b_plot[i, N_b_plot]),
        ylim = c(0, max(b_bef_dist, b_aft_dist, 1)),
        xaxt = "n", yaxt = "n"
      )
      lines(b_plot[i, ], b_bef_dist, type = "l", col = "black")
      lines(b_plot[i, ], b_aft_dist, type = "l", col = "blue")
      
      axis(1, tcl = 0.25, lwd = 2, las = 1)
      axis(2, tcl = 0.25, lwd = 2, las = 3)
      grid(col = "grey", lty = "dashed")
      
      b_plot[i, ] <- rep(0, N_b_plot)
    }

    par(mfrow = c(1, 1))
    
  }, res = 100, height = plot_height)
  
}

