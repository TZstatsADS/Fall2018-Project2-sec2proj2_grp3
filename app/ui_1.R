'library(shiny)
     shinyServer(function(input, output) {
     output$plot=renderPlot({
     hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
     xlab = "Duration (minutes)", main = "Geyser eruption duration")
     
     dens <- density(faithful$eruptions, adjust = input$bw_adjust)
     lines(dens, col = "blue")
     })
     })'
