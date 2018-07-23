library(shiny)
library(animint2)

shinyServer(function(input, output) {
  
  getPlot <- reactive({
    a_plot(mapping = aes_string(
             x = input$x, y = input$y,
             size=input$size,
             color = input$col)) + 
      a_geom_point(data = mtcars)
  })
  
  output$a_plot <- renderPlot({
    print(getPlot())
  }, width = 300, height = 300)
  
  # renderAnimint() expects a list of a_plots and animint options
  output$animint <- renderAnimint({
    # unlike plotOutput, height/width is controlled with a_theme_animint()
    p <- getPlot() + a_theme_animint(height = 300, width = 300)
    list(plot = p)
  })
  
})
