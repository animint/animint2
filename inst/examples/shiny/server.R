library(shiny)
library(animint2)

renderAnimint <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!requireNamespace("shiny")) message("Please install.packages('shiny')")
  func <- shiny::exprToFunction(expr, env, quoted)
  renderFunc <- function(shinysession, name, ...) {
    val <- func()
    tmp <- tempfile()
    stuff <- animint2dir(val, out.dir = tmp, open.browser = FALSE)
    shiny::addResourcePath("animintAssets", tmp)
    list(jsonFile = "plot.json")
  }
  shiny::markRenderFunction(animint2::animintOutput, renderFunc) 
}


shinyServer(function(input, output) {
  
  getPlot <- reactive({
    ggplot(mapping = aes_string(
             x = input$x, y = input$y,
             size=input$size,
             color = input$col)) + 
      geom_point(data = mtcars)
  })
  
  output$ggplot <- renderPlot({
    print(getPlot())
  }, width = 300, height = 300)
  
  # renderAnimint() expects a list of ggplots and animint options
  output$animint <- renderAnimint({
    # unlike plotOutput, height/width is controlled with theme_animint()
    p <- getPlot() + theme_animint(height = 300, width = 300)
    list(plot = p)
  })
  
})
