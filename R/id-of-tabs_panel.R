if (interactive()) {
  
  library(shiny)
  library(shinyWidgets)
  
  ui <- fluidPage(
    tags$h2("Add a shiny app background image"),
    setBackgroundImage(
      src = "https://www.fillmurray.com/1920/1080"
    )
  )
  
  server <- function(input, output, session) {
    
  }
  
  shinyApp(ui, server)
  
}
