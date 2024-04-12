box::use(
  shiny[reactiveVal, verbatimTextOutput, moduleServer, observeEvent, tags, sliderInput, NS, tagList, textInput, passwordInput, textAreaInput, textOutput, renderText],
  bslib[card, card_body, card_header]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        tags$h2("TEST")
      ),
      card_body(
        sliderInput(ns("min"), "Limit (minimum)", value = 50, min = 0, max = 100),
        textOutput(ns("text")),
        textInput(ns("name"), "What's your name?"),
        passwordInput(ns("password"), "What's your password?"),
        textAreaInput(ns("story"), "Tell me about yourself", rows = 3),
        verbatimTextOutput(ns("verbatim"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    f <- reactiveVal()

    observeEvent(input$min, {
      output$text <- renderText(paste0("slider::: ", input$min))
    })
    observeEvent(input$name, 
      {f(
        paste0("I call you ", input$name, ".", "I stole your pass, it reads as", input$password, ", no mater what you say: ", input$story)
      )}
    )
    observeEvent(input$password, 
      {f(
        paste0("I call you ", input$name, ".", "I stole your pass, it reads as", input$password, ", no mater what you say: ", input$story)
      )}
    )
    observeEvent(input$story, 
      {f(
        paste0("I call you ", input$name, ".", "I stole your pass, it reads as", input$password, ", no mater what you say: ", input$story)
      )}
    )

    observeEvent(f, 
    {
      output$verbatim <- renderText(f())        
    })

  })
}