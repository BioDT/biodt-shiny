box::use(
  shiny[renderPrint, textOutput, NS, actionButton, radioButtons, textInput, numericInput, observeEvent, tags, moduleServer],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[toggle, hidden],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    class = "me-md-3 card-shadow",
    id = ns("location_select"),
    full_screen = FALSE,
    card_header(
      tags$h5("Select Location")
    ),
    card_body(
      radioButtons(
        inputId = ns("input_type"),
        label = "Choose location type:",
        choices = list("DEIMS.id", "Lat, Long"),
        selected = "DEIMS.id"
      ),
      textInput(
        inputId = ns("deimsid"),
        "DEIMS.id",
        value = "102ae489-04e3-481d-97df-45905837dc1a"
      ),
      textOutput(ns("summary")),
      hidden(
        tags$div(
          id = ns("latlon"),
          layout_column_wrap(
            width = 1 / 3,
            numericInput(
              ns("lat"),
              label = "Latitude",
              value = 0
            ),
            numericInput(
              ns("lng"),
              label = "Longitude",
              value = 0
            )
          ),
        )
      ),
      actionButton(
        inputId = ns("update_map_location"),
        label = "Update Map Location"
      ),
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # work in progress code - TODO remove later
      observeEvent(input$input_type, {
        output$summary <- renderPrint("location radio btn changed")
      })

      # At UI makes visible type of location input (deims vs lat/lng) ----
      observeEvent(input$input_type, ignoreInit = TRUE,
        {          
          shinyjs::toggle(
            id = "latlon",
            condition = input$input_type == "Lat, Long"
          )
          shinyjs::toggle(
            id = "deimsid",
            condition = input$input_type == "DEIMS.id"
          )
        }
      )

      # TODO!!! by pressing UI's button update_map_location make somehow working 
      # this function: `update_inputmap(ns(x), map_defaults)` (from logic folder)
      # with values user type here
      observeEvent(
        input$update_map_location,
        {
          if (input$input_type == "Lat, Long") {
            lng <- input$lon
            lat <- input$lat
          } else if (input$input_type == "DEIMS.id") {
            lng <- 0
            lat <- 0
          }
        }
      )

    }
  )
}
