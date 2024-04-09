box::use(
  shiny[NS, actionButton, radioButtons, textInput, numericInput, observeEvent, tags, moduleServer],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[hidden, disabled, toggle],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    class = "me-md-3 card-shadow",
    id = ns("location_select"),
    full_screen = TRUE,
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
        "Input DEIMS.id",
        value = "102ae489-04e3-481d-97df-45905837dc1a"
      ),
      hidden(
        tags$div(
          id = ns("latlon"),
          layout_column_wrap(
            width = 1 / 3,
            numericInput(
              ns("lat"),
              label = "Latitude",
              value = 0
            ), # value = 51.3919),
            numericInput(
              ns("lon"),
              label = "Longitude",
              value = 0
            ) # value = 11.8787),
          )
        )
      ),
      actionButton(
        inputId = ns("update_map_location"),
        label = "Update location on map"
      ),
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      observeEvent(
        input$input_type,
        ignoreInit = TRUE,
        {
          print("input$input_type::: ", input$input_type)
          toggle(
            id = "latlon",
            condition = input$input_type == "Lat, Long"
          )
          toggle(
            id = "deimsid",
            condition = input$input_type == "DEIMS.id"
          )
        }
      )


      # TODO - start over
      map_defaults <- list(
        lon = 11.8787,
        lat = 51.3919,
        zoom = 9
      )
      #map_opts <- reactiveVal()

      # observeEvent(
      #   # input$update_map_location,
      #   {
      #     if (input$input_type == "Lat, Long") {
      #       map_opts$lon <- input$lon
      #       map_opts$lat <- input$lat
      #     # TODO - deims, viz chat ----
      #     # } else if (input$input_type == "DEIMS.id") {
      #     #   map_opts$lon <- 0
      #     #   map_opts$lon <- 0
      #     } else {
      #       map_opts$lon <- map_defaults$lon
      #       map_opts$lat <- map_defaults$lat
      #     }
      #   }
      # )

      #reactive(map_opts())
    }
  )
}
