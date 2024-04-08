box::use(shiny[NS, actionButton, h3, radioButtons, textInput, numericInput, observeEvent, tags],
         htmltools[div],
         bslib[card, card_header, card_body, layout_column_wrap, sidebar],
         shinyjs[hidden, disabled,toggle],
         leaflet[leafletProxy, clearMarkers, setView, addMarkers])

#' @export
mod_grassland_location_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "me-md-3 card-shadow",
    id = ns("location_select"),
    full_screen = TRUE,
    card_header(
      tags$h5("Select Location")
    ),
    card_body(
      shiny::radioButtons(
        inputId = ns("input_type"),
        label = "Choose location type:",
        choices = list("DEIMS.id", "Lat, Long"),
        selected = "DEIMS.id"
      ),
      shiny::textInput(
        inputId = ns("deimsid"),
        "Input DEIMS.id",
        value = "102ae489-04e3-481d-97df-45905837dc1a"
      ),
      shinyjs::hidden(
        shiny::tags$div(
        id = ns("latlon"),
          bslib::layout_column_wrap(
            width = 1 / 3,
            shiny::numericInput(
              ns("lat"),
              label = "Latitude",
              value = 51.3919),
            shiny::numericInput(
              ns("lon"),
              label = "Longitude",
              value = 11.8787)
          )
        )
      ),
      shiny::actionButton(
        inputId = ns("update_map_location"),
        label = "Update location on map"
      ),
    )
  )
}

#' @export
mod_grassland_location_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      shiny::observeEvent(
        input$input_type,
        ignoreInit = TRUE,
        {
          print("input$input_type::: ", input$input_type)
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
      
      shiny::observeEvent(
        input$update_map_location,
        {
          if (input$input_type == "Lat, Long") {
            lng <- input$lon
            lat <- input$lat
          } else if (input$input_type == "DEIMS.id") {
            lng <- 0
            lat <- 0
          }
          
          leaflet::leafletProxy("input_map_plot") |>
            leaflet::clearMarkers() |>
            leaflet::setView(lng = lng,
                             lat = lat,
                             zoom = 9) |>
            leaflet::addMarkers(lng = lng,
                                lat = lat)
        }
      )
    }
  )
}
