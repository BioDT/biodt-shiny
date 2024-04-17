box::use(
  shiny[
    reactiveVal, renderText, verbatimTextOutput, NS, actionButton, radioButtons,
    textInput, numericInput, observeEvent, tags, moduleServer
  ],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[toggle, hidden],
)

box::use(
  app/logic/deimsid_coordinates,
  app/logic/grassland/update_inputmap
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
        # examples: Bily Kriz a61dd7df-5fd7-47b4-8172-b7dfaf969748, Elbe 858b9f78-889f-4acb-8a12-c3c2436d794c...
      ),
      hidden(
        tags$div(
          id = ns("latlon"),
          layout_column_wrap(
            width = 1 / 3,
            numericInput(
              ns("lat"),
              label = "Latitude",
              value = 51.3919
            ),
            numericInput(
              ns("lng"),
              label = "Longitude",
              value = 11.8787
            )
          ),
        )
      ),
      verbatimTextOutput(
        ns("deimsidinfo")
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

      # Makes visible type of location input in UI (deims vs lat/lng) ----
      observeEvent(input$input_type, ignoreInit = TRUE,
        {          
          toggle(
            id = "latlon",
            condition = input$input_type == "Lat, Long"
          )
          toggle(
            id = "deimsid",
            condition = input$input_type == "DEIMS.id"
          )
          toggle(
            id = "deimsidinfo",
            condition = input$input_type == "DEIMS.id"
          )
        }
      )

      # Loads lng/lat when DEIMS.id input is set ----
      # TODO MERGE THIS PART INTO OBSERVE EVENT BELOW
      coordinates <- reactiveVal()
      observeEvent(input$deimsid, ignoreInit = FALSE, {      
        input$deimsid |>
          deimsid_coordinates$get_coords() |>
          coordinates()

        coords_outtext <- coordinates()
        # if (coords_outtext == NA) {
        #   output$deimsidinfo <- renderText(paste0("Coordinates for the given DEIMS.id not found"))
        # } else {
          if (coords_outtext$lng == 11.8787 & coords_outtext$lat == 51.3919) {
            output$deimsidinfo <- renderText(paste0("Waiting for DEIMS.id input..."))
          } else if (coords_outtext$lng != 11.8787 & coords_outtext$lat != 51.3919) {
            output$deimsidinfo <- renderText(paste0("Found coordinates:\nlng = ", coords_outtext$lng, ", lat = ", coords_outtext$lat))
          }
        # }        
      })

      # Calls update inputmap (aka leafletProxy fn) with the given coordinates (lng/lat or by DEIMS.id) ----
      observeEvent(
        input$update_map_location,
        {
          map_options <- list()
          if (input$input_type == "Lat, Long") {
            map_options$lng <- input$lng
            map_options$lat <- input$lat
          } else if (input$input_type == "DEIMS.id") {
            map_options <- coordinates()
          }

          map_options$zoom <- 9
        }
      )

      return(coordinates)
    }
  )
}
