box::use(
  shiny[ # nolint
    reactiveVal, renderText, verbatimTextOutput, NS, actionButton, radioButtons,
    textInput, numericInput, observeEvent, tags, moduleServer, observe
  ],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[toggle, hidden],
  htmltools[as.tags, tags, HTML],
)

box::use(
  app / logic / deimsid_coordinates[get_coords_deimsid],
  app / logic / translate_multiple_choices[translate_multiple_choices],
)

#' @export
grassland_dynamics_location_ui <- function(id, i18n) {
  ns <- NS(id)

  card(
    class = "mt-2 me-md-3 card-shadow",
    id = ns("location_select"),
    full_screen = FALSE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Location")
      )
    ),
    card_body(
      radioButtons(
        inputId = ns("input_type"),
        label = i18n$translate("Choose location type:"),
        choices = c(
          "Latitude, Longitude" = "Latitude, Longitude",
          "DEIMS.id" = "DEIMS.id"
        ),
        selected = "Latitude, Longitude"
      ),
      hidden(
        textInput(
          inputId = ns("deimsid"),
          "DEIMS.id",
          value = "102ae489-04e3-481d-97df-45905837dc1a"
        )
      ),
      tags$div(
        id = ns("latlon"),
        layout_column_wrap(
          width = 1 / 3,
          numericInput(
            ns("lat"),
            label = i18n$translate("Latitude"),
            value = 51.3919
          ),
          numericInput(
            ns("lng"),
            label = i18n$translate("Longitude"),
            value = 11.8787
          )
        ),
      ),
      hidden(
        verbatimTextOutput(
          ns("deimsidinfo")
        )
      ),
      actionButton(
        inputId = ns("update_map_location"),
        label = i18n$translate("Update Map Location"),
        class = "btn-primary"
      ),
    )
  )
}

#' @export
grassland_dynamics_location_server <- function(id, i18n) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # translates radio buttons - choosing an input type of location ----
    observe({
      translate_multiple_choices(
        session,
        "radio",
        input_id = "input_type",
        label = "Choose location type:",
        inline = FALSE,
        i18n,
        choices_type = "namedlist",
        selected_choice = input$input_type,
        c(
          "Latitude, Longitude" = "Latitude, Longitude",
          "DEIMS.id" = "DEIMS.id"
        )
      )
    })

    # Makes visible type of location input in UI (deims vs lat/lng) ----
    observeEvent(input$input_type, ignoreInit = TRUE, {
      toggle(
        id = "latlon",
        condition = input$input_type == "Latitude, Longitude"
      )
      toggle(
        id = "deimsid",
        condition = input$input_type == "DEIMS.id"
      )
      toggle(
        id = "deimsidinfo",
        condition = input$input_type == "DEIMS.id"
      )
    })

    # Sets up reactive value for map coordinates ----
    # The variable is then pulled up to app.R, from there passed down as fn's argument
    # into the location.R server and from there its change is observed - when the change
    # happens, grassland_update_map function (in logic dir) is called
    coordinates <- reactiveVal()

    observeEvent(
      input$update_map_location,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        map_options <- list()

        if (input$input_type == "Latitude, Longitude") {
          # Loads lng/lat from direct inputs ----

          map_options$lng <- input$lng
          map_options$lat <- input$lat

          map_options |>
            coordinates()
        } else if (input$input_type == "DEIMS.id") {
          # Loads lng/lat when DEIMS.id input is set ----

          input$deimsid |>
            get_coords_deimsid() |>
            coordinates()

          ## Short info that the given DEIMS.id was loaded correctly ----
          coords_outtext <- coordinates()
          if (is.numeric(coords_outtext$lng) & is.numeric(coords_outtext$lat)) {
            output$deimsidinfo <- renderText(
              paste0(
                i18n$translate("Found coordinates:"), "\nlng = ", coords_outtext$lng, ", lat = ", coords_outtext$lat
              )
            )
          }
        }
      }
    )

    coordinates
  })
}
