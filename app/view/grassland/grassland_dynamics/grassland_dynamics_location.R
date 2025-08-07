box::use(
  shiny[ # nolint
    reactiveVal, renderText, verbatimTextOutput, NS, actionButton, radioButtons,
    textInput, numericInput, observeEvent, tags, moduleServer
  ],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[toggle, hidden],
  htmltools[as.tags, tags, HTML],
)

box::use(
  app / logic / deimsid_coordinates[get_coords_deimsid],
)

#' @export
grassland_dynamics_location_ui <- function(id, i18n) {
  ns <- NS(id)

  choice_names <- c(
    paste0(i18n$t("DEIMS.id")),
    paste0(i18n$t("Latitude, Longitude"))
  )

  choice_values <- c(
    "DEIMS.id",
    "Lat, Long"
  )

  print("length(choice_names):::")
  print(length(choice_names))
  print("length(choice_values):::")
  print(length(choice_values))
  if (length(choice_names) != length(choice_values)) {
    stop("`choiceNames` and `choiceValues` must have the same length.")
  }

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
          "DEIMS.id" = i18n$t("DEIMS.id"),
          "Lat, Long" = i18n$t("Latitude, Longitude")
        )
      ),
      textInput(
        inputId = ns("deimsid"),
        "DEIMS.id",
        value = "102ae489-04e3-481d-97df-45905837dc1a"
      ),
      hidden(
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
        )
      ),
      verbatimTextOutput(
        ns("deimsidinfo")
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
grassland_dynamics_location_server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Makes visible type of location input in UI (deims vs lat/lng) ----
    observeEvent(input$input_type, ignoreInit = TRUE, {
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

        if (input$input_type == "Lat, Long") {
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
                "Found coordinates:\nlng = ", coords_outtext$lng, ", lat = ", coords_outtext$lat
              )
            )
          }
        }
      }
    )

    coordinates
  })
}
