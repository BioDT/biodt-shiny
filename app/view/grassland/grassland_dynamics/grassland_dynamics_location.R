box::use(
  shiny[
    # nolint
    reactiveVal,
    renderText,
    verbatimTextOutput,
    NS,
    actionButton,
    radioButtons,
    textInput,
    numericInput,
    observeEvent,
    tags,
    moduleServer,
    observe,
    updateNumericInput,
    req,
    debounce,
    reactive,
    showNotification
  ],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[toggle, hidden],
  htmltools[as.tags, tags, HTML],
  purrr[is_empty],
  config,
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
      layout_column_wrap(
        width = 1 / 2,
        numericInput(
          ns("start_year"),
          label = i18n$translate("Start Year"),
          value = 2000,
          min = 1900,
          max = as.numeric(format(Sys.Date(), "%Y")) - 2,
          step = 1
        ),
        numericInput(
          ns("end_year"),
          label = i18n$translate("End Year"),
          value = as.numeric(format(Sys.Date(), "%Y")) - 1,
          min = 1901,
          max = as.numeric(format(Sys.Date(), "%Y")) - 1,
          step = 1
        )
      ),
      actionButton(
        inputId = ns("update_map_location"),
        label = i18n$translate("Update Map Location"),
        class = "btn-primary"
      ),
      tags$hr(),
      tags$h3(
        class = "mt-3",
        i18n$translate("Run simulation")
      ),
      actionButton(
        inputId = ns("run_simulation"),
        label = i18n$translate("Run simulation"),
        class = "btn-secondary btn-lg",
        style = "width: 100%;"
      ),
    )
  )
}

#' @export
grassland_dynamics_location_server <- function(id, i18n, session_dir) {
  # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare directory for results ----
    # Making a grassland dir in the shared folder
    temp_dir <- session_dir |>
      file.path("grassland")

    # Counter for simulation runs
    counter <- reactiveVal(0)

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

    # Debounced year inputs to allow users time to type ----
    start_year_debounced <- debounce(reactive(input$start_year), 2000)
    end_year_debounced <- debounce(reactive(input$end_year), 2000)

    # Validates year inputs to ensure start_year < end_year ----
    observeEvent(start_year_debounced(), {
      req(input$start_year, input$end_year)
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      max_year <- current_year - 1

      if (input$start_year >= input$end_year) {
        new_value <- input$end_year - 1
        updateNumericInput(
          session,
          "start_year",
          value = new_value,
          max = max_year
        )
        showNotification(
          paste0(
            i18n$translate("Start year adjusted to"),
            " ",
            new_value,
            " ",
            i18n$translate("(must be less than end year)")
          ),
          type = "warning",
          duration = 4
        )
      }
    })

    observeEvent(end_year_debounced(), {
      req(input$start_year, input$end_year)
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      max_year <- current_year - 1

      if (input$end_year <= input$start_year) {
        new_value <- input$start_year + 1
        updateNumericInput(
          session,
          "end_year",
          value = new_value,
          max = max_year
        )
        showNotification(
          paste0(
            i18n$translate("End year adjusted to"),
            " ",
            new_value,
            " ",
            i18n$translate("(must be greater than start year)")
          ),
          type = "warning",
          duration = 4
        )
      } else if (input$end_year > max_year) {
        # Ensure end_year does not exceed previous year
        updateNumericInput(
          session,
          "end_year",
          value = max_year,
          max = max_year
        )
        showNotification(
          paste0(
            i18n$translate("End year adjusted to"),
            " ",
            max_year,
            " ",
            i18n$translate("(maximum allowed year)")
          ),
          type = "warning",
          duration = 4
        )
      }
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
                i18n$translate("Found coordinates:"),
                "\nlng = ",
                coords_outtext$lng,
                ", lat = ",
                coords_outtext$lat
              )
            )
          }
        }
      }
    )

    # Run simulation button ----
    observeEvent(
      input$run_simulation,
      {
        # Check data ----
        req(input$lat, input$lng, input$start_year, input$end_year)

        counter(counter() + 1)

        # Prepare folder structure ----
        if (!dir.exists(temp_dir)) {
          dir.create(temp_dir, recursive = TRUE)
        }

        run_dir <- file.path(
          temp_dir,
          Sys.time() |> format(format = "%Y-%m-%d_%H-%M-%S")
        )
        dir.create(run_dir, recursive = TRUE)

        print(paste("Created simulation directory:", run_dir))
        print(paste("Simulation run number:", counter()))
        print(paste("Latitude:", input$lat, "Longitude:", input$lng))
        print(paste("Year range:", input$start_year, "-", input$end_year))

        # TODO: Add simulation logic here in next steps
      }
    )

    coordinates
  })
}
