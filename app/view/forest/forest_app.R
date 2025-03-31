box::use(
  shiny,
  terra,
  leaflet,
  gridExtra,
  stringr,
  config,
  bslib,
  utils,
)

box::use(
  app /
    logic /
    forest /
    analysis_functions[
      # get_file_list,
      get_file_name,
      get_data,
      get_figure,
      convert_landis_output
    ]
)

#' @export
forest_app_ui <- function(id, i18n) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      shiny$tagList(
        shiny$selectInput(
          ns("management"),
          "Select Forest Management Regime:",
          choices = c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
        ),
        shiny$selectInput(
          ns("climate"),
          "Select Climate Scenario:",
          choices = c("Current climate", "RCP4.5", "RCP8.5")
        ),
        shiny$selectInput(
          ns("output"),
          "Select Output Type:",
          choices = c(
            "Above-ground biomass",
            "Below-ground biomass",
            "Harvested biomass",
            "Woody Debris",
            "Max-age of selected species",
            "Average age of all trees",
            "Median age of all trees"
          )
        ),
        shiny$conditionalPanel(
          condition = "input.output == 'Above-ground biomass'",
          shiny$selectInput(
            ns("species"),
            "Select Species Type:",
            choices = c(
              "Birch (betulaSP)",
              "Pine (pinussyl)",
              "Spruce (piceabbies)",
              "Other trees (other)"
            )
          ),
        ),
        shiny$conditionalPanel(
          condition = "input.output == 'Max-age of selected species'",
          shiny$selectInput(
            ns("species"),
            "Select Species Type:",
            choices = c(
              "Birch (betulaSP)",
              "Pine (pinussyl)",
              "Spruce (piceabbies)",
              "Other trees (other)",
              "All species"
            )
          )
        ),
        shiny$sliderInput(
          ns("res_file_slider"),
          "Select tick number:",
          min = 0, # Set minimum value
          max = 0, # Set maximum value
          value = 0 # Default value
        )
      )
    ),
    bslib$card(
      shiny$textOutput("selection"),
      leaflet$leafletOutput(ns("map")),
      shiny$plotOutput(ns("plot"), height = "800px")
    )
  )
}

#' @export
forest_app_server <- function(id, app_selected) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_folder <- file.path(config$get("data_path"), "forest_bird")

    output$selection <- shiny$renderText({
      text <- paste(
        "Management Regime:",
        input$management,
        "\n",
        "Climate Scenario:",
        input$climate,
        "\n",
        "Output Type:",
        input$output
      )

      if (input$output %in% c("Above-ground biomass", "Max-age of selected species")) {
        text <- paste(text, "\nSpecies Type:", input$species)
      }

      text
    })

    tick <- shiny$reactiveVal(0)
    res_working_folder <- shiny$reactiveVal(NULL)
    res_file <- shiny$reactiveVal(NULL)
    experiment_data_file <- shiny$reactiveVal(NULL)

    # observeEvent(input$res_file_slider, {
    #   tick(input$res_file_slider)
    #   res_file(stringr$str_replace(string = res_file(), pattern = "\\d+(?=\\D*$)", replacement = as.character(tick())))
    # })
    output$map <- NULL

    shiny$observeEvent(
      app_selected(),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())
        output$map <- leaflet$renderLeaflet({
          leaflet$leaflet() |>
            leaflet$clearImages() |>
            leaflet$addTiles() |>
            leaflet$setView(
              lng = 24.545,
              lat = 60.192,
              zoom = 8
            )
        })
      }
    )

    shiny$observeEvent(
      c(app_selected(), input$management, input$climate, input$output, input$species, input$res_file_slider),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())
        print("load data")
        tick(input$res_file_slider)
        res_file(stringr$str_replace(
          string = res_file(),
          pattern = "\\d+(?=\\D*$)",
          replacement = as.character(tick())
        ))

        input_selection <- get_file_name(
          input,
          tick(),
          data_folder
        )
        print("input_selection")
        print(input_selection)

        # TODO test files exists
        res_file(input_selection$res_file)
        experiment_data_file(input_selection$experiment_data_file)
        res_working_folder(input_selection$res_working_folder)
        res_file_list_tick <- input_selection$res_file_list_tick

        if (length(res_file_list_tick) > 0) {
          min_value <- min(res_file_list_tick, na.rm = TRUE)
          max_value <- max(res_file_list_tick, na.rm = TRUE)

          # Get the current value of the slider
          value <- shiny$isolate(input$res_file_slider)

          # Ensure the value stays within the new range
          if (value < min_value) {
            value <- min_value
          } else if (value > max_value) {
            value <- max_value
          }

          shiny$updateSliderInput(
            session,
            "res_file_slider",
            min = min_value,
            max = max_value,
            value = value,
            step = sort(res_file_list_tick)[2] - sort(res_file_list_tick)[1]
          )
        }
      }
    )

    # shiny$observeEvent(input$res_file_slider, {
    #   tick(input$res_file_slider)
    #   res_file(stringr::str_replace(string = res_file(), pattern = "\\d+(?=\\D*$)", replacement = as.character(tick())))
    # })

    # Map update ----
    shiny$observeEvent(
      c(
        res_file(),
        experiment_data_file()
      ),
      ignoreInit = TRUE,
      {
        shiny$req(res_file())
        shiny$req(experiment_data_file())
        raster_path <- terra$rast(
          utils$unzip(
            experiment_data_file(),
            files = res_file(),
            exdir = tempdir()
          )
        )
        raster_data <- convert_landis_output(raster_path)
        raster_data <- terra$aggregate(raster_data, fact = 2, fun = mean)
        ext <- terra$ext(raster_data)
        pal <- leaflet$colorNumeric(
          palette = "YlOrBr",
          domain = c(
            min(terra$values(raster_data), na.rm = TRUE) -
              mean(terra$values(raster_data), na.rm = TRUE) * 2,
            max(terra$values(raster_data), na.rm = TRUE) * 1.5
          ),
          na.color = "transparent",
          reverse = TRUE
        )
        raster_data <- terra$aggregate(raster_data, fact = 2, fun = mean)

        leaflet$leafletProxy("map") |>
          leaflet$clearImages() |>
          leaflet$addRasterImage(
            raster_data,
            opacity = 0.4,
            colors = pal,
            project = FALSE
          ) |>
          leaflet$addLegend(
            position = "bottomright",
            pal = leaflet$colorNumeric(
              palette = "YlOrBr",
              domain = c(
                min(terra$values(raster_data), na.rm = TRUE) -
                  mean(terra$values(raster_data), na.rm = TRUE) * 2,
                max(terra$values(raster_data), na.rm = TRUE) * 1.5
              ) *
                -1,
              na.color = "transparent"
            ),
            values = terra$values(raster_data) * -1,
            opacity = 0.4,
            labFormat = leaflet$labelFormat(transform = function(x) x * -1)
          )
      }
    )

    output$plot <- shiny$renderPlot({
      # tryCatch(
      #   {
      shiny$req(res_file()) # Ensure the file exists before proceeding
      print("plot data")
      climate_scenarios <- c("current", "4.5", "8.5")
      management_scenarios <- c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
      years <- seq(0, 100, by = 10)
      combined_data <- get_data(climate_scenarios, management_scenarios, years, data_folder)

      plots <- list()

      plots[[1]] <- get_figure(
        combined_data,
        column = "AverageB.g.m2.",
        unit = expression(paste("AGBiomass (g/m"^2, ")")),
        title = "Average above-ground biomass over time (until 2100)"
      )

      plots[[2]] <- get_figure(
        combined_data,
        column = "AverageBelowGround.g.m2.",
        unit = expression(paste("BGBiomass (g/m"^2, ")")),
        title = "Average below-ground biomass over time (until 2100)"
      )

      plots[[3]] <- get_figure(
        combined_data,
        column = "AverageAge",
        unit = expression(paste("Age (years)")),
        title = "Average age over time (until 2100)"
      )

      plots[[4]] <- get_figure(
        combined_data,
        column = "WoodyDebris.kgDW.m2.",
        unit = expression(paste("Woody Debris (kgDW/m"^2, ")")),
        title = "Woody debris over time (until 2100)"
      )

      do.call(gridExtra$grid.arrange, c(plots, nrow = 4))
      #   },
      #   error = function(e) {
      #     shiny$showNotification("Error generating analysis plot", type = "error")
      #     NULL
      #   }
      # )
    })
  })
}
