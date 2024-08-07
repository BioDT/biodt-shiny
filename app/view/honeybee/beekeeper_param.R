box::use(
  shiny[moduleServer, NS, tagList, sliderInput, tags, div, numericInput, bootstrapPage, reactiveVal, observeEvent, reactive, updateSliderInput, checkboxInput],
  bslib[card, card_header, card_body],
)

#' @export
honeybee_param_ui <- function(id, theme, i18n) {
  ns <- NS(id)
  # tagList(
  bootstrapPage(
    theme = theme,
    card(
      class = "me-md-3 card-shadow overflow-hidden",
      title = "params_simulation",
      full_screen = FALSE,
      card_header(
        tags$h2(
          class = "card_title",
          "Simulation Parameters")
      ),
      card_body(
        sliderInput(
          ns("N_INITIAL_BEES"),
          label = "Number of adult bees at the beginning of the simulation",
          min = 0,
          max = 30000,
          value = 10000,
          step = 100
        ),
        sliderInput(
          ns("N_INITIAL_MITES_HEALTHY"),
          label = "Number of Mites at the beginning of the simulation",
          value = 100,
          min = 0,
          max = 100,
          step = 1
        ),
        sliderInput(
          ns("N_INITIAL_MITES_INFECTED"),
          label = "Number of infected Mites at the beginning of the simulation",
          value = 50,
          min = 0,
          max = 100,
          step = 1
        ),
        checkboxInput(
          ns("HoneyHarvesting"),
          label = "Honey Harvest",
          value = TRUE
        ),
        checkboxInput(
          ns("VarroaTreatment"),
          label = "Varroa treatment with arcaricide",
          value = FALSE
        ),
        checkboxInput(
          ns("DroneBroodRemoval"),
          label = "Drone Brood Removal",
          value = TRUE
        ),
        div(
          class = "no-minor-ticks",
          sliderInput(
            ns("SimulationYearStart"),
            label = "Choose start year of the simulation:",
            value = 2016L,
            min = 2016L,
            #ticks = FALSE,
            max = (Sys.Date() - 395) |> format("%Y") |> as.integer(), # 395 -> year plus one month
            step = 1
          )
        ),
        sliderInput(
          ns("DaysLimit"),
          label = "For how many days:",
          value = 365,
          min = 365,
          max = as.integer((Sys.Date() - 31) - as.Date("2016-01-01")),
          step = 30
        ),
      )
    )
  )
  # )
}

#' @export
honeybee_param_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    out <- reactiveVal()
    ## Update MAX of sliderInput "N_INITIAL_MITES_INFECTED" ----
    ## when value of sliderInput "N_INITIAL_MITES_HEALTHY" changes
    observeEvent(
      input$N_INITIAL_MITES_HEALTHY,
      ignoreInit = TRUE,
      {
        updateSliderInput(
          inputId = "N_INITIAL_MITES_INFECTED",
          max = input$N_INITIAL_MITES_HEALTHY
        )
      }
    )

    listen_param_change <- reactive({
      list(
        input$N_INITIAL_MITES_HEALTHY,
        input$N_INITIAL_MITES_INFECTED,
        input$N_INITIAL_BEES,
        input$HoneyHarvesting,
        input$VarroaTreatment,
        input$DroneBroodRemoval,
        input$SimulationYearStart,
        input$DaysLimit
      )
    })

    observeEvent(listen_param_change(), {
      parameters <- data.frame(
        Parameter = c(
          "N_INITIAL_BEES",
          "N_INITIAL_MITES_HEALTHY",
          "N_INITIAL_MITES_INFECTED",
          "HoneyHarvesting",
          "VarroaTreatment",
          "DroneBroodRemoval"#,
          #"SimulationYearStart",
          #"DaysLimit"
        ),
        Value = c(
          input$N_INITIAL_BEES,
          input$N_INITIAL_MITES_HEALTHY,
          input$N_INITIAL_MITES_INFECTED,
          input$HoneyHarvesting,
          input$VarroaTreatment,
          input$DroneBroodRemoval#,
          #input$SimulationYearStart,
          #input$DaysLimit
        ),
        Default.Value = NA
      )

      observeEvent(
        input$SimulationYearStart,
        ignoreInit = TRUE,
        {
          updateSliderInput(
            inputId = "DaysLimit",
            max = as.integer((Sys.Date() - 31) - as.Date(paste0(input$SimulationYearStart, "-01-01")))
          )
        }
      )

      simulation <- data.frame(
        sim_days = input$DaysLimit,
        start_day = as.Date(paste0(input$SimulationYearStart, "-01-01"))
      )

      list(
        parameters = parameters,
        simulation = simulation
      ) |>
        out()
    })

    reactive(out())
  })
}
