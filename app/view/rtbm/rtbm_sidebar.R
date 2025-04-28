# /app/view/rtbm/rtbm_sidebar.R

box::use(
  # Shiny fundamentals and UI
  shiny[
    actionButton,
    dateRangeInput,
    icon,
    isolate,
    invalidateLater,
    moduleServer,
    NS,
    observe,
    observeEvent,
    reactive,
    reactiveVal,
    renderUI,
    req,
    selectInput,
    sliderInput,
    updateSliderInput,
    uiOutput,
    tagList # Needed to wrap UI elements
  ],

  # HTML tools for structured UI building
  htmltools[div, span, hr, tags, p],

  # Modern Shiny components
  shinyWidgets[pickerInput, updatePickerInput],
  shinyjs[toggleClass, addClass, removeClass],

  # Utilities
  lubridate[as_date],

  # Local modules
  app / logic / rtbm / utils[format_date_for_display]
)

#' RTBM Sidebar UI
#'
#' @param id Module ID.
#'
#' @return A Shiny UI definition.
#' @export
rtbm_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "card-body overflow-auto",
      # View selector (always visible)
      selectInput(
        inputId = ns("viewSelector"),
        label = "Select View",
        choices = c(
          "Map" = "map",
          "Summary: Figure 3" = "fig3",
          "Summary: Figure 4" = "fig4",
          "Summary: Figure 5" = "fig5"
        ),
        selected = "map"
      ),
      # Date range input (always visible)
      dateRangeInput(
        inputId = ns("dateRange"),
        label = "Select Date Range",
        start = Sys.Date() - 30,
        end = Sys.Date(),
        min = "2025-01-16",
        max = Sys.Date(),
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 1,
        separator = " to ",
        language = "en"
      ),
      # Always render the species picker, but hide/show with shinyjs
      shinyjs::hidden(
        pickerInput(
          inputId = ns("speciesPicker"),
          label = "Select Bird Species",
          choices = NULL,
          options = list(`live-search` = TRUE)
        )
      ),
      # Conditional UI for map controls (except species picker)
      uiOutput(ns("conditionalMapControls"))
    )
  )
}

#' RTBM Sidebar Server
#'
#' @param id Module ID.
#' @param bird_spp_info Reactive value containing bird species info.
#' @param available_dates Reactive value containing available observation dates.
#'
#' @return A list of reactive values/expressions:
#'   - current_date: The currently selected date.
#'   - set_current_date: Setter for current_date.
#'   - selected_species: The currently selected species.
#'   - date_range: The selected date range.
#'   - animation_running: Boolean indicating if animation is playing.
#'   - animation_speed: The animation speed in milliseconds.
#'   - sidebar_collapsed: Boolean indicating if the sidebar is collapsed.
#'   - selected_view: The selected view.
#'   - load_data_trigger: Counter to trigger data load.
#' @export
rtbm_sidebar_server <- function(id, bird_spp_info, available_dates) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive Values ---
    current_date_rv <- reactiveVal(NULL)
    animation_running_rv <- reactiveVal(FALSE)
    animation_speed_rv <- reactiveVal(1000) # Default speed
    sidebar_collapsed_rv <- reactiveVal(FALSE) # Track collapsed state
    animation_last_step <- reactiveVal(Sys.time()) # Track when the last step occurred
    status_msg <- reactiveVal("Select date range and species.")
    load_data_trigger <- reactiveVal(0) # Counter to trigger data load

    # --- Observers and Logic ---

    # Always update species picker choices when bird_spp_info is available
    observeEvent(bird_spp_info(), {
      req(bird_spp_info())
      updatePickerInput(
        session = session,
        inputId = "speciesPicker",
        choices = bird_spp_info()$common_name,
        selected = bird_spp_info()$common_name[1]
      )
    })

    # Show/hide the species picker based on view selection
    observe({
      if (is.null(input$viewSelector) || input$viewSelector == "map") {
        shinyjs::show("speciesPicker")
      } else {
        shinyjs::hide("speciesPicker")
      }
    })

    # Update available dates based on date range (Handled in rtbm_app.R, triggered by returned date_range)
    # This module only reacts to the `available_dates` passed in.
    observeEvent(available_dates(),
      {
        req(available_dates())
        dates <- available_dates()
        if (length(dates) > 0) {
          status_msg(paste0(length(dates), " observation dates found."))
          update_date_slider(dates)
          # Set current date to the first available date in the new range
          current_date_rv(dates[1])
        } else {
          status_msg("No data found for the selected date range.")
          update_date_slider(NULL) # Clear slider
          current_date_rv(NULL)
        }
      },
      ignoreNULL = FALSE
    ) # Allow initial NULL to clear things

    # Date Slider -> Update Current Date
    observeEvent(input$date_slider_index, {
      req(available_dates())
      dates <- available_dates()
      idx <- as.integer(input$date_slider_index)
      if (!is.na(idx) && idx > 0 && idx <= length(dates)) {
        current_date_rv(dates[idx])
      }
    })

    # Speed Control -> Update Animation Speed
    observeEvent(input$speedControl, {
      req(input$speedControl)
      animation_speed_rv(input$speedControl)
    })

    # --- Animation Logic ---
    # Handle play/pause toggle via animateControl button
    observeEvent(input$animateControl, {
      new_state <- !animation_running_rv()
      animation_running_rv(new_state)
      if (new_state) {
        animation_last_step(Sys.time() - (animation_speed_rv() / 1000))
        # If data is not loaded (e.g., available_dates is NULL or empty), trigger load
        if (is.null(available_dates()) || length(available_dates()) == 0) {
          load_data_trigger(isolate(load_data_trigger()) + 1)
        }
      }
      output$playPauseButton <- renderUI({
        actionButton(
          inputId = ns("animateControl"),
          label = if (animation_running_rv()) "Pause" else "Play",
          icon = if (animation_running_rv()) icon("pause") else icon("play"),
          class = "btn-primary btn-lg",
          width = "100%"
        )
      })
    })

    # Function for a single animation step
    animation_step <- function() {
      req(animation_running_rv(), available_dates())
      dates <- available_dates()
      if (is.null(dates) || length(dates) == 0) {
        return(NULL)
      }

      current_idx <- isolate(input$date_slider_index)
      if (is.null(current_idx) || is.na(current_idx)) current_idx <- 1 # Handle NULL/NA initial state

      next_idx <- if (current_idx >= length(dates)) 1 else current_idx + 1
      updateSliderInput(session, "date_slider_index", value = next_idx)
    }

    # Observer for animation timing (simpler version using invalidateLater delay)
    observe({
      req(animation_running_rv())
      invalidateLater(animation_speed_rv())
      animation_step()
    })

    # --- Dynamic UI Rendering ---

    # Display current date
    output$currentDateDisplay <- renderUI({
      req(current_date_rv())
      span(
        class = "current-date",
        format_date_for_display(current_date_rv())
      )
    })

    # Dynamic UI for date slider
    update_date_slider <- function(dates) {
      if (!is.null(dates) && length(dates) > 0) {
        output$dateSlider <- renderUI({
          sliderInput(
            ns("date_slider_index"),
            label = "Select Observation Date:",
            min = 1,
            max = length(dates),
            value = 1,
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        })
      } else {
        # Render empty UI or a message if no dates
        output$dateSlider <- renderUI({
          p(class = "text-muted", "No observation dates available for selected range.")
        })
      }
    }

    # Dynamic UI for Play/Pause button
    output$playPauseButton <- renderUI({
      # Ensure this renders even when animation isn't running
      actionButton(
        inputId = ns("animateControl"),
        label = if (animation_running_rv()) "Pause" else "Play",
        icon = if (animation_running_rv()) icon("pause") else icon("play"),
        class = "btn-primary btn-lg",
        width = "100%"
      )
    })

    # Status Message Display
    output$statusMsg <- renderUI({
      tags$p(class = "text-muted", status_msg())
    })

    # --- Conditional UI for map controls ---
    output$conditionalMapControls <- renderUI({
      if (is.null(input$viewSelector) || input$viewSelector == "map") {
        tagList(
          uiOutput(ns("currentDateDisplay")),
          hr(),
          uiOutput(ns("playPauseButton")),
          sliderInput(
            inputId = ns("speedControl"),
            label = "Animation Speed (ms)",
            min = 100,
            max = 2000,
            value = 1000,
            step = 100,
            ticks = FALSE
          ),
          uiOutput(ns("statusMsg")),
          hr(),
          uiOutput(ns("dateSlider"))
        )
      } else {
        NULL
      }
    })

    # --- Return Values ---
    # Return reactive expressions/values needed by the parent module
    return(
      list(
        current_date = reactive(current_date_rv()),
        set_current_date = current_date_rv,
        selected_species = reactive(input$speciesPicker),
        date_range = reactive(input$dateRange),
        animation_running = reactive(animation_running_rv()),
        sidebar_collapsed = reactive(sidebar_collapsed_rv()),
        selected_view = reactive(input$viewSelector),
        load_data_trigger = load_data_trigger
      )
    )
  })
}
