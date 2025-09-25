box::use(
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
    updateSelectInput,
    sliderInput,
    updateSliderInput,
    uiOutput,
    tagList, # Needed to wrap UI elements
    div,
    withProgress,
    incProgress
  ],

  # HTML tools for structured UI building
  htmltools[div, span, hr, tags, p, strong],

  # Modern Shiny components
  shinyWidgets[pickerInput, updatePickerInput],
  shinyjs[toggleClass, addClass, removeClass],

  # Utilities
  lubridate[as_date],
)

box::use(
  # Local modules
  app / logic / rtbm / utils[format_date_for_display],
  app / logic / translate_multiple_choices[translate_multiple_choices],
)

#' RTBM Sidebar UI
#'
#' @param id Module ID.
#'
#' @return A Shiny UI definition.
#' @export
rtbm_sidebar_ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    div(
      class = "card-body overflow-auto",
      # View selector (always visible)
      selectInput(
        inputId = ns("viewSelector"),
        label = i18n$t("Select View"),
        choices = list(
          "Map" = "map",
          "Summary" = "summary"
        ),
        selected = "map"
      ),
      # Date range input (always visible)
      dateRangeInput(
        inputId = ns("dateRange"),
        label = i18n$t("Select Date Range"),
        start = Sys.Date() - 30,
        end = Sys.Date() - 1,
        min = "2025-01-16",
        max = Sys.Date() - 1,
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 1,
        separator = " to ",
        language = "en"
      ),
      # Summary progress indicator (shown only for summary view)
      uiOutput(ns("summaryProgressIndicator")),
      # Always render the species picker, but hide/show with shinyjs
      shinyjs::hidden(
        pickerInput(
          inputId = ns("speciesPicker"),
          label = i18n$t("Select Bird Species"),
          choices = NULL,
          options = list(`live-search` = TRUE)
        )
      ),
      # Add Load Data button here
      actionButton(
        inputId = ns("loadDataButton"),
        label = i18n$t("Load Data"),
        icon = icon("sync"),
        class = "btn btn-primary w-100 mt-3 mb-3" # Changed btn-success to btn-primary
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
#' @param summary_progress_info Reactive value containing summary loading progress information.
#'
#' @return A list of reactive values/expressions:
#'   - current_date: The currently selected date.
#'   - set_current_date: Setter for current_date.
#'   - selected_species: The currently selected species.
#'   - date_range: The selected date range.
#'   - animation_running: Boolean indicating if animation is playing.
#'   - animation_speed: The animation speed in milliseconds.
#'   - selected_view: The selected view.
#'   - load_button_clicked: Reactive trigger for the load data button.
#' @export
rtbm_sidebar_server <- function(id, bird_spp_info, available_dates, summary_progress_info = reactive(NULL), i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive Values ---
    current_date_rv <- reactiveVal(NULL)
    animation_running_rv <- reactiveVal(FALSE)
    animation_speed_rv <- reactiveVal(1000) # Default speed
    animation_last_step <- reactiveVal(Sys.time()) # Track when the last step occurred
    status_msg <- reactiveVal("Select date range and species, then click Load Data.")
    obs_found <- reactiveVal(" observation dates found.") # "dummy" reactive value; no other way how to include it in one of the status messages with i18n

    # --- Observers and Logic ---

    # Observe viewSelector due to language changes
    observe({
      updateSelectInput(
        session,
        "viewSelector",
        label = i18n$t("Select View"),
        choices = structure(
          c("map", "summary"),
          names = c(i18n$t("Map"), i18n$t("Summary"))
        )
      )
    })

    # Always update species picker choices when bird_spp_info is available
    observeEvent(bird_spp_info(), {
      req(bird_spp_info())
      choices <- bird_spp_info()$common_name
      # Determine the default selection
      default_selection <- if ("Arctic Loon" %in% choices) {
        "Arctic Loon"
      } else if (length(choices) > 0) {
        choices[1]
      } else {
        NULL
      }
    })

    observe({
      req(bird_spp_info())
      choices <- bird_spp_info()$common_name
      translate_multiple_choices(
        session,
        "picker",
        "speciesPicker",
        label = "Select Bird Species",
        inline = FALSE,
        i18n,
        choices_type = "singlelist",
        selected_choice = NULL,
        choices
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
          status_msg(paste0(length(dates), i18n$t(obs_found())))
          update_date_slider(dates)
          # Set current date to the first available date in the new range
          current_date_rv(dates[1])
        } else {
          i18n$t(status_msg("No data found for the selected date range."))
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
        # Ensure slider is set to the first frame if starting fresh
        if (is.null(input$date_slider_index) || input$date_slider_index == 0) {
          updateSliderInput(session, "date_slider_index", value = 1)
        }
      }
      output$playPauseButton <- renderUI({
        actionButton(
          inputId = ns("animateControl"),
          label = ifelse(animation_running_rv(), i18n$t("Pause"), i18n$t("Play")),
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

    # --- Reactive for Selected Date Display ---
    selected_date_formatted <- reactive({
      req(input$date_slider_index, available_dates())
      dates <- available_dates()
      idx <- input$date_slider_index
      if (idx > 0 && idx <= length(dates)) {
        format_date_for_display(dates[idx])
      } else {
        NULL
      }
    })

    # --- Dynamic UI Rendering ---

    # Dynamic UI for date slider
    update_date_slider <- function(dates) {
      if (!is.null(dates) && length(dates) > 0) {
        output$dateSlider <- renderUI({
          sliderInput(
            ns("date_slider_index"),
            label = NULL, # Removed label "Select Observation Date:"
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
          p(class = "text-muted", i18n$t("No observation dates available for selected range."))
        })
      }
    }

    # Dynamic UI for Play/Pause button
    output$playPauseButton <- renderUI({
      # Ensure this renders even when animation isn't running
      actionButton(
        inputId = ns("animateControl"),
        label = ifelse(animation_running_rv(), i18n$t("Pause"), i18n$t("Play")),
        icon = if (animation_running_rv()) icon("pause") else icon("play"),
        class = "btn-primary btn-lg",
        width = "100%"
      )
    })

    # Status Message Display
    output$statusMsg <- renderUI({
      tags$p(class = "text-muted", i18n$t(status_msg()))
    })

    # Render the selected date text
    output$selectedDateText <- renderUI({
      date_str <- selected_date_formatted()
      if (!is.null(date_str)) {
        # Center the text and add some margin
        tags$p(class = "text-center mt-2 mb-2", strong(date_str))
      } else {
        NULL
      }
    })

    # --- Conditional UI for map controls ---
    output$conditionalMapControls <- renderUI({
      if (is.null(input$viewSelector) || input$viewSelector == "map") {
        # Show controls only if dates are loaded
        if (!is.null(available_dates()) && length(available_dates()) > 0) {
          tagList(
            # Status Message (e.g., "X observation dates found.")
            uiOutput(ns("statusMsg")),
            # Date slider immediately after status
            uiOutput(ns("dateSlider")),
            # Display selected date below slider
            uiOutput(ns("selectedDateText")),
            # Animation Delay Slider
            sliderInput(
              inputId = ns("speedControl"),
              label = i18n$t("Animation Delay (ms)"),
              min = 100,
              max = 2000,
              value = animation_speed_rv(), # Use reactive value for persistence
              step = 100,
              ticks = FALSE,
              width = "100%"
            ),
            # Play/Pause Button
            uiOutput(ns("playPauseButton"))
          )
        } else {
          # Show only the status message if no dates are loaded yet
          tagList(
            uiOutput(ns("statusMsg"))
          )
        }
      } else {
        NULL # Hide controls for non-map views
      }
    })

    # --- Summary Progress Indicator ---
    output$summaryProgressIndicator <- renderUI({
      if (!is.null(input$viewSelector) && input$viewSelector == "summary") {
        progress_info <- summary_progress_info()
        if (!is.null(progress_info)) {
          div(
            class = "mt-2 mb-2",
            div(
              class = "alert alert-info",
              style = "margin-bottom: 5px; padding: 8px 12px;",
              tags$small(
                icon("sync", class = "fa-spin"),
                " ",
                progress_info$message
              )
            ),
            if (!is.null(progress_info$current) && !is.null(progress_info$total)) {
              div(
                class = "progress",
                style = "height: 8px;",
                div(
                  class = "progress-bar progress-bar-striped progress-bar-animated",
                  role = "progressbar",
                  style = paste0("width: ", round((progress_info$current / progress_info$total) * 100), "%;"),
                  `aria-valuenow` = progress_info$current,
                  `aria-valuemin` = "0",
                  `aria-valuemax` = progress_info$total
                )
              )
            }
          )
        } else {
          NULL
        }
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
        selected_view = reactive(input$viewSelector),
        load_button_clicked = reactive(input$loadDataButton)
      )
    )
  })
}
