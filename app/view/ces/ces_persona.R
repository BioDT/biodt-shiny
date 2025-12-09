box::use(
  shiny[
    moduleServer,
    NS,
    tagList,
    column,
    fluidRow,
    verbatimTextOutput,
    actionButton,
    observe,
    observeEvent,
    radioButtons,
    checkboxInput,
    p,
    textOutput,
    renderText,
    reactive,
    HTML,
    selectInput,
    updateSelectInput,
    req,
    renderUI,
    uiOutput,
    htmlOutput,
    selectizeInput,
    tags,
    reactiveVal,
    showNotification,
    removeNotification,
    updateSliderInput,
    modalDialog,
    showModal,
    removeModal,
    textInput,
    downloadButton,
    modalButton,
    downloadHandler,
    fileInput,
    updateRadioButtons
  ],
  bslib[card, nav_select, card_title, card_body],
  leaflet[
    addRasterImage,
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    colorBin,
    layersControlOptions,
    removeLayersControl,
    addControl,
    addLayersControl,
    clearControls,
    hideGroup,
    showGroup,
    clearGroup,
    setMaxBounds,
    labelFormat,
    tileOptions,
    fitBounds
  ],
  terra[
    rast,
    values,
    crop,
    app,
    ifel,
    ext,
    as.polygons,
    sprc,
    merge,
    mean
  ],
  waiter[Waiter],
  dplyr[mutate, select, arrange, left_join, desc, filter, pull],
  purrr[map_chr],
  cli[hash_md5],
  utils[read.csv],
  stats[setNames],
  shinyjs[useShinyjs, runjs, disabled],
  shinyWidgets[
    virtualSelectInput,
    sliderTextInput,
    awesomeCheckbox,
    sendSweetAlert
  ],
  shiny.i18n[update_lang],
  config,
  bslib[navset_tab, nav_panel],
  biodt.recreation[load_config, compute_potential, get_preset_persona_file, read_persona_csv],
  readr[read_csv, cols, col_character, col_integer],
)

box::use(
  app / logic / waiter[waiter_text],
  app / logic / translate_multiple_choices[translate_multiple_choices],
  app / logic / ces / ces_persona_map[setup_map, update_map],
  app / logic / ces / ces_persona_sliders[make_sliders],
  app /
    logic /
    ces /
    ces_persona_utils[
      get_persona_from_sliders,
      capture_messages,
      update_user_info,
      clear_user_info,
      is_error,
      errors_as_messages,
      clean_error_message
    ],
  app /
    logic /
    ces /
    ces_persona_helpers[
      update_selector_personas
    ],
  app / logic / ces / ces_persona_bbox[check_valid_bbox],
  app / logic / ces / ces_persona[check_valid_persona, save_persona],
)

# UI function
ces_persona_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css",
        target = "_blank"
      ),
    ),
    fluidRow(
      style = "height: calc(100vh - 150px); min-height: 800px; margin : 0; padding: 0;",
      column(
        12, # Enlarge the map to full width
        style = "height: 100%;",
        card(
          id = "biodiversity-page",
          title = "combined_map",
          full_screen = FALSE,
          height = "100%",
          card_title(i18n$t("Recreation and Biodiversity Mapping")),
          card_body(
            style = "height: calc(100% - 60px);",
            verbatimTextOutput(ns("userInfo")),
            leafletOutput(ns("combined_map_plot"), height = "100%", width = "100%"),
            tags$div(
              class = "button-container",
              id = ns("button_container"),
              actionButton(
                ns("toggleInfo"),
                HTML('<i class="fa-solid fa-circle-info"></i>'),
                class = "toggle-button",
                title = i18n$t("Info")
              ),
              actionButton(
                ns("toggleSliders"),
                HTML('<i class="fa-solid fa-person-hiking"></i>'),
                class = "toggle-button",
                title = i18n$t("Hiker Settings")
              ),
              actionButton(
                ns("toggleMaps"),
                HTML('<i class="fa-solid fa-layer-group"></i>'),
                class = "toggle-button",
                title = i18n$t("Map Layers")
              )
              # Grayscale button is added dynamically by the map's onRender function
            ),
            # Single Sidebar
            tags$div(
              class = "sidebar sidebar_persona",
              id = ns("sidebar_persona"),
              actionButton(
                ns("closeButton"),
                class = "close-button",
                HTML('<i class="fa-solid fa-x"></i>'),
                title = i18n$t("Close Sidebar")
              ),
              # sliders content
              tags$div(
                id = ns("slidersSidebar"),
                class = "d-none",
                tags$h4(i18n$t("Recreation Potential")),
                tags$h4(i18n$t("Persona Management")),
                selectInput(
                  inputId = ns("persona_selector"),
                  label = i18n$t("Select recreationist type:"),
                  selected = "",
                  choices = list("")
                ),
                actionButton(ns("load_persona"), "Load Persona"),
                # actionButton(
                #   inputId = ns("apply_filter_recre"),
                #   label = i18n$t("Apply filter"),
                #   class = "btn-primary"
                # ),
                p(),
                fileInput(ns("upload_persona"), "Upload Persona"),
                actionButton(ns("save_persona"), "Save Persona"),
                p(),
                navset_tab(
                  nav_panel("Landscape", make_sliders("SLSRA", ns)),
                  nav_panel("Natural Features", make_sliders("FIPS_N", ns)),
                  nav_panel("Infrastructure", make_sliders("FIPS_I", ns)),
                  nav_panel("Water", make_sliders("Water", ns))
                )
              ),
              # map content
              tags$div(
                id = ns("mapsSidebar"),
                class = "d-none",
                tags$h4(i18n$t("Base Map Layer"), class = "mt-3"),
                radioButtons(
                  inputId = ns("map_base_layers"),
                  label = i18n$t("Choose base map:"),
                  choices = c(
                    "Street" = "Esri.WorldStreetMap",
                    "Topographical" = "Esri.WorldTopoMap",
                    "Satellite" = "Esri.WorldImagery",
                    "Greyscale" = "Esri.WorldGrayCanvas"
                  ),
                  selected = "Esri.WorldStreetMap"
                ),
                tags$h4(i18n$t("Overlay Layers"), class = "mt-3"),
                radioButtons(
                  inputId = ns("overlay_layers"),
                  label = i18n$t("Choose overlay layer:"),
                  choices = c(
                    "None" = "none",
                    "Landscape & Land Cover" = "Landscape & Land Cover",
                    "Natural Features" = "Natural Features",
                    "Infrastructure" = "Infrastructure",
                    "Water" = "Water",
                    "Recreational Potential" = "Recreational Potential"
                  ),
                  selected = "none"
                ),
                tags$h5(i18n$t("Layer Opacity"), class = "mt-3"),
                sliderTextInput(
                  inputId = ns("overlay_opacity"),
                  label = i18n$t("Adjust overlay opacity:"),
                  choices = seq(0, 1, by = 0.1),
                  selected = 0.8,
                  grid = FALSE
                ),
                tags$div(
                  class = "custom-slider",
                  tags$h4(i18n$t("Recreation Potential Filter")),
                  tags$p(i18n$t("Use the sliders below to filter the data:")),
                  sliderTextInput(
                    inputId = ns("recreation_potential_filter_slider"),
                    label = i18n$t("Filter Recreation Potential:"),
                    choices = seq(0, 1, by = 0.1),
                    selected = 0.3, # c(0, 1),
                    grid = FALSE,
                  ),
                ),
              ),
              # info content
              tags$div(
                id = ns("infoSidebar"),
                class = "d-none",
                tags$h4(i18n$t("How to Use Persona Mapping"), class = "mt-3"),
                tags$div(
                  class = "info-content",
                  tags$h5(HTML(paste0('<i class="fa-solid fa-1"></i> ', i18n$t("Define Your Area")))),
                  tags$p(
                    i18n$t(
                      "Click the square button (■) on the map to activate the drawing tool. Draw a rectangle by clicking and dragging to select the area where you want to compute recreational potential."
                    )
                  ),
                  tags$h5(HTML(paste0('<i class="fa-solid fa-2"></i> ', i18n$t("Customize Your Persona")))),
                  tags$p(i18n$t("Open the Persona Settings (hiker icon) to customize preferences:")),
                  tags$ul(
                    tags$li(
                      tags$strong(i18n$t("Load Preset:")),
                      " ",
                      i18n$t("Select from predefined recreationist types (e.g., Mountain Biker, Bird Watcher)")
                    ),
                    tags$li(tags$strong(i18n$t("Custom Settings:")), " ", i18n$t("Adjust sliders in four categories:")),
                    tags$ul(
                      tags$li(
                        tags$strong(i18n$t("Landscape:")),
                        " ",
                        i18n$t("Preferences for terrain and land cover types")
                      ),
                      tags$li(tags$strong(i18n$t("Natural Features:")), " ", i18n$t("Importance of natural elements")),
                      tags$li(
                        tags$strong(i18n$t("Infrastructure:")),
                        " ",
                        i18n$t("Proximity to facilities and access points")
                      ),
                      tags$li(tags$strong(i18n$t("Water:")), " ", i18n$t("Preferences related to water features"))
                    ),
                    tags$li(
                      tags$strong(i18n$t("Save/Upload:")),
                      " ",
                      i18n$t("Save your custom persona or upload a previously saved one")
                    )
                  ),
                  tags$h5(HTML(paste0('<i class="fa-solid fa-3"></i> ', i18n$t("Compute Recreational Potential")))),
                  tags$p(
                    i18n$t("Once you've drawn your area and set your persona preferences, click the"),
                    " ",
                    tags$strong(HTML(paste0(
                      i18n$t("Compute Recreation"),
                      ' (<i class="fa-solid fa-rotate-right"></i>)'
                    ))),
                    " ",
                    i18n$t(
                      "button. The system will calculate recreational potential based on your preferences and display the results on the map."
                    )
                  ),
                  tags$h5(HTML(paste0('<i class="fa-solid fa-4"></i> ', i18n$t("Explore Results")))),
                  tags$p(i18n$t("Use the Map Layers panel (layer icon) to:")),
                  tags$ul(
                    tags$li(
                      tags$strong(i18n$t("Base Map:")),
                      " ",
                      i18n$t("Switch between Street, Topographical, Satellite, or Greyscale views")
                    ),
                    tags$li(
                      tags$strong(i18n$t("Overlay Layers:")),
                      " ",
                      i18n$t(
                        "Toggle visibility of individual component layers or the final Recreational Potential layer"
                      )
                    )
                  ),
                  tags$p(
                    class = "text-muted mt-3",
                    tags$em(
                      i18n$t(
                        "Tip: Darker the recreational potential values indicate areas better suited to your specified recreationist profile."
                      )
                    )
                  )
                )
              )
            )
          )
        ),
      )
    )
  )
}

# Server function
ces_persona_server <- function(id, ces_selected, i18n, language_change) {
  moduleServer(id, function(input, output, session) {
    persona_config <- load_config()
    layer_names <- persona_config[["Name"]]

    ns <- session$ns
    ces_path <- file.path(config$get("data_path"), "ces")
    first_time <- reactiveVal(TRUE)

    # Create reactive variables outside of observeEvent
    biodiversity_pal <- reactiveVal()
    recreation_pal <- reactiveVal()
    bbox <- reactiveVal()
    bbox_orig <- reactiveVal()
    df_persona <- reactiveVal()
    current_persona <- reactiveVal()
    computed_layers <- reactiveVal(NULL)

    # Reactive variable for displaying info to user
    userInfoText <- reactiveVal("")

    output$userInfo <- renderText({
      userInfoText()
    })

    # Waiter for loading screens
    msg <- list(
      waiter_text(message = tags$h3(i18n$t("Loading data..."), style = "color: #414f2f;"))
    )
    w <- Waiter$new(
      html = msg[[1]],
      color = "rgba(256,256,256,0.9)"
    )

    # Logic for handling the Sliders button click
    observeEvent(input$toggleSliders, {
      runjs(
        paste0(
          '
    var sidebar = document.getElementById("',
          ns("sidebar_persona"),
          '");
    var buttonContainer = document.getElementById("',
          ns("button_container"),
          '");
    var slidersContent = document.getElementById("',
          ns("slidersSidebar"),
          '");
    var mapsContent = document.getElementById("',
          ns("mapsSidebar"),
          '");
    var infoContent = document.getElementById("',
          ns("infoSidebar"),
          '");
    
    if (window.activeSection_persona === "sliders") {
      sidebar.classList.remove("active");
      buttonContainer.classList.remove("moved-persona");
      window.activeSection_persona = null;
    } else {
      sidebar.classList.add("active");
      buttonContainer.classList.add("moved-persona");
      slidersContent.classList.remove("d-none");
      slidersContent.classList.add("d-block");
      mapsContent.classList.add("d-none");
      infoContent.classList.add("d-none");
      window.activeSection_persona = "sliders";
    }
  '
        )
      )
    })

    # Logic for handling the Maps button click
    observeEvent(input$toggleMaps, {
      runjs(
        paste0(
          '
    var sidebar = document.getElementById("',
          ns("sidebar_persona"),
          '");
    var buttonContainer = document.getElementById("',
          ns("button_container"),
          '");
    var slidersContent = document.getElementById("',
          ns("slidersSidebar"),
          '");
    var mapsContent = document.getElementById("',
          ns("mapsSidebar"),
          '");
    var infoContent = document.getElementById("',
          ns("infoSidebar"),
          '");
    
    if (window.activeSection_persona === "maps") {
      sidebar.classList.remove("active");
      buttonContainer.classList.remove("moved-persona");
      window.activeSection_persona = null;
    } else {
      sidebar.classList.add("active");
      buttonContainer.classList.add("moved-persona");
      slidersContent.classList.add("d-none");
      mapsContent.classList.remove("d-none");
      mapsContent.classList.add("d-block");
      infoContent.classList.add("d-none");
      window.activeSection_persona = "maps";
    }
  '
        )
      )
    })

    # Logic for handling the Info button click
    observeEvent(input$toggleInfo, {
      runjs(
        paste0(
          '
    var sidebar = document.getElementById("',
          ns("sidebar_persona"),
          '");
    var buttonContainer = document.getElementById("',
          ns("button_container"),
          '");
    var slidersContent = document.getElementById("',
          ns("slidersSidebar"),
          '");
    var mapsContent = document.getElementById("',
          ns("mapsSidebar"),
          '");
    var infoContent = document.getElementById("',
          ns("infoSidebar"),
          '");
    
    if (window.activeSection_persona === "info") {
      sidebar.classList.remove("active");
      buttonContainer.classList.remove("moved-persona");
      window.activeSection_persona = null;
    } else {
      sidebar.classList.add("active");
      buttonContainer.classList.add("moved-persona");
      slidersContent.classList.add("d-none");
      mapsContent.classList.add("d-none");
      infoContent.classList.remove("d-none");
      infoContent.classList.add("d-block");
      window.activeSection_persona = "info";
    }
  '
        )
      )
    })

    # Logic for basic sidebar closing
    observeEvent(input$closeButton, {
      runjs(paste0(
        '
    var sidebar = document.getElementById("',
        ns("sidebar_persona"),
        '");
    var buttonContainer = document.getElementById("',
        ns("button_container"),
        '");
    sidebar.classList.remove("active");
    buttonContainer.classList.remove("moved-persona");
    window.activeSection_persona = null;
  '
      ))
    })

    # Only trigger when ces_selected() becomes TRUE (after which the value will not change).
    observeEvent(
      ces_selected(),
      ignoreInit = TRUE,
      {
        req(ces_selected() == "CES_Persona", first_time())
        w$show()
        # Create palettes
        biodiversity_pal(colorBin(
          "PuBuGn",
          c(0, 1),
          bins = seq(0, 1, length.out = 5 + 1),
          na.color = "transparent",
          reverse = FALSE,
          alpha = 1
        ))
        recreation_pal(colorBin(
          "YlOrBr",
          c(0, 1.5),
          bins = seq(0, 1, length.out = 5 + 1),
          na.color = "transparent",
          reverse = FALSE,
          alpha = 0.8
        ))

        # Get default personas
        get_preset_persona_file() |>
          read_persona_csv() |>
          df_persona()

        # Update persona selector
        update_selector_personas(
          session,
          df_persona()
        )

        # Update current persona selection
        tmp <- df_persona()[[2]]
        names(tmp) <- df_persona()[["index"]]
        current_persona(tmp)

        # Apply new persona to sliders
        lapply(names(current_persona()), function(layer_name) {
          updateSliderInput(
            session,
            inputId = layer_name,
            value = current_persona()[[layer_name]]
          )
        })

        print("First time CES Persona opened")
        first_time(FALSE)
        w$hide()
      }
    )

    observeEvent(
      input$load_persona,
      {
        req(input$persona_selector)
        selected_persona <- input$persona_selector
        tmp <- df_persona()[[selected_persona]]
        names(tmp) <- df_persona()[["index"]]
        current_persona(tmp)
        # Apply new persona to sliders
        lapply(names(current_persona()), function(layer_name) {
          updateSliderInput(
            session,
            inputId = layer_name,
            value = current_persona()[[layer_name]]
          )
        })
      }
    )

    observeEvent(
      {
        language_change()
        ces_selected()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        runjs(paste0(
          "
            App.fixTooltip('",
          ns("toggleSliders"),
          "');
            App.fixTooltip('",
          ns("toggleMaps"),
          "');
            App.fixTooltip('",
          ns("toggleInfo"),
          "');
          "
        ))
        update_lang(language_change())
      }
    )

    # Render the map in leaflet
    output$combined_map_plot <- renderLeaflet({
      req(ces_selected())

      setup_map(recreation_pal(), ns("button_container"), ns)
    })

    # Observe event for base layer changes
    observeEvent(
      input$map_base_layers,
      {
        req(input$map_base_layers)

        # Hide all layers first
        layers <- c("Esri.WorldStreetMap", "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldGrayCanvas")
        for (layer in layers) {
          leafletProxy(ns("combined_map_plot")) |>
            hideGroup(layer)
        }

        # Show the selected layer
        leafletProxy(ns("combined_map_plot")) |>
          showGroup(input$map_base_layers)
      },
      ignoreInit = TRUE
    )

    # Observe event for overlay layer changes
    observeEvent(
      input$overlay_layers,
      {
        req(input$overlay_layers)

        # Only proceed if layers have been computed (unless selecting "none")
        if (input$overlay_layers != "none") {
          req(computed_layers())
        }
        print(paste0("Overlay layer changed to: ", input$overlay_layers))
        # Get all possible overlay layer names
        all_layers <- c(
          "Landscape & Land Cover",
          "Natural Features",
          "Infrastructure",
          "Water",
          "Recreational Potential"
        )

        proxy <- leafletProxy(ns("combined_map_plot"))

        # Hide all overlay layers first
        for (layer in all_layers) {
          proxy <- proxy |> hideGroup(layer)
        }

        # Show the selected layer if not "none"
        if (input$overlay_layers != "none") {
          print(paste0("Showing overlay layer: ", input$overlay_layers))
          proxy |> showGroup(input$overlay_layers)
        }
      },
      ignoreInit = TRUE
    )

    # Observe event for opacity slider changes
    observeEvent(
      {
        input$overlay_opacity
        input$recreation_potential_filter_slider
      },
      {
        req(input$overlay_opacity)
        req(computed_layers())

        w$show()
        # Update all layers with new opacity using leafletProxy
        leafletProxy(ns("combined_map_plot")) |>
          update_map(
            computed_layers(),
            pal = recreation_pal(),
            opacity = as.numeric(input$overlay_opacity),
            hide_after_add = TRUE,
            filter_threshold = as.numeric(input$recreation_potential_filter_slider)
          )

        # Restore the current overlay selection
        if (!is.null(input$overlay_layers) && input$overlay_layers != "none") {
          leafletProxy(ns("combined_map_plot")) |>
            showGroup(input$overlay_layers)
        }
        w$hide()
      },
      ignoreInit = TRUE
    )

    # Observe event for "Apply filters" button
    observeEvent(
      ignoreInit = TRUE,
      {
        input$apply_filter_recre
      },
      {
        w$show()

        w$hide()
      }
    )

    # Draw rectangle
    # NOTE: input$combined_map_plot_draw_new_feature automatically created by leaflet.extras
    # when using addDrawToolbar()
    observeEvent(input$combined_map_plot_draw_new_feature, {
      bbox <- input$combined_map_plot_draw_new_feature

      stopifnot(bbox$geometry$type == "Polygon")

      # This is pretty hacky - must be a cleaner way...
      coords <- bbox$geometry$coordinates[[1]]
      lons <- unlist(sapply(coords, function(coord) coord[1]))
      lats <- unlist(sapply(coords, function(coord) coord[2]))
      xmin <- min(lons)
      xmax <- max(lons)
      ymin <- min(lats)
      ymax <- max(lats)

      # Fit the map to these bounds
      leafletProxy(ns("combined_map_plot")) |>
        fitBounds(lng1 = xmin, lat1 = ymin, lng2 = xmax, lat2 = ymax)

      # These coords are in EPSG:4326, but our rasters are EPSG:27700
      extent_4326 <- terra::ext(xmin, xmax, ymin, ymax)
      extent_27700 <- terra::project(extent_4326, from = "EPSG:4326", to = "EPSG:27700")

      # Store the SpatExtent as a reactive value
      bbox(extent_27700)

      req(bbox())
      valid_bbox_check <- check_valid_bbox(bbox())
      if (is_error(valid_bbox_check$result)) {
        sendSweetAlert(
          session = session,
          title = "Error in bounding box",
          text = valid_bbox_check$message,
          type = "error",
          html = TRUE, # Enable HTML in text
          customClass = list(
            popup = "swal2-small-text",
            title = "swal2-small-title"
          )
        )
        return()
      }
    })

    # Recompute raster when update button is clicked
    observeEvent(
      input$compute_recreation,
      {
        print("Custom map button clicked!")
        persona <- get_persona_from_sliders(input, layer_names)

        valid_persona_check <- check_valid_persona(persona)
        if (is_error(valid_persona_check$result)) {
          sendSweetAlert(
            session = session,
            title = "Error in persona",
            text = clean_error_message(valid_persona_check$message),
            type = "error",
            html = TRUE, # Enable HTML in text
            customClass = list(
              popup = "swal2-small-text",
              title = "swal2-small-title"
            )
          )
          return()
        }

        if (is.null(bbox())) {
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "Please draw a bounding box on the map to define the area of interest.",
            type = "error",
            html = TRUE, # Enable HTML in text
            customClass = list(
              popup = "swal2-small-text",
              title = "swal2-small-title"
            )
          )
          return()
        }
        valid_bbox_check <- check_valid_bbox(bbox())
        if (is_error(valid_bbox_check$result)) {
          sendSweetAlert(
            session = session,
            title = "Error in bounding box",
            text = valid_bbox_check$message,
            type = "error",
            html = TRUE, # Enable HTML in text
            customClass = list(
              popup = "swal2-small-text",
              title = "swal2-small-title"
            )
          )
          return()
        }

        w$show()
        output <- capture_messages(errors_as_messages(compute_potential))(
          persona,
          file.path(config$get("data_path"), "ces", "recreational_scotland"),
          bbox = bbox()
        )

        w$hide()

        if (is_error(output$result)) {
          sendSweetAlert(
            session = session,
            title = "Error in bounding box",
            text = valid_bbox_check$message,
            type = "error",
            html = TRUE, # Enable HTML in text
            customClass = list(
              popup = "swal2-small-text",
              title = "swal2-small-title"
            )
          )
          return()
        }

        # Store the computed layers
        computed_layers(output$result)

        # Create a proxy for the rendered leaflet map and pass it to update_map
        # Hide layers initially so radio button controls visibility
        tryCatch(
          {
            leafletProxy(ns("combined_map_plot")) |>
              update_map(
                output$result,
                pal = recreation_pal(),
                hide_after_add = TRUE,
                filter_threshold = as.numeric(input$recreation_potential_filter_slider)
              )
          },
          error = function(e) {
            showNotification(paste0("Map update failed: ", conditionMessage(e)), type = "error")
          }
        )

        # Show Recreation layer by default after computation
        updateRadioButtons(session, "overlay_layers", selected = "Recreational Potential")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$upload_persona$datapath,
      {
        req(input$upload_persona$datapath)

        upload_results <- read_persona_csv(input$upload_persona$datapath)
        print(upload_results)
        # Update persona dataframe
        new_columns_names <- setdiff(names(upload_results), names(df_persona()))

        if (length(new_columns_names) == 0) {
          sendSweetAlert(
            session = session,
            title = "No new personas found",
            text = "The uploaded file does not contain any new personas to add.",
            type = "info",
            html = TRUE, # Enable HTML in text
            customClass = list(
              popup = "swal2-small-text",
              title = "swal2-small-title"
            )
          )
          return()
        }

        new_columns <- upload_results |>
          select(c("index", new_columns_names))

        df_persona() |>
          left_join(new_columns, by = "index") |>
          df_persona()

        # Update persona selector
        update_selector_personas(
          session,
          df_persona(),
          selected_persona = input$persona_selector
        )
      }
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Saving by Marsh Rossney                   #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

    make_save_dialog <- function(persona_dir) {
      modalDialog(
        title = "Save Persona",
        textInput(
          ns("savePersonaName"),
          "Enter a unique name for the persona",
          value = "My persona"
        ),
        #actionButton("confirmSave", "Save"),
        #hr(),
        # selectInput(
        #   ns("downloadUserSelect"),
        #   "Download persona File",
        #   choices = c("", list_users(persona_dir)),
        #   selected = ""
        # ),
        downloadButton(ns("confirmDownload"), "Download"),
        footer = modalButton("Cancel")
      )
    }
    save_dialog <- make_save_dialog(persona_dir)
    observeEvent(input$save_persona, {
      showModal(save_dialog)
    })
    observeEvent(input$confirmSave, {
      req(input$savePersonaName)

      persona_name <- input$savePersonaName

      # Remove characters that may cause problems with i/o and dataframe filtering
      persona_name <- make_safe_string(persona_name)

      message <- paste0("Saving persona '", persona_name, "' under user '", user_name, "'")

      extra_messages <- capture_messages(
        save_persona(
          persona = get_persona_from_sliders(input, layer_names),
          csv_path = file.path(persona_dir, paste0(persona_name, ".csv")),
          name = persona_name
        )
      )
      removeModal()
    })

    observeEvent(
      input$upload_button,
      {}
    )

    output$confirmDownload <- downloadHandler(
      filename = function() {
        paste0(input$savePersonaName, ".csv")
      },
      content = function(file) {
        save_persona(
          persona = get_persona_from_sliders(input, layer_names),
          csv_path = file,
          name = input$savePersonaName
        )
        removeModal()
      }
    )
  })
}
