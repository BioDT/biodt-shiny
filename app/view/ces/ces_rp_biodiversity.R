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
    req,
    renderUI,
    uiOutput,
    htmlOutput,
    selectizeInput,
    tags,
    reactiveVal
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
    showGroup,
    clearGroup,
    setMaxBounds,
    labelFormat,
    tileOptions
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
  DT[renderDT, DTOutput],
  dplyr[mutate, select, arrange, left_join, desc, filter, pull],
  purrr[map_chr],
  cli[hash_md5],
  utils[read.csv],
  stats[setNames],
  shinyjs[useShinyjs, runjs, disabled],
  shinyWidgets[
    virtualSelectInput,
    pickerInput,
    sliderTextInput,
    updatePickerInput,
    pickerOptions,
    awesomeCheckbox
  ],
  shiny.i18n[update_lang],
  config,
)

box::use(
  app / logic / ces / ces_map[ces_leaflet_map],
  app /
    logic /
    ces /
    ces_map_update[
      clear_species,
      update_recreation,
      update_base_layers,
      update_species_biodiversity,
      add_species,
      show_species
    ],
  app / logic / waiter[waiter_text],
  app / logic / translate_multiple_choices[translate_multiple_choices],
)

# UI function
ces_rp_biodiversity_ui <- function(id, i18n) {
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
      column(
        12, # Enlarge the map to full width
        card(
          id = "biodiversity-page",
          title = "combined_map",
          full_screen = FALSE,
          card_title(i18n$t("Recreation and Biodiversity Mapping")),
          card_body(
            leafletOutput(ns("combined_map_plot"), height = 800, width = "100%"),
            tags$div(
              class = "button-container",
              id = "openSidebar",
              actionButton(
                ns("toggleSliders"),
                HTML('<i class="fa-solid fa-person-hiking"></i>'),
                class = "toggle-button",
                title = i18n$t("Hiker Settings")
              ),
              actionButton(
                ns("toggleSpecies"),
                HTML('<i class="fa-solid fa-paw"></i>'),
                class = "toggle-button",
                title = i18n$t("Species Occurrence")
              ),
              actionButton(
                ns("toggleMaps"),
                HTML('<i class="fa-solid fa-layer-group"></i>'),
                class = "toggle-button",
                title = i18n$t("Map Layers")
              ),
              # actionButton(ns("toggleGrayscale"), HTML('<i class="fa-solid fa-droplet-slash"></i>'), class = "toggle-button", title = "Grayscale map")
            ),
            # Single Sidebar
            tags$div(
              class = "sidebar",
              id = "sidebar",
              actionButton(
                ns("closeButton"),
                class = "close-button",
                HTML('<i class="fa-solid fa-x"></i>'),
                title = i18n$t("Close Sidebar")
              ),
              # sliders content
              tags$div(
                id = "slidersSidebar",
                class = "d-none",
                tags$h4(i18n$t("Recreation Potential")),
                radioButtons(
                  inputId = ns("recreation_potential"),
                  label = i18n$t("Select recreationist type:"),
                  selected = "Soft",
                  choices = list(
                    Soft = "Soft",
                    Hard = "Hard",
                    Empty = "Empty"
                  )
                ),
                tags$div(
                  class = "custom-slider",
                  tags$h4(i18n$t("Recreation Potential Filter")),
                  tags$p(i18n$t("Use the sliders below to filter the data:")),
                  sliderTextInput(
                    inputId = ns("recreation_potential_slider"),
                    label = i18n$t("Filter Recreation Potential:"),
                    choices = seq(0, 1, by = 0.1),
                    selected = 0.5, # c(0, 1),
                    grid = FALSE,
                  ),
                ),
                actionButton(
                  inputId = ns("apply_filter_recre"),
                  label = i18n$t("Apply filter"),
                  class = "btn-primary"
                ),
              ),
              # species content
              tags$div(
                id = "speciesSidebar",
                class = "d-none",
                tags$h4("Species Selection", class = "mt-3"),
                pickerInput(
                  ns("species_group_selector"),
                  "Select species group:",
                  choices = c(
                    "All biodiversity" = "all",
                    "Mammals" = "mammals",
                    "Birds" = "birds",
                    "Plants" = "plants",
                    "Insects" = "insects"
                  ),
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1)
                ),
                pickerInput(
                  ns("species_selector"),
                  "Select species:",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    `size` = 5,
                    `dropdownAlignRight` = FALSE
                  )
                ),
                tags$div(
                  class = "custom-slider",
                  tags$h4("Species Occurrence", class = "mt-3"),
                  sliderTextInput(
                    inputId = ns("species_occurrence_slider"),
                    label = "Filter Species Occurrence:",
                    choices = seq(0, 1, by = 0.1),
                    selected = 0.5,
                    grid = FALSE,
                  ),
                ),
                actionButton(
                  inputId = ns("apply_filter_species"),
                  label = "Apply filter",
                  class = "btn-primary mb-3"
                ),
                disabled(
                  checkboxInput(
                    inputId = ns("species_occurence"),
                    label = "Show Species Occurence",
                    value = TRUE,
                  )
                ),
              ),
              # map content
              tags$div(
                id = "mapsSidebar",
                class = "d-none",
                tags$h4("Base Map Layer", class = "mt-3"),
                radioButtons(
                  inputId = ns("map_base_layers"),
                  label = "Choose base map:",
                  choices = list(
                    "Open Street Map",
                    "ESRI World Imagery",
                    "Open Topo Map"
                  ),
                  selected = "Open Street Map"
                ),
                tags$h4("Biodiversity Data", class = "mt-3"),
                checkboxInput(
                  inputId = ns("biodiversity"),
                  label = "Biodiversity Occurence Density Layer(GBIF)",
                  value = FALSE
                ),
              )
            )
          )
        ),
      )
    )
  )
}

# Server function
ces_rp_biodiversity_server <- function(id, ces_selected, i18n, language_change) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ces_path <- file.path(config$get("data_path"), "ces")

    # Create reactive variables outside of observeEvent
    key_files <- reactiveVal()
    rec_pot_map <- reactiveVal()
    biodiversity_alpha <- reactiveVal(1)
    recreation_alpha <- reactiveVal(0.8)
    biodiversity_pal <- reactiveVal()
    recreation_pal <- reactiveVal()
    layer_selected <- reactiveVal()
    biodiversity_data_selected <- reactiveVal(FALSE)
    focal_species_merged_raster <- reactiveVal()
    species_added <- reactiveVal(FALSE)
    species_selected <- reactiveVal(FALSE)
    species_files_list <- reactiveVal()
    species_ids_list <- reactiveVal()
    cairngorms_species_list_full <- reactiveVal()
    species_include <- reactiveVal()
    species_choices <- reactiveVal()
    species_include_filtered <- reactiveVal()

    # Waiter for loading screens
    msg <- list(
      waiter_text(message = tags$h3("Loading data...", style = "color: #414f2f;"))
    )
    w <- Waiter$new(
      html = msg[[1]],
      color = "rgba(256,256,256,0.9)"
    )

    # Logic for handling the Sliders button click
    observeEvent(input$toggleSliders, {
      runjs(
        '
    if (window.activeSection === "sliders") {
      App.toggleSidebar(); // Close sidebar if Sliders is already active
      window.activeSection = null; // Reset active section
    } else {
      if (!document.getElementById("sidebar").classList.contains("active")) {
        App.toggleSidebar();
      }
      App.activeRecreation();
      window.activeSection = "sliders";
    }
  '
      )
    })

    # Logic for handling the Species button click
    observeEvent(input$toggleSpecies, {
      runjs(
        ' if (window.activeSection === "species") {
      App.toggleSidebar(); // Close sidebar if Species is already active
      window.activeSection = null;
    } else {
      if (!document.getElementById("sidebar").classList.contains("active")) {
        App.toggleSidebar();
      }
      App.activeSpecies();
      window.activeSection = "species";
    }'
      )
    })

    # Logic for handling the Maps button click
    observeEvent(input$toggleMaps, {
      runjs(
        'if (window.activeSection === "maps") {
      App.toggleSidebar(); // Close sidebar if Maps is already active
      window.activeSection = null;
    } else {
      if (!document.getElementById("sidebar").classList.contains("active")) {
        App.toggleSidebar();
      }
      App.activeMaps();
      window.activeSection = "maps";
    }'
      )
    })

    # Logic for basic sidebar closing
    observeEvent(input$closeButton, {
      runjs("App.toggleSidebar()")
    })

    # Only trigger when ces_selected() becomes TRUE (after which the value will not change).
    observeEvent(
      ces_selected(),
      ignoreInit = TRUE,
      {
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

        species_files_list(list.files(paste0(ces_path, "/sdms"), full.names = TRUE))
        species_ids_list(
          list.files(paste0(ces_path, "/sdms"), full.names = FALSE) |>
            purrr::map_chr(~ gsub("prediction_(\\d+)_.*", "\\1", .x))
        )
        cairngorms_species_list_full(read.csv(paste0(ces_path, "/cairngorms_sp_list.csv")))
        # Load key files
        key_files_list <- list(
          cairngorms_sp_list = read.csv(paste0(ces_path, "/cairngorms_sp_list.csv")),
          files_and_ids = data.frame(
            files = list.files(paste0(ces_path, "/sdms"), full.names = TRUE),
            ids = list.files(paste0(ces_path, "/sdms"), full.names = FALSE) |>
              purrr::map_chr(~ gsub("prediction_(\\d+)_.*", "\\1", .x))
          ),
          hard_rec = terra::rast(paste0(ces_path, "/RP_maps/rec_hard_new.tif")),
          soft_rec = terra::rast(paste0(ces_path, "/RP_maps/rec_soft_new.tif"))
        )

        key_files(key_files_list)

        soft_rec_vals <- terra::values(key_files()$soft_rec)
        soft_rec_filtered <- ifelse(
          soft_rec_vals >= input$recreation_potential_slider,
          soft_rec_vals,
          NA
        )

        # duplicate the raster adn replace with the hard and soft recreation values
        soft_rec_filtered_raster <- key_files()$soft_rec
        terra::values(soft_rec_filtered_raster) <- soft_rec_filtered

        rec_pot_map_plot <- ces_leaflet_map(
          recre_palette = recreation_pal,
          biodiversity_palette = biodiversity_pal,
          rec_opacity = recreation_alpha,
          soft_rec_filt = soft_rec_filtered_raster
        )

        rec_pot_map(rec_pot_map_plot)

        # pre-load Cairngorms's list of species
        species_include(
          cairngorms_species_list_full() |>
            mutate(in_group = (cairngorms_species_list_full() |> pull("all")))
        )

        # make species_selector combobox button disable on CES init
        runjs(paste0(
          '
          let species_selector = document.getElementById("',
          ns("species_selector"),
          '")
          let species_selector_btn = species_selector.nextElementSibling
          species_selector_btn.disabled = true
        '
        ))

        print("First time CES opened")
        w$hide()
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
        update_lang(language_change())
        runjs(paste0(
          "
            App.fixTooltip('", ns("toggleSliders"), "');
            App.fixTooltip('", ns("toggleSpecies"), "');
            App.fixTooltip('", ns("toggleMaps"), "');
          "
        ))
      }
    )

    observe({
      req(input$recreation_potential)

      recreationist_types <- c(
        "Soft" = "Soft",
        "Hard" = "Hard",
        "Empty" = "Empty"
      )

      translate_multiple_choices(
        session,
        "radio",
        input_id = "recreation_potential",
        label = "Select recreationist type:",
        i18n,
        choices_type = "namedlist",
        selected_choice = input$recreation_potential,
        recreationist_types
      )
    })

    # Render the map in leaflet
    output$combined_map_plot <- renderLeaflet({
      req(rec_pot_map())

      rec_pot_map()
    })

    # Update species selector when group is selected
    observeEvent(
      {
        input$species_group_selector
        species_include()
        cairngorms_species_list_full()
      },
      {
        req(input$species_group_selector, species_include(), cairngorms_species_list_full())
        species_include_filtered <- species_include() |>
          mutate(
            in_group = (cairngorms_species_list_full() |>
              pull(input$species_group_selector))
          ) |>
          filter(speciesKey %in% species_ids_list(), in_group == TRUE)

        species_choices(paste0(species_include_filtered$common_name, " (", species_include_filtered$sci_name, ")"))
      }
    )

    # disable species selector based on whether species_group_selector has value
    observeEvent(input$species_group_selector, ignoreInit = TRUE, {
      runjs(paste0(
        '
        let species_selector = document.getElementById("',
        ns("species_selector"),
        '")
        let species_selector_btn = species_selector.nextElementSibling

        let species_group_selector = document.getElementById("',
        ns("species_group_selector"),
        '")
        if (species_group_selector.value > 0) {
          species_selector_btn.disabled = false
        }
      '
      ))
    })

    observeEvent(
      species_choices(),
      {
        req(species_choices())
        print("Updating species selector!")
        updatePickerInput(
          session,
          "species_selector",
          # selected = NULL,
          selected = species_choices()[1],
          choices = species_choices()
        )
      }
    )

    # Helper function to update species layer
    updateSpeciesLayer <- function() {
      if (is.null(input$species_selector) || length(input$species_selector) == 0) {
        species_selected(FALSE)
        clear_species(ns("combined_map_plot"))
      } else {
        selected_species <- sub(".*\\(([^)]+)\\)", "\\1", input$species_selector)
        selected_species_ids <- filter(cairngorms_species_list_full(), sci_name %in% selected_species) |>
          pull(speciesKey)

        clear_species(ns("combined_map_plot"))

        rasters_to_merge <- list()

        for (id in selected_species_ids) {
          file_path <- species_files_list()[species_ids_list() == id]

          if (file.exists(file_path)) {
            rast_to_add <- terra::rast(file_path)[[1]]
            rast_to_add_vals <- terra::values(rast_to_add)
            rast_to_add_filtered <- ifelse(
              rast_to_add_vals >= input$species_occurrence_slider,
              rast_to_add_vals,
              NA
            )
            terra::values(rast_to_add) <- rast_to_add_filtered

            rasters_to_merge <- c(rasters_to_merge, list(rast_to_add))
          } else {
            warning(paste("File not found for species ID:", id))
          }
        }

        if (length(rasters_to_merge) > 0) {
          if (length(rasters_to_merge) == 1) {
            merged_raster <- rasters_to_merge[[1]]
          } else {
            rasters_stack <- terra::rast(rasters_to_merge)
            merged_raster <- terra::app(rasters_stack, fun = mean, na.rm = TRUE)
          }

          # Set zero values to NA to prevent black squares
          merged_raster[merged_raster == 0] <- NA
          focal_species_merged_raster(merged_raster)

          species_added(TRUE)
          species_selected(TRUE)
          add_species(
            ns("combined_map_plot"),
            focal_species_merged_raster(),
            biodiversity_pal
          )
        }

        show_species(ns("combined_map_plot"))
      }
    }

    # Observe event to update the map when the species selector changes
    observeEvent(input$species_selector, {
      w$show()
      updateSpeciesLayer()
      w$hide()
    })

    observeEvent(
      species_selected(),
      {
        if (species_selected() == TRUE) {
          runjs(paste0(
            '
          let species_checkbox_element = document.getElementById("',
            ns("species_occurence"),
            '")
          species_checkbox_element.disabled = false
        '
          ))
        }
        if (species_selected() == FALSE) {
          runjs(paste0(
            "
          species_checkbox_element.disabled = true
        "
          ))
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$species_occurence, {
      if (input$species_occurence == TRUE && species_selected() == TRUE) {
        updateSpeciesLayer()
      }
      if (input$species_occurence == FALSE) {
        clear_species(ns("combined_map_plot"))
      }
    })

    # Observe event for "Apply filters" button
    observeEvent(
      ignoreInit = TRUE,
      {
        input$apply_filter_recre
        input$recreation_potential
      },
      {
        w$show()
        # Update recreation raster layers
        if (input$recreation_potential == "Soft") {
          rec_vals <- key_files()$soft_rec
          rec_vals[rec_vals < input$recreation_potential_slider] <- NA
        } else if (input$recreation_potential == "Hard") {
          rec_vals <- key_files()$hard_rec
          rec_vals[rec_vals < input$recreation_potential_slider] <- NA
        } else {
          rec_vals <- NULL
        }

        # rec_filtered <- ifelse(
        #   rec_vals >= input$recreation_potential_slider,
        #   rec_vals, NA) #|> terra::values()

        # hard_rec_vals <- terra::values(key_files()$hard_rec)
        # soft_rec_vals <- terra::values(key_files()$soft_rec)
        # hard_rec_filtered <- ifelse(
        #   hard_rec_vals >= input$recreation_potential_slider,
        #   hard_rec_vals, NA)
        # soft_rec_filtered <- ifelse(
        #   soft_rec_vals >= input$recreation_potential_slider,
        #   soft_rec_vals, NA)

        # # duplicate the raster adn replace with the hard and soft recreation values
        # hard_rec_filtered_raster <- key_files()$hard_rec
        # soft_rec_filtered_raster <- key_files()$soft_rec
        # terra::values(hard_rec_filtered_raster) <- hard_rec_filtered
        # terra::values(soft_rec_filtered_raster) <- soft_rec_filtered

        # Update the map with the filtered rasters
        update_recreation(
          "filter_recreation",
          ns("combined_map_plot"),
          # hard_recreationists_raster = hard_rec_filtered_raster,
          # soft_recreationists_raster = soft_rec_filtered_raster,
          input_raster = rec_vals,
          recreation_palette = recreation_pal
        )

        w$hide()
      }
    )

    observeEvent(
      input$apply_filter_species,
      {
        w$show()
        updateSpeciesLayer()

        w$hide()
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # observeEvent(input$recreation_potential, {
    #   w$show()

    #   # Update recreation raster layers
    #   hard_rec_vals <- terra::values(key_files()$hard_rec)
    #   soft_rec_vals <- terra::values(key_files()$soft_rec)
    #   hard_rec_filtered <- ifelse(
    #     hard_rec_vals >= input$recreation_potential_slider,
    #     hard_rec_vals, NA)
    #   soft_rec_filtered <- ifelse(
    #     soft_rec_vals >= input$recreation_potential_slider,
    #     soft_rec_vals, NA)

    #   # duplicate the raster adn replace with the hard and soft recreation values
    #   hard_rec_filtered_raster <- key_files()$hard_rec
    #   soft_rec_filtered_raster <- key_files()$soft_rec
    #   terra::values(hard_rec_filtered_raster) <- hard_rec_filtered
    #   terra::values(soft_rec_filtered_raster) <- soft_rec_filtered

    #   update_recreation(
    #     input$recreation_potential,
    #     ns("combined_map_plot"),
    #     soft_recreationists_raster = soft_rec_filtered_raster,
    #     hard_recreationists_raster = hard_rec_filtered_raster,
    #     recreation_palette = recreation_pal
    #   )

    #   w$hide()
    # }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(
      input$map_base_layers,
      {
        w$show()
        layer_selected(input$map_base_layers)
        update_base_layers(layer_selected(), ns("combined_map_plot"))

        w$hide()
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    observeEvent(
      input$biodiversity,
      {
        w$show()
        biodiversity_data_selected(input$biodiversity)
        update_species_biodiversity(biodiversity_data_selected(), ns("combined_map_plot"))

        w$hide()
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
  })
}
