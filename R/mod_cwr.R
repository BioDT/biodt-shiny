#' cwr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput sliderInput
#' @importFrom leaflet leafletOutput
#' @importFrom shinyWidgets pickerInput
#' @importFrom leaflegend addLegendNumeric
mod_cwr_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(bslib::page_fluid(
    class = "p-0",
    bslib::navset_tab(
      # Map ----
      bslib::nav_panel(
        title = "Map",
        shiny::div(
          shiny::HTML("<h2>Genus and species</h2>"),
          bslib::layout_column_wrap(
            width = "300px",
            fixed_width = TRUE,
            shinyWidgets::pickerInput(
              ns("genus"),
              label = "Choose genus",
              choices = list("Lathyrus"),
              multiple = FALSE,
              options = list(
                `actions-box` = NULL,
                `live-search` = TRUE,
                container = "body"
              )
            ),
            shinyWidgets::pickerInput(
              ns("species"),
              label = "Choose species",
              choices = list("Sativus"),
              multiple = FALSE,
              selected = c("Sativus"),
              options = list(
                `actions-box` = NULL,
                `live-search` = TRUE,
                container = "body"
              )
            )
          ),
        ),
        #shiny::div(
        #  shiny::HTML("<h2>Presence/Absence observation options</h2>"),
        #  bslib::layout_column_wrap(
        #    width = "300px",
        #    fixed_width = TRUE,
        #    shinyWidgets::pickerInput(
        #      ns("abs_pres_filter"),
        #      label = "Choose species for the presence/absence observation visualization",
        #      choices = list("Lathyrus Sativus"),
        #      multiple = TRUE,
        #      options = list(`actions-box` = TRUE,
        #                     `live-search` = TRUE,
        #                     container = "body")
        #    )
        #  )
        #),
        shiny::div(
          shiny::HTML("<h2>Raster filters</h2>"),
          bslib::layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            shiny::sliderInput(
              ns("suitability"),
              label = "Select suitability range",
              min = 0,
              max = 500,
              value = c(0, 500)
            ),
            shiny::sliderInput(
              ns("predicted_presence"),
              label = "Select predicted presence/absence range",
              min = 0,
              max = 1,
              value = c(0, 1)
            )
          )
        ),
        shiny::div(
          shiny::HTML("<h2>Abiotic factors</h2>"),
          bslib::layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            shinyWidgets::pickerInput(
              ns("abiotic_factor_type"),
              label = "Select type of abiotic factor",
              choices = list(
                Nutrient = "nutrient",
                `Soil moisture` = "soil_moisture",
                `Temperature` = "temperature",
                `Toxicity` = "toxicity"
              ),
              multiple = TRUE
            ),
            shinyjs::hidden(
              shiny::sliderInput(
                ns("nutrient"),
                label = "Select nutrient range",
                min = 0,
                max = 1,
                value = c(0, 1)
              )
            ),
            shinyjs::hidden(
              shiny::sliderInput(
                ns("soil_moisture"),
                label = "Select soil moisture range",
                min = 0,
                max = 1,
                value = c(0, 1)
              )
            ),
            shinyjs::hidden(
              shiny::sliderInput(
                ns("temperature"),
                label = "Select acidity range",
                min = -20,
                max = 50,
                value = c(-20, 50)
              )
            ),
            shinyjs::hidden(
              shiny::sliderInput(
                ns("toxicity"),
                label = "Select toxicity range",
                min = 0,
                max = 1,
                value = c(0, 1)
              )
            )
          )
        ),
        shiny::actionButton(ns("recompute"),
                            label = "Recompute",
                            width = "130px"),
        shiny::div(
          style = "min-height: 500px !important",
          class = "html-fill-container html-fill-item",
          leaflet::leafletOutput(ns("map"))
        )
      ),
      
      # Response Curves ----
      bslib::nav_panel(
        title = "Response Curves",
        shiny::div(
          shiny::HTML("<h2>Genus and species</h2>"),
          bslib::layout_column_wrap(
            width = "300px",
            fixed_width = TRUE,
            shinyWidgets::pickerInput(
              ns("genus_response_curves"),
              label = "Choose genus",
              choices = list("Lathyrus"),
              multiple = FALSE,
              options = list(
                `actions-box` = NULL,
                `live-search` = TRUE,
                container = "body"
              )
            ),
            shinyWidgets::pickerInput(
              ns("species_response_curves"),
              label = "Choose species",
              choices = list("Sativus"),
              multiple = TRUE,
              selected = c("Sativus"),
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                container = "body"
              )
            )
          ),
        ),
        shiny::div(
          shiny::HTML("<h2>Raster filters</h2>"),
          bslib::layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            shiny::sliderInput(
              ns("suitability_response_curves"),
              label = "Select suitability range",
              min = 0,
              max = 500,
              value = c(0, 500)
            ),
            shiny::sliderInput(
              ns("predicted_presence_response_curves"),
              label = "Select predicted presence/absence range",
              min = 0,
              max = 1,
              value = c(0, 1)
            )
          )
        ),
        shiny::div(
          shiny::HTML("<h2>Abiotic factors</h2>"),
          bslib::layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            shinyWidgets::pickerInput(
              ns("stressor_response_curves"),
                 choices = c("Nutrient",
                             "Soil Moisture",
                             "Temperature",
                             "Toxicity")
            )
            # shiny::sliderInput(
            #   ns("acidity_response_curves"),
            #   label = "Select acidity range",
            #   min = 1,
            #   max = 12,
            #   value = c(1, 12)
            # )
          )
        ),
        shiny::actionButton(
          ns("recompute_response_curves"),
          label = "Recompute",
          width = "130px"
        ),
        shiny::imageOutput(
          ns("response_curve")
        )
      ),
      
    )
  ))
}

#' cwr Server Functions
#'
#' @importFrom shiny moduleServer reactiveValues reactive observeEvent req tagList
#' @importFrom leaflet leaflet addTiles setView renderLeaflet
#'
#' @noRd
mod_cwr_server <- function(id,
                           r,
                           loaders) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize reactiveValues for CWR pDT ----
    r_cwr <-
      shiny::reactiveValues(map = leaflet::leaflet() |> leaflet::addTiles(),
                            rasters = NULL)
    
    # Path to stored files (should be set for where the files are stored)
    cwr_path <- golem::get_golem_options("cwr_path")
    
    # Set pickers when CWR page opens ----
    observeEvent(r$page_name,
                 {
                   if (r$page_name == "Crop wild relatives and genetic resources for food security") {
                     golem::print_dev("Loading data for CWR")
                     
                     # Get available datasets ----
                     
                     # Genus
                     list_genus <- list.dirs(cwr_path,
                                             recursive = FALSE,
                                             full.names = FALSE)
                     
                     shinyWidgets::updatePickerInput(inputId = "genus",
                                                     choices = list_genus,
                                                     selected = list_genus[1])
                     
                     shinyWidgets::updatePickerInput(inputId = "genus_response_curves",
                                                     choices = list_genus,
                                                     selected = list_genus[1])
                    
                   }
                 })
    # Sync genus and genus_response_curves ----
    
    observeEvent(input$genus,
                 {
                   shinyWidgets::updatePickerInput(inputId = "genus_reponse_curves",
                                                   selected = input$genus)
                 })
    
    observeEvent(input$genus_reponse_curves,
                 {
                   shinyWidgets::updatePickerInput(inputId = "genus",
                                                   selected = input$genus_reponse_curves)
                 })
    
    # Set species based on genus ----
    
    # Since genus and genus_response_curves are synced we do it just for one and set them on both pages
    observeEvent(input$genus,
                 {
                   species_list <- list.files(file.path(cwr_path, input$genus))
                   
                   shinyWidgets::updatePickerInput(inputId = "species",
                                                   choices = species_list,
                                                   selected = species_list[1],
                   )
                   
                   shinyWidgets::updatePickerInput(inputId = "species_response_curves",
                                                   choices = species_list,
                                                   selected = species_list[1],
                   )
                 })
    
    
    # Abiotic factor type change----
    shiny::observeEvent(input$abiotic_factor_type,
                        {
                          golem::print_dev("changig abiotic factor type")
                          abiotic_factor_list <-
                            c("nutrient", "soil_moisture", "temperature", "toxicity")
                          
                          for (abiotic_factor in abiotic_factor_list) {
                            golem::print_dev(abiotic_factor)
                            shinyjs::toggle(abiotic_factor,
                                            condition = abiotic_factor %in% input$abiotic_factor_type)
                          }
                        })
    
    # Connected change in sliders ----
    
    observeEvent(input$suitability,
                 {
                   shiny::updateSliderInput(inputId = "suitability_response_curves",
                                            value = input$suitability)
                 })
    
    observeEvent(input$suitability_response_curves,
                 {
                   shiny::updateSliderInput(inputId = "suitability",
                                            value = input$suitability_response_curves)
                 })
    
    observeEvent(input$predicted_presence,
                 {
                   shiny::updateSliderInput(inputId = "predicted_presence_response_curves",
                                            value = input$predicted_presence)
                 })
    
    observeEvent(input$predicted_presence_response_curves,
                 {
                   shiny::updateSliderInput(inputId = "predicted_presence",
                                            value = input$predicted_presence_response_curves)
                 })
    
    # Species change logic ----
    observeEvent(input$species,
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,
                 {
                   if (exists("Shiny_ls")) {
                    rm(Shiny_ls)
                   }
                   
                   
                   loaders$waiter$show()
                   loaders$hostess$set(0)
                   
                   species_path <- file.path(cwr_path,
                                             input$genus,
                                             input$species)
                   
                   if (is.null(r_cwr$rasters[[input$species]])) {
                     r_cwr$rasters[[input$species]] <-
                       terra::rast(file.path(species_path,
                                             paste0(input$species, "-Outputs.nc")),
                                   drivers = "NETCDF") |>
                       leaflet::projectRasterForLeaflet(method = "bilinear")
                   }
                   
                   
                   loaders$hostess$inc(40)
                   suitability_range <- r_cwr$rasters[[input$species]] |>
                     terra::subset(1) |>
                     terra::minmax() |>
                     range()
                   
                   
                   shiny::updateSliderInput(
                     inputId = "suitability",
                     min = floor(suitability_range[1]),
                     max = ceiling(suitability_range[2]),
                     value = c(floor(suitability_range[1]),
                               ceiling(suitability_range[2]))
                     
                   )
                   
                   loaders$hostess$inc(40)
                   shiny::updateSliderInput(
                     inputId = "suitability_response_curves",
                     min = floor(suitability_range[1]),
                     max = ceiling(suitability_range[2]),
                     value = c(floor(suitability_range[1]),
                               ceiling(suitability_range[2]))
                     
                   )
                   
                   loaders$hostess$set(100)
                   loaders$waiter$hide()
                 })
    
    
    observeEvent(input$species_response_curves,
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,
                 {
                   if (exists("Shiny_ls")) {
                     rm(Shiny_ls)
                   }
                 })
    # Prepare map after clicking recompute ----
    shiny::observeEvent(input$recompute,
                        ignoreInit = TRUE,
                        {
                          shiny::req(
                            input$genus,
                            input$species,
                            input$predicted_presence,
                            input$suitability,
                            r$page_name == "Crop wild relatives and genetic resources for food security"
                          )
                          
                          golem::print_dev("Showing waiter.")
                          loaders$waiter$show()
                          loaders$hostess$set(0)
                          # Predefine variables ----
                          
                          golem::print_dev("Setting up variables.")
                          icon.red <-
                            leaflet::makeAwesomeIcon(icon = NA, markerColor = 'red')
                          icon.green <-
                            leaflet::makeAwesomeIcon(icon = NA, markerColor = 'green')
                          
                          layer_names <- c(
                            "Suitability",
                            "Predicted Presence/Absence",
                            "Presences",
                            "Absences",
                            "Buffer"
                          )
                          titles <- c("Suitability",
                                      "Predicted \nPresence/Absence")
                          palettes <- c("viridis",
                                        "RdYlGn")
                          
                          loaders$hostess$inc(5)
                          # Load input data ----
                          species_path <- file.path(cwr_path,
                                                    input$genus,
                                                    input$species)
                          
                          load(
                            file.path(species_path,
                                      "ShinyData.RData"),
                            .GlobalEnv
                          )
                          
                          loaders$hostess$inc(10)
                          req(r_cwr$rasters[[input$species]],
                              Shiny_ls)
                          
                          modelled_ras_sub <- r_cwr$rasters[[input$species]]
                          r_cwr$map <- leaflet::leaflet() |>
                            leaflet::addTiles()
                          # Subset raster based on inputs ----
                          modelled_ras_sub[[1]] <-
                            modelled_ras_sub |>
                            terra::subset(1) |>
                            terra::clamp(
                              lower = input$suitability[1],
                              upper = input$suitability[2],
                              values = FALSE
                            )
                          modelled_ras_sub[[2]] <-
                            modelled_ras_sub |>
                            terra::subset(2) |>
                            terra::clamp(
                              lower = input$predicted_presence[1],
                              upper = input$predicted_presence[2],
                              values = FALSE
                            )
                          
                          loaders$hostess$inc(10)
                          # Loop through layers ----
                          n_layers <- dim(modelled_ras_sub)[3]
                          for (i in seq_len(n_layers)) {
                            golem::cat_dev("\nLayer ", i, " is being computed.\n")
                            layer <-
                              modelled_ras_sub |> terra::subset(i)
                            layer_range <-
                              r_cwr$rasters[[input$species]] |> terra::subset(i) |> terra::minmax() |> range()
                            layer_range[1] <-
                              floor(layer_range[1])
                            layer_range[2] <-
                              ceiling(layer_range[2])
                            layer_bins <-
                              layer |> as.vector() |> unique() |> length()
                            # Setup color palette
                            if (layer_bins > 5) {
                              pal <- leaflet::colorNumeric(palettes[i],
                                                           domain = layer_range,
                                                           # reverse = TRUE,
                                                           na.color = NA)
                            } else {
                              pal <- leaflet::colorFactor(
                                "Set1",
                                domain = layer_range,
                                levels = layer_bins,
                                na.color = NA
                              )
                            }
                            
                            # Plot layer ----
                            r_cwr$map <- r_cwr$map |>
                              leaflet::addRasterImage(
                                layer,
                                project = FALSE,
                                group = layer_names[i],
                                opacity = 0.7,
                                colors = pal
                              ) |>
                              leaflegend::addLegendNumeric(
                                pal = pal,
                                values = layer_range,
                                group = layer_names[i],
                                title = titles[i],
                                width = 150,
                                height = 20,
                                orientation = "horizontal",
                                position = "bottomright"
                              )
                            
                            loaders$hostess$inc(15)
                          }
                          
                          # Plot absences ----
                          if (!is.null(Shiny_ls$Absences)) {
                            golem::print_dev("Adding absences in CWR.")
                              r_cwr$map <- r_cwr$map |>
                                leaflet::addAwesomeMarkers(
                                  data = Shiny_ls$Absences,
                                  # lng = ~ lon,
                                  # lat = ~ lat,
                                  clusterOptions = leaflet::markerClusterOptions(
                                    iconCreateFunction = leaflet::JS(
                                      "function (cluster) {
                                        var childCount = cluster.getChildCount();
                                        return new L.DivIcon({ html: '<div><span>'
                                        + childCount + '</span></div>',
                                        className: 'marker-cluster marker-cluster-red', iconSize: new L.Point(40, 40) });}"
                                      
                                    )
                                  ),
                                  group = "Absences",
                                  icon = icon.red
                                )
                            
                            loaders$hostess$inc(15)
                          }
                          # Plot presences ----
                          if (!is.null(Shiny_ls$Presences)) {
                            golem::print_dev("Adding presences in CWR.")
                              r_cwr$map <- r_cwr$map |>
                                leaflet::addAwesomeMarkers(
                                  data = Shiny_ls$Presences,
                                  # lng = ~ lon,
                                  # lat = ~ lat,
                                  clusterOptions = leaflet::markerClusterOptions(
                                    iconCreateFunction = leaflet::JS(
                                      "function (cluster) {
                                       var childCount = cluster.getChildCount();
                                       return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>',
                                       className: 'marker-cluster marker-cluster-small', iconSize: new L.Point(40, 40) });}"
                                    )
                                  ),
                                  group = "Presences",
                                  icon = icon.green
                                )
                            
                            loaders$hostess$inc(15)
                          }
                          # Plot buffer ----
                          if (!is.null(Shiny_ls$Buffer)) {
                            golem::print_dev("Adding buffer in CWR.")
                            r_cwr$map <- r_cwr$map |>
                              leaflet::addPolygons(data = Shiny_ls$Buffer,
                                                   group = "Buffer")
                          }
                          r_cwr$map <- r_cwr$map |>
                            leaflet::addLayersControl(position = "topleft",
                                                      overlayGroups = layer_names)
                          
                          loaders$hostess$set(100)
                          
                          golem::print_dev("Hiding waiter.")
                          loaders$waiter$hide()
                        })
    
    output$map <- leaflet::renderLeaflet(r_cwr$map)
    
    # PNG output for response curves ----
    
    output$response_curve <- renderImage(
      {
        req(input$species_response_curves)
        
        list(src = file.path(cwr_path,
                       input$genus_response_curves,
                       input$species_response_curves[1],
                       paste0("RESPCURV_", input$stressor_response_curves, ".png")),
             contentType = "image/png",
             width = 1000,
             height = 600,
             alt = "Response curve")
      },
      deleteFile = FALSE
    )
    
  })
}

## To be copied in the UI
# mod_cwr_ui("cwr_1")

## To be copied in the server
# mod_cwr_server("cwr_1")
