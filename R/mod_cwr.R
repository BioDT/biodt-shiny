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
  shiny::tagList(
    shiny::div(
      shiny::HTML("<h2>Genus and species</h2>"),
      bslib::layout_column_wrap(
        width = "300px",
        fixed_width = TRUE,
        shiny::selectizeInput(
          ns("genus"),
          label = "Choose genus",
          choices = list("Lathyrus")
        ),
        shiny::selectizeInput(
          ns("species"),
          label = "Choose species ",
          choices = list("Sativus")
        )
      ),
    ),
    shiny::div(
      shiny::HTML("<h2>Presence/Absence observation options</h2>"),
      bslib::layout_column_wrap(
        width = "300px",
        fixed_width = TRUE,
        shinyWidgets::pickerInput(
          ns("abs_pres_filter"),
          label = "Choose species for the presence/absence observation visualization",
          choices = list("Lathyrus Sativus"),
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
                         `live-search` = TRUE,
                         container = "body")
        )
      )
    ),
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
        shiny::sliderInput(
          ns("acidity"),
          label = "Select acidity range",
          min = 1,
          max = 12,
          value = c(1, 12)
        )
      )
    ),
    shiny::actionButton(ns("recompute"),
                        label = "Recompute",
                        width = "130px"),
    shiny::div(
      style = "min-height = 500px",
    leaflet::leafletOutput(ns("map"),
                           height = "500px")
    )
  )
}

#' cwr Server Functions
#'
#' @importFrom shiny moduleServer reactiveValues reactive observeEvent req tagList
#' @importFrom leaflet leaflet addTiles setView renderLeaflet
#'
#' @noRd
mod_cwr_server <- function(id,
                           r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize reactiveValues for CWR pDT ----
    r_cwr <-
      shiny::reactiveValues(map = leaflet::leaflet() |> leaflet::addTiles(),
                            modelled_ras = NULL)
    
    # For now there is a CWR variable available which contains and example data for Lathyrus Sativus
    # Loaders ----
    golem::print_dev("Setting up loaders.")
    hostess <- waiter::Hostess$new()
    hostess$set_loader(
      waiter::hostess_loader(
        progress_type = "fill",
        preset = "circle",
        center_page = TRUE,
        min = 1,
        max = 100,
        fill_color = waiter::hostess_stripe(color1 = "#414f2f",
                                            color2 = "#bc6c25"),
        text_color = "#414f2f"
      )
    )
    
    golem::print_dev("Hostess setup. Setting up waiter.")
    waiter <- waiter::Waiter$new(
      html = hostess$get_loader(),
      color = "rgba(256,256,256,0.9)",
      fadeout = 200
    )
    
    # Load data when CWR page opens ----
    observeEvent(r$page_name,
                 {
                   if (r$page_name == "Crop wild relatives and genetic resources for food security") {
                    
                     waiter$show()
                     hostess$set(0)
                     golem::print_dev("Loading data for CWR")
                     load(biodtshiny_example("CWR_demo.RData"),
                          .GlobalEnv)
                     hostess$set(30)
                     r_cwr$modelled_ras <-
                       terra::rast(biodtshiny_example("Lathyrus_sativus-Outputs.nc"),
                                   drivers = "NETCDF") |>
                       leaflet::projectRasterForLeaflet(method = "bilinear")
                     hostess$set(50)
                     shinyWidgets::updatePickerInput(
                       inputId = "abs_pres_filter",
                       choices = unique(c(
                         presences_sf$species, absences_sf$species
                       )),
                       selected = c("Lathyrus sativus")
                     )
                     hostess$set(100)
                     waiter$hide()
                   }
                 })
    
    
    shiny::observeEvent(input$recompute,
                        ignoreInit = TRUE,
                        {
                          shiny::req(
                            input$genus,
                            input$species,
                            input$predicted_presence,
                            input$suitability,
                            r_cwr$modelled_ras,
                            r$page_name == "Crop wild relatives and genetic resources for food security"
                          )
                          
                          # if (input$genus == "lathyrus" &
                          # input$species == "sativus") {
                         
                          
                          golem::print_dev("Showing waiter.")
                          waiter$show()
                          hostess$set(0)
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
                          
                          modelled_ras_sub <- r_cwr$modelled_ras
                          r_cwr$map <- leaflet::leaflet() |>
                            leaflet::addTiles()
                          # Specific part for the current example ----
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
                          
                          hostess$inc(15)
                          # This could be probably quite generic ----
                          n_layers <- dim(modelled_ras_sub)[3]
                          for (i in seq_len(n_layers)) {
                            golem::cat_dev("\nLayer ", i, " is being computed.\n")
                            layer <-
                              modelled_ras_sub |> terra::subset(i)
                            layer_range <-
                              r_cwr$modelled_ras |> terra::subset(i) |> terra::minmax() |> range()
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
                            
                            # Plot layer
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
                            
                            hostess$inc(15)
                          }
                          if (exists("absences_df")) {
                            golem::print_dev("Adding absences in CWR.")
                            temp_data <- absences_df[absences_df$species %in% input$abs_pres_filter, ]
                            golem::print_dev("Subsetted absences in CWR.")
                            if (nrow(temp_data) > 0) {
                              golem::print_dev("Adding absences in CWR. [leaflet]")
                              r_cwr$map <- r_cwr$map |>
                                leaflet::addAwesomeMarkers(
                                  data = temp_data,
                                  lng = ~ lon,
                                  lat = ~ lat,
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
                                  icon = icon.red,
                                  label = ~ species
                                )
                            }
                            
                            hostess$inc(15)
                          }
                          if (exists("presences_df")) {
                            golem::print_dev("Adding presences in CWR.")
                            temp_data <- presences_df[presences_df$species %in% input$abs_pres_filter, ]
                            print(temp_data)
                            if (nrow(temp_data) > 0) {
                              r_cwr$map <- r_cwr$map |>
                                leaflet::addAwesomeMarkers(
                                  data = temp_data,
                                  lng = ~ lon,
                                  lat = ~ lat,
                                  clusterOptions = leaflet::markerClusterOptions(
                                    iconCreateFunction = leaflet::JS(
                                      "function (cluster) {
                                       var childCount = cluster.getChildCount();
                                       return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>',
                                       className: 'marker-cluster marker-cluster-small', iconSize: new L.Point(40, 40) });}"
                                    )
                                  ),
                                  group = "Presences",
                                  icon = icon.green,
                                  label = ~ species
                                )
                            }
                            
                            hostess$inc(15)
                          }
                          if (exists("buffer_sf")) {
                            golem::print_dev("Adding buffer in CWR.")
                            r_cwr$map <- r_cwr$map |>
                              leaflet::addPolygons(data = buffer_sf,
                                                   group = "Buffer")
                          }
                          r_cwr$map <- r_cwr$map |>
                            leaflet::addLayersControl(position = "topleft",
                                                      overlayGroups = layer_names)
                          
                          
                          hostess$set(100)
                          
                          golem::print_dev("Hiding waiter.")
                          waiter$hide()
                        })
    
    output$map <- leaflet::renderLeaflet(r_cwr$map)
    
  })
}

## To be copied in the UI
# mod_cwr_ui("cwr_1")

## To be copied in the server
# mod_cwr_server("cwr_1")
