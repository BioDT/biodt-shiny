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
mod_cwr_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      ns("species"),
      label = "Choose species (In the future family and species)",
      choices = list("Lathyrus Sativus" = "lathyrus_sativus")
    ),
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
    ),
    shiny::actionButton(
      ns("recompute"),
      label = "Recompute"
    ),
    leaflet::leafletOutput(ns("map"))
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
    r_cwr <- shiny::reactiveValues(map = leaflet::leaflet() |> leaflet::addTiles(),
                                   modelled_ras = NULL)
    
    # For now there is a CWR variable available which contains and example data for Lathyrus Sativus
    

    # Load data when CWR page opens ----
    observeEvent(r$page_name,
                 {
                   if (r$page_name == "Crop wild relatives and genetic resources for food security") {
                     golem::print_dev("Loading data for CWR")
                     load(biodtshiny_example("CWR_demo.RData"),
                          .GlobalEnv)
                     r_cwr$modelled_ras <- terra::rast(biodtshiny_example("Lathyrus_sativus-Outputs.nc"),
                                        drivers = "NETCDF") |>
                       leaflet::projectRasterForLeaflet(method = "bilinear")
                   }
                 })
    

    shiny::observeEvent(input$recompute,
                        ignoreInit = TRUE,
                        {
                          shiny::req(
                            input$species,
                            input$predicted_presence,
                            input$suitability,
                            r_cwr$modelled_ras,
                            r$page_name == "Crop wild relatives and genetic resources for food security"
                          )
                          if (input$species == "lathyrus_sativus") {
                            layer_names <- c("Suitability",
                                             "Predicted Presence/Absence")
                            
                            modelled_ras_sub <- r_cwr$modelled_ras
                            r_cwr$map <- leaflet::leaflet() |>
                              leaflet::addTiles()
                            # Specific part for the current example ----
                            modelled_ras_sub[[1]] <- modelled_ras_sub |>
                              terra::subset(1) |>
                              terra::clamp(lower = input$suitability[1],
                                           upper = input$suitability[2],
                                           values = FALSE)
                            modelled_ras_sub[[2]] <- modelled_ras_sub |>
                              terra::subset(2) |>
                              terra::clamp(lower = input$predicted_presence[1],
                                           upper = input$predicted_presence[2],
                                           values = FALSE)
                            
                            # This could be probably generic ----
                            n_layers <- dim(modelled_ras_sub)[3]
                            for (i in seq_len(n_layers)) {
                              golem::cat_dev("\nLayer ", i, " is being computed.\n")
                              layer <- modelled_ras_sub |> terra::subset(i)
                              layer_range <-
                                r_cwr$modelled_ras |> terra::minmax() |> range()
                              layer_range[1] <- floor(layer_range[1])
                              layer_range[2] <- ceiling(layer_range[2])
                              layer_bins <-
                                layer |> as.vector() |> unique() |> length()
                              
                              # Setup color palette
                              if (layer_bins > 5) {
                                pal <- leaflet::colorNumeric("viridis",
                                                             domain = layer_range,
                                                             reverse = TRUE,
                                                             na.color = NA)
                              } else {
                                pal <- leaflet::colorFactor(
                                  "RdYlBu",
                                  domain = layer_range,
                                  levels = layer_bins,
                                  reverse = TRUE,
                                  na.color = NA
                                )
                              }
                              
                              # Plot layer
                              r_cwr$map <- r_cwr$map |>
                                leaflet::addRasterImage(
                                  layer,
                                  project = FALSE,
                                  group = layer_names[i],
                                  opacity = max((1 - (i /
                                                        5)), 0.2)
                                ) |>
                                leaflet::addLegend(pal = pal,
                                                   values = layer_range,
                                                   group = layer_names[i],
                                                   labels = layer_names[i])
                            }
                            if (exists("absences_sf")) {
                              r_cwr$map <- r_cwr$map 
                                
                            }
                            
                            
                            r_cwr$map <- r_cwr$map |>
                              leaflet::addLayersControl(position = "topleft",
                                                        overlayGroups = layer_names)
                          }
                        })
    
    output$map <- leaflet::renderLeaflet(r_cwr$map)
    
    
  })
}

## To be copied in the UI
# mod_cwr_ui("cwr_1")

## To be copied in the server
# mod_cwr_server("cwr_1")
