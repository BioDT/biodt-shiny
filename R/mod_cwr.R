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
      ns("acidity"),
      label = "Select acidity range",
      min = 0,
      max = 1,
      value = c(0, 1)
    ),
    shiny::sliderInput(
      ns("occurrence"),
      label = "Select occurence",
      min = 0,
      max = 500,
      value = c(0, 500)
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
    
    # For now there is a CWR variable available which contains and example data for Lathyrus Sativus
    
    CWR <- terra::rast(biodtshiny_example("Lathyrus_sativus-Outputs.nc"),
                       drivers = "NETCDF") |>
      leaflet::projectRasterForLeaflet(method = "bilinear")
    
    
    r_cwr <- shiny::reactiveValues(map = leaflet::leaflet() |> leaflet::addTiles(),
                                   data = CWR)
    shiny::observeEvent(input$recompute,
                        ignoreInit = TRUE,
                        {
                          shiny::req(
                            input$species,
                            input$acidity,
                            input$occurrence,
                            r$page_name == "Crop wild relatives and genetic resources for food security"
                          )
                          if (input$species == "lathyrus_sativus") {
                            r_cwr$map <- leaflet::leaflet() |>
                              leaflet::addTiles()
                            # Specific part for the current example ----
                            r_cwr$data[[1]] <- CWR |>
                              terra::subset(1) |>
                              terra::clamp(lower = input$occurrence[1],
                                           upper = input$occurrence[2])
                            r_cwr$data[[2]] <- CWR |>
                              terra::subset(2) |>
                              terra::clamp(lower = input$acidity[1],
                                           upper = input$acidity[2])
                            
                            # This could be probably generic ----
                            n_layers <- dim(r_cwr$data)[3]
                            for (i in seq_len(n_layers)) {
                              golem::cat_dev("\nLayer ", i, " is being computed.\n")
                              layer <- r_cwr$data |> terra::subset(i)
                              layer_range <-
                                layer |> terra::minmax() |> range() |> round()
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
                                  group = i |> as.character(),
                                  opacity = max((1 - (i /
                                                        5)), 0.2)
                                ) |>
                                leaflet::addLegend(pal = pal,
                                                   values = layer_range,
                                                   group = i |> as.character())
                            }
                            r_cwr$map <- r_cwr$map |>
                              leaflet::addLayersControl(position = "topleft",
                                                        overlayGroups = seq_len(n_layers) |> as.character())
                          }
                        })
    
    output$map <- leaflet::renderLeaflet(r_cwr$map)
    
    
  })
}

## To be copied in the UI
# mod_cwr_ui("cwr_1")

## To be copied in the server
# mod_cwr_server("cwr_1")
