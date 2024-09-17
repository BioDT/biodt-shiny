# NOTE: just a provisional temporary draft coded separately as one file in RStudio
# TODO separate and export all functions, incl. leaflet map, export them and
# pass properly down to the Rhino modules, setup proper reactive variables, etc

library(shiny)
library(leaflet)
library(terra)


read_disease_outbreak_tif <- function(map_path) {
    map_raster <- rast(map_path)
}

make_full_tif_map_path <- function(map_name) {
    return(paste0("app/data/disease_outbreak/", map_name, ".tif"))
}

# Shiny's UI part:
ui <- fluidPage(
  titlePanel("Wild Boar Disease - Leaflet map preparation"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "disease_map_select",
        label = "Select disease map:",
        choices = c("Mosaic_final", "outfirst_infection"),
        multiple = FALSE
      )
    ),
    mainPanel(
      leafletOutput("map_output")
    )
  )
)

# server part:
server <- function(input, output) {
  tif_map <- reactiveVal()
  tif_map_path <- reactiveVal()

  observeEvent(input$disease_map_select, {
    # which one of the two tif maps is going to be selected
    make_full_tif_map_path(input$disease_map_select) |>
      tif_map_path()

    # load tif raster, hardcoded for prototype
    tif_map_path() |>
      read_disease_outbreak_tif() |>
      tif_map()

    leaflet_map <- leaflet() |>
      addTiles() |>
      setView(
        lng = 11.8787,
        lat = 51.3919,
        zoom = 3
      ) |>
      addRasterImage(
        tif_map(),
        opacity = 0.75,
        project = FALSE,
        group = "Disease Outbreaks Layer"
      )

    output$map_output <- renderLeaflet(leaflet_map)
  })
}