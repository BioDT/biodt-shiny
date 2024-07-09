box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent,radioButtons,HTML,p,textOutput,renderText,showNotification],
  bslib[card,nav_select,card_title,card_body],
  leaflet[leaflet,leafletOutput, renderLeaflet, leafletProxy,colorBin,removeLayersControl,addLayersControl,setView,addTiles,addRasterImage,hideGroup,showGroup,addProviderTiles,providerTileOptions,providers,tileOptions],
  terra[rast, values]
)


#' @export
ces_rp_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      title = "rec_pot_map",
      full_screen = TRUE,
      card_title("Recreation potential mapping"),
      card_body(
        radioButtons(
          ns("persona"),
          "Please select a recreation potential persona from the list below:",
          choiceNames = c("Hard recreationalist - visitors who prefer high-adrenaline activities that require a high level of fitness",
                          "Soft recreationalist - who prefer calmer activities that do not require a high fitness level"),
          choiceValues = c("hard", "soft"),
          width = "100%",
          selected = character(0)
        ),
        leafletOutput(ns("rec_pot_map"), height = 600),
        HTML('<p>
             <span style="background-color: #FFFFCC; color: black;">Low recreation potential</span>
             <span style="background-color: #A1DAB4;color: #A1DAB4;">----</span>
             <span style="background-color: #41B6C4;color: #41B6C4;">----</span>
             <span style="background-color: #2C7FB8;color: #2C7FB8;">----</span>
             <span style="background-color: #253494; color: white;">High recreation potential</span></p>'),
        p("Recreation Potential (RP), an estimate of the potential capacity of a landscapes to provide opportunities for outdoor recreation, parameterized by scoring landscape features such as water bodies, types of forest.")
      )
    )
  )
}

#' @export
ces_rp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ces_path <- "app/data/ces"

    ns <- session$ns
    
    output$rec_pot_map <- renderLeaflet({
      leaflet() |> addTiles()
    })
    
    output$rec_pot_map <- renderLeaflet({
      hard_rec <- tryCatch({
        rast(paste0(ces_path, "/RP_maps/recreation_potential_HR_4326_agg.tif"))
      }, error = function(e) {
        showNotification(e$message, type = "error", closeButton = TRUE, duration = NULL)
        NULL
      })

      soft_rec <- tryCatch({
        rast(paste0(ces_path, "/RP_maps/recreation_potential_SR_4326_agg.tif"))
      }, error = function(e) {
        showNotification(e$message, type = "error", closeButton = TRUE, duration = NULL)
        NULL
      })

      hard_pal <- colorBin("YlGnBu", values(hard_rec), bins = c(0,0.25,0.3,0.33,0.36,0.39,0.45,1), na.color = "transparent",reverse = F)
      soft_pal <- colorBin("YlGnBu", values(soft_rec), bins = c(0,0.25,0.3,0.33,0.36,0.39,0.45,1), na.color = "transparent",reverse = F)

      leaflet() |>
        addTiles(group = "Open Street Map") |>
        addProviderTiles(providers$Esri.WorldImagery, providerTileOptions(zIndex=-1000),group="ESRI World Imagery") |>
        addProviderTiles(providers$OpenTopoMap, providerTileOptions(zIndex=-1000),group="Open Topo Map") |>
        setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
        addLayersControl(
          baseGroups = c("Open Street Map","ESRI World Imagery","Open Topo Map"),
          overlayGroups = c("Hard recreationalist", "Soft recreationalist")
        ) |>
        addRasterImage(hard_rec, group = "Hard recreationalist", opacity = 0.75, colors = hard_pal,options = tileOptions(zIndex = 1000)) |>
        addRasterImage(soft_rec, group = "Soft recreationalist", opacity = 0.75, colors = soft_pal,options = tileOptions(zIndex = 1000)) |>
        hideGroup("Hard recreationalist") |>
        hideGroup("Soft recreationalist")
    })

    observeEvent(input$persona, {
      if (input$persona == "hard") {
        leafletProxy(ns("rec_pot_map")) |>
          hideGroup("Soft recreationalist") |>
          showGroup("Hard recreationalist")
      } else {
        leafletProxy(ns("rec_pot_map")) |>
          hideGroup("Hard recreationalist") |>
          showGroup("Soft recreationalist")
      }
    })
  })
}
