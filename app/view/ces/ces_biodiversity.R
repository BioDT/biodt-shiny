box::use(
  shiny[moduleServer, NS, tagList, radioButtons, fluidRow, column, textOutput, observeEvent, reactive, renderText,observe,req, HTML,p,tags, showNotification],
  leaflet[leafletOutput, renderLeaflet, leafletProxy, addTiles, addLayersControl, addRasterImage, hideGroup, layersControlOptions,setView, leaflet,clearGroup, showGroup,addProviderTiles,providers,providerTileOptions,tileOptions],
  DT[renderDT, DTOutput],
  dplyr[mutate, select, arrange, left_join, desc],
  terra[rast, crop, values, ext, as.polygons, app, ifel],
  purrr[map_chr, pluck],
  cli[hash_md5],
  utils[read.csv],
  bslib[card,nav_select,card_title,card_body],
  waiter[Waiter],
  app/logic/waiter[waiter_text]
)

#' @export
ces_biodiversity_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      title = "biodiversity_controls",
      full_screen = FALSE,
      max_height = "550px",
      card_title("Biodiversity"),
      card_body(
        radioButtons(
          ns("radio_group_select"),
          "Please select a species group from the list below:",
          c(
            "All biodiversity" = "all",
            "Mammals" = "mammals",
            "Birds" = "birds",
            "Plants" = "plants",
            "Insects" = "insects"
          ),
          inline = TRUE,
          selected = character(0)
        )
      )
    ),
    fluidRow(
      column(12,
             card(
               title = "biodiversity_map",
               full_screen = TRUE,
               max_height = "650px",
               card_title("Biodiversity mapping"),
               card_body(
                 leafletOutput(ns("sp_map"), height = 600, width = "100%"),
                 HTML('<p><span style="background-color: #FFFFCC; color: black;">Low biodiversity</span>
             <span style="background-color: #A1DAB4;color: #A1DAB4;">----</span>
             <span style="background-color: #41B6C4;color: #41B6C4;">----</span>
             <span style="background-color: #2C7FB8;color: #2C7FB8;">----</span>
             <span style="background-color: #253494; color: white;">High biodiversity</span></p>'),
                 textOutput(ns("selected_species"))
               )
             )
      ),
      column(12,
             card(
               title = "sdm_table",
               full_screen = TRUE,
               min_height = "800px",
               card_title("Species list"),
               card_body(
                 min_height = "1200px",
                 p("Click on a species in the species list to show its distribution on the map"),
                 DTOutput(ns('sp_tbl'), height = 1200)
               )
             )
      )
    )
  )
}



#' @export
ces_biodiversity_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    msg <- 
      waiter_text(message = tags$h3("Loading...",
                                    style = "color: #414f2f;"
      ))
    
    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )
    
    # Define the path to the data directory
    ces_path <- "app/data/ces"
    ns <- session$ns
    
    # Load the species list if the file exists, otherwise show an error notification
    if (file.exists(paste0(ces_path, "/cairngorms_sp_list.csv"))) {
      cairngorms_sp_list <- read.csv(paste0(ces_path, "/cairngorms_sp_list.csv"))
    } else {
      showNotification(paste0("File missing: ", paste0(ces_path, "/cairngorms_sp_list.csv")), type= "error", closeButton = TRUE, duration = NULL)
    }
    
    # List all SDM (Species Distribution Models) files and extract taxon IDs from filenames
    all_sdm_files <- list.files(paste0(ces_path, "/sdms"), full.names = TRUE)
    taxon_ids_from_file_names <- list.files(paste0(ces_path, "/sdms"), full.names = FALSE) |>
      map_chr(~ gsub("prediction_(\\d+)_.*", "\\1", .x))
    
    # Combine files and their corresponding taxon IDs into a data frame
    files_and_ids <- data.frame(files = all_sdm_files, ids = taxon_ids_from_file_names)
    
    # Reactive expression to filter selected species based on user input
    selected_species <- reactive({
      req(input$radio_group_select)
      cairngorms_sp_list[cairngorms_sp_list[, input$radio_group_select] == TRUE, ]
    })
    
    # Reactive expression to get the bounding box from the map input
    bounding_box <- reactive({
      # bounds <- input$sp_map_bounds
      # req(bounds)
      # ext(c(bounds$west, bounds$east, bounds$south, bounds$north)) |>
      #   as.polygons(crs = "+proj=longlat")
      ext(c(-4.5, -2.5, 56, 57.5)) |>
        as.polygons(crs = "+proj=longlat")
    })
    
    # Reactive expression to load and filter SDM rasters based on selected species
    sdm_rasts <- reactive({
      ids <- selected_species()$speciesKey
      sdm_files <- files_and_ids$files[files_and_ids$ids %in% ids]
      sdm_rasts <- rast(sdm_files)[[names(rast(sdm_files)) == "constrained"]]
      names(sdm_rasts) <- files_and_ids$ids[files_and_ids$ids %in% ids]
      sdm_rasts
    })
    
    # Reactive expression to load and filter gap rasters based on selected species
    gap_rasts <- reactive({
      ids <- selected_species()$speciesKey
      sdm_files <- files_and_ids$files[files_and_ids$ids %in% ids]
      gap_rasts <- rast(sdm_files)[[names(rast(sdm_files)) == "suitable_unrecorded"]]
      names(gap_rasts) <- files_and_ids$ids[files_and_ids$ids %in% ids]
      gap_rasts
    })
    
    # Reactive expression to compute the total SDM raster by averaging and filtering values
    sdm_rast_total <- reactive({
      req(input$radio_group_select)
      
      rast_out <- app(sdm_rasts(), mean)
      rast_out <- ifel(rast_out < 0.1, NA, rast_out)
      
      rast_out
      
    })
    
    # Reactive expression to arrange species based on the maximum priority of gap rasters
    species_gap_arranged <- reactive({
      req(input$radio_group_select)
      gap_rasts_used <- gap_rasts() |> crop(bounding_box())
      
      out <- data.frame(speciesKey = as.integer(names(gap_rasts_used)))
      
      out$max_priority <- rep(0,dim(gap_rasts_used)[3])
      for (i in 1:(dim(gap_rasts_used)[3])) {
        out$max_priority[i] <- mean(values(gap_rasts_used[[i]]), na.rm = TRUE)
      }
      
      out |> arrange(desc(max_priority))
    })
    
    # Reactive expression to arrange species based on the mean probability of SDM rasters and join additional data
    species_arranged <- reactive({
      req(input$radio_group_select)
      sdm_rasts_used <- sdm_rasts() |> crop(bounding_box())
      
      
      mean_prob_values <- rep(0,dim(sdm_rasts_used)[3])
      for (i in 1:(dim(sdm_rasts_used)[3])) {
        mean_prob_values[i] <- mean(values(sdm_rasts_used[[i]]), na.rm = TRUE)
      }
      
      out <- data.frame(
        speciesKey = as.integer(names(sdm_rasts_used)),
        mean_prob = mean_prob_values
      ) |>
        arrange(desc(mean_prob)) |>
        left_join(cairngorms_sp_list, by = "speciesKey") |>
        left_join(species_gap_arranged(), by = "speciesKey")
      
      out
    })
    
    # Render the initial leaflet map with layers and controls
    output$sp_map <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        addProviderTiles(providers$Esri.WorldImagery, providerTileOptions(zIndex=1000),group="ESRI World Imagery") |>
        addProviderTiles(providers$OpenTopoMap, providerTileOptions(zIndex=1000),group="Open Topo Map") |>
        setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
        addTiles(
          urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
          attribution = "GBIF",
          group = "Biodiversity data"
        ) |>
        addLayersControl(
          baseGroups = c("Open Street Map","ESRI World Imagery","Open Topo Map"),
          overlayGroups = c("Biodiversity hotspots", "Biodiversity data", "Focal species"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        hideGroup("Biodiversity data") |>
        hideGroup("Focal species")
    })
    
    # Observe changes in the selected species group and update the map with the total SDM raster
    observe({
      req(input$radio_group_select)
      w$show()
      leafletProxy(ns("sp_map")) |>
        clearGroup("Biodiversity hotspots") |>
        addRasterImage(
          sdm_rast_total(),
          group = "Biodiversity hotspots",
          opacity = 0.6,
          colors = "YlGnBu",
          options = tileOptions(zIndex = 1000)
        )
      w$hide()
    })
    
    # Render the species table with additional columns for likelihood and priority
    output$sp_tbl <- renderDT({
      species_arranged() |>
        mutate(
          likelihood = cut(mean_prob, breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("Very unlikely", "Unlikely", "Likely", "Very likely")),
          priority = cut(max_priority, breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("Very low", "Low", "High", "Very high")),
          image_url = paste0("<img src='", image_url, "' height='60'></img>"),
          sci_name = paste0("<a target='_blank' href='https://www.gbif.org/species/", speciesKey, "'><i>", sci_name, "</i></a>")
        ) |>
        select(
          "Vernacular name" = common_name,
          "Scientific name" = sci_name,
          #"Observation probability" = likelihood,
          #"Recording priority" = priority,
          " " = image_url
        )
    }, escape = FALSE, selection = 'single', class = 'compact')
    
    # Observe row selection in the species table and update the map with the selected species raster
    observeEvent(input$sp_tbl_rows_selected, {
      selected_species <- species_arranged()[input$sp_tbl_rows_selected,]
      sp_ids_selected <- selected_species$speciesKey
      rast_to_add <- sdm_rasts()[[as.character(sp_ids_selected)]]
      rast_to_add <- ifel(rast_to_add < 0.1, NA, rast_to_add)
      
      
      leafletProxy(ns("sp_map")) |>
        hideGroup("Biodiversity hotspots") |>
        clearGroup("Focal species") |>
        showGroup("Focal species") |>
        addRasterImage(
          rast_to_add,
          group = "Focal species",
          colors = "YlGnBu",
          options = tileOptions(zIndex = 1000),
          opacity = 0.6
        )
      
      output$selected_species <- renderText({
        paste0("Species selected: ", selected_species$common_name)
      })
    })
  })
}
