box::use(
  shiny,
  bslib[navset_tab, nav_panel, layout_column_wrap],
  shinyjs[hidden,],
  leaflet,
  leaflegend[addLegendNumeric],
  terra,
  shinyWidgets[pickerInput, updatePickerInput],
  waiter[Waiter],
)

box::use(
  app/logic/waiter[waiter_text],
)

box::use(
  app/view/cwr/cwr_contributors[cwr_contributors_ui,cwr_contributors_server],
)

mod_cwr_ui <- function(id,
                       i18n) {
  ns <- shiny$NS(id)
  shiny$tagList(
    navset_tab(
      id = ns("tab"),
      # Map ----
      nav_panel(
        title = "Map",
        value = "Map",
        shiny$div(
          shiny$HTML("<h2>Genus and species</h2>"),
          layout_column_wrap(
            width = "300px",
            fixed_width = TRUE,
            pickerInput(
              ns("genus"),
              label = "Choose genus",
              choices = list(""),
              multiple = FALSE,
              options = list(
                `actions-box` = NULL,
                `live-search` = TRUE,
                container = "body"
              )
            ),
            pickerInput(
              ns("species"),
              label = "Choose species",
              choices = list(""),
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
        #shiny$div(
        #  shiny$HTML("<h2>Presence/Absence observation options</h2>"),
        #  layout_column_wrap(
        #    width = "300px",
        #    fixed_width = TRUE,
        #    pickerInput(
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
        shiny$div(
          shiny$HTML("<h2>Raster filters</h2>"),
          layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            shiny$sliderInput(
              ns("suitability"),
              label = "Select suitability range",
              min = 0,
              max = 500,
              value = c(0, 500)
            ),
            shiny$sliderInput(
              ns("predicted_presence"),
              label = "Select predicted presence/absence range",
              min = 0,
              max = 1,
              value = c(0, 1)
            )
          )
        ),
        shiny$div(
          shiny$HTML("<h2>Abiotic factors</h2>"),
          layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            pickerInput(
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
            hidden(
              shiny$sliderInput(
                ns("nutrient"),
                label = "Select nutrient range",
                min = 0,
                max = 1,
                value = c(0, 1)
              )
            ),
            hidden(
              shiny$sliderInput(
                ns("soil_moisture"),
                label = "Select soil moisture range",
                min = 0,
                max = 1,
                value = c(0, 1)
              )
            ),
            hidden(
              shiny$sliderInput(
                ns("temperature"),
                label = "Select acidity range",
                min = -20,
                max = 50,
                value = c(-20, 50)
              )
            ),
            hidden(
              shiny$sliderInput(
                ns("toxicity"),
                label = "Select toxicity range",
                min = 0,
                max = 1,
                value = c(0, 1)
              )
            )
          )
        ),
        shiny$actionButton(ns("recompute"),
                            label = "Recompute",
                            width = "130px"),
        shiny$div(
          style = "min-height: 500px !important",
          class = "html-fill-container html-fill-item",
          leaflet$leafletOutput(ns("map"))
        )
      ),
      
      # Response Curves ----
      nav_panel(
        title = "Response Curves",
        value = "Response Curves",
        shiny$div(
          shiny$HTML("<h2>Genus and species</h2>"),
          layout_column_wrap(
            width = "300px",
            fixed_width = TRUE,
            pickerInput(
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
            pickerInput(
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
        shiny$div(
          shiny$HTML("<h2>Raster filters</h2>"),
          layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            shiny$sliderInput(
              ns("suitability_response_curves"),
              label = "Select suitability range",
              min = 0,
              max = 500,
              value = c(0, 500)
            ),
            shiny$sliderInput(
              ns("predicted_presence_response_curves"),
              label = "Select predicted presence/absence range",
              min = 0,
              max = 1,
              value = c(0, 1)
            )
          )
        ),
        shiny$div(
          shiny$HTML("<h2>Abiotic factors</h2>"),
          layout_column_wrap(
            width = "400px",
            fixed_width = TRUE,
            pickerInput(
              ns("stressor_response_curves"),
              choices = c("Nutrient",
                          "Soil Moisture",
                          "Temperature",
                          "Toxicity")
            )
            # shiny$sliderInput(
            #   ns("acidity_response_curves"),
            #   label = "Select acidity range",
            #   min = 1,
            #   max = 12,
            #   value = c(1, 12)
            # )
          )
        ),
        shiny$actionButton(
          ns("recompute_response_curves"),
          label = "Recompute",
          width = "130px"
        ),
        shiny$imageOutput(
          ns("response_curve")
        )
      ),
      nav_panel(
        title = i18n$translate("Contributors"),
        value = "Contributors",
        cwr_contributors_ui(
          ns("cwr_contributors"),
          i18n
        )
      ),
      
    )
  )
}

mod_cwr_server <- function(id, i18n) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    msg <- 
      waiter_text(message = shiny$tags$h3("Loading...",
                                    style = "color: #414f2f;"
      ))
    
    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )
    
    cwr_ls <- shiny$reactiveVal()
    
    # Initialize reactiveValues for CWR pDT ----
    r_cwr <-
      shiny$reactiveValues(map = leaflet$leaflet() |> leaflet$addTiles(),
                            rasters = NULL)
    
    # Path to stored files (should be set for where the files are stored)
    cwr_path <- "app/data/cwr"
    
    # Set pickers when CWR page opens ----
    shiny$observeEvent(input$tab,
                 {
                   shiny$req(input$tab == "Map") 
                   
                   # Get available datasets ----
                   
                   # Genus
                   list_genus <- list.dirs(cwr_path,
                                           recursive = FALSE,
                                           full.names = FALSE)
                   
                   updatePickerInput(inputId = "genus",
                                                   choices = list_genus,
                                                   selected = list_genus[1])
                   
                   updatePickerInput(inputId = "genus_response_curves",
                                                   choices = list_genus,
                                                   selected = list_genus[1])
                   
                   
                 })
    # Sync genus and genus_response_curves ----
    
    shiny$observeEvent(input$genus,
                 ignoreInit = TRUE,
                 {
                   updatePickerInput(inputId = "genus_reponse_curves",
                                                   selected = input$genus)
                 })
    
    shiny$observeEvent(input$genus_reponse_curves,
                 ignoreInit = TRUE,
                 {
                   updatePickerInput(inputId = "genus",
                                                   selected = input$genus_reponse_curves)
                 })
    
    # Set species based on genus ----
    
    # Since genus and genus_response_curves are synced we do it just for one and set them on both pages
    shiny$observeEvent(input$genus,
                 ignoreInit = TRUE,
                 {
                   species_list <- list.files(file.path(cwr_path, input$genus))
                   
                   updatePickerInput(inputId = "species",
                                                   choices = species_list,
                                                   selected = species_list[1],
                   )
                   
                   updatePickerInput(inputId = "species_response_curves",
                                                   choices = species_list,
                                                   selected = species_list[1],
                   )
                 })
    
    
    # Abiotic factor type change----
    shiny$observeEvent(input$abiotic_factor_type,
                        ignoreInit = TRUE,
                        {
                          abiotic_factor_list <-
                            c("nutrient", "soil_moisture", "temperature", "toxicity")
                          
                          for (abiotic_factor in abiotic_factor_list) {
                            
                           shinyjs::toggle(abiotic_factor,
                                            condition = abiotic_factor %in% input$abiotic_factor_type)
                          }
                        })
    
    # Connected change in sliders ----
    
    shiny$observeEvent(input$suitability,
                 ignoreInit = TRUE,
                 {
                   shiny$updateSliderInput(inputId = "suitability_response_curves",
                                            value = input$suitability)
                 })
    
    shiny$observeEvent(input$suitability_response_curves,
                 ignoreInit = TRUE,
                 {
                   shiny$updateSliderInput(inputId = "suitability",
                                            value = input$suitability_response_curves)
                 })
    
    shiny$observeEvent(input$predicted_presence,
                 ignoreInit = TRUE,
                 {
                   shiny$updateSliderInput(inputId = "predicted_presence_response_curves",
                                            value = input$predicted_presence)
                 })
    
    shiny$observeEvent(input$predicted_presence_response_curves,
                 ignoreInit = TRUE,
                 {
                   shiny$updateSliderInput(inputId = "predicted_presence",
                                            value = input$predicted_presence_response_curves)
                 })
    
    # Species change logic ----
    shiny$observeEvent(input$species,
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,
                 {
                   
                   shiny$req(input$tab == "Map") 
                   if (!is.null(cwr_ls())) {
                     cwr_ls(NULL)
                   }
                   
                   
                   w$show()
                   #loaders$hostess$set(0)
                   
                   species_path <- file.path(cwr_path,
                                             input$genus,
                                             input$species)
                   
                   if (is.null(r_cwr$rasters[[input$species]])) {
                     r_cwr$rasters[[input$species]] <-
                       terra$rast(file.path(species_path,
                                             paste0(input$species, "-Outputs.nc")),
                                   drivers = "NETCDF") |>
                       leaflet$projectRasterForLeaflet(method = "bilinear")
                   }
                   
                   
                   # loaders$hostess$inc(40)
                   suitability_range <- r_cwr$rasters[[input$species]] |>
                     terra$subset(1) |>
                     terra$minmax() |>
                     range()
                   
                   
                   shiny$updateSliderInput(
                     inputId = "suitability",
                     min = floor(suitability_range[1]),
                     max = ceiling(suitability_range[2]),
                     value = c(floor(suitability_range[1]),
                               ceiling(suitability_range[2]))
                     
                   )
                   
                   # loaders$hostess$inc(40)
                   shiny$updateSliderInput(
                     inputId = "suitability_response_curves",
                     min = floor(suitability_range[1]),
                     max = ceiling(suitability_range[2]),
                     value = c(floor(suitability_range[1]),
                               ceiling(suitability_range[2]))
                     
                   )
                   
                   #loaders$hostess$set(100)
                   w$hide()
                 })
    
    
    shiny$observeEvent(input$species_response_curves,
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,
                 {
                   if (!is.null(cwr_ls())) {
                     cwr_ls(NULL)
                   }
                 })
    # Prepare map after clicking recompute ----
    shiny$observeEvent(input$recompute,
                        ignoreInit = TRUE,
                        {
                          shiny$req(
                            input$genus,
                            input$species,
                            input$predicted_presence,
                            input$suitability,
                            input$tab == "Map"
                          )
                          
                          w$show()
                          #loaders$hostess$set(0)
                          # Predefine variables ----
                          
                          icon.red <-
                            leaflet$makeAwesomeIcon(icon = NA, markerColor = 'red')
                          icon.green <-
                            leaflet$makeAwesomeIcon(icon = NA, markerColor = 'green')
                          
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
                          
                          #loaders$hostess$inc(5)
                          # Load input data ----
                          species_path <- file.path(cwr_path,
                                                    input$genus,
                                                    input$species)
                          
                          load(
                            file.path(species_path,
                                      "ShinyData.RData"),
                            #envir = .GlobalEnv
                          )
                          cwr_ls(Shiny_ls)
                          
                          #loaders$hostess$inc(10)
                          shiny$req(r_cwr$rasters[[input$species]],
                              cwr_ls())
                          
                          modelled_ras_sub <- r_cwr$rasters[[input$species]]
                          r_cwr$map <- leaflet$leaflet() |>
                            leaflet$addTiles()
                          # Subset raster based on inputs ----
                          modelled_ras_sub[[1]] <-
                            modelled_ras_sub |>
                            terra$subset(1) |>
                            terra$clamp(
                              lower = input$suitability[1],
                              upper = input$suitability[2],
                              values = FALSE
                            )
                          modelled_ras_sub[[2]] <-
                            modelled_ras_sub |>
                            terra$subset(2) |>
                            terra$clamp(
                              lower = input$predicted_presence[1],
                              upper = input$predicted_presence[2],
                              values = FALSE
                            )
                          
                          #loaders$hostess$inc(10)
                          # Loop through layers ----
                          n_layers <- dim(modelled_ras_sub)[3]
                          for (i in seq_len(n_layers)) {
                            layer <-
                              modelled_ras_sub |> terra$subset(i)
                            layer_range <-
                              r_cwr$rasters[[input$species]] |> terra$subset(i) |> terra$minmax() |> range()
                            layer_range[1] <-
                              floor(layer_range[1])
                            layer_range[2] <-
                              ceiling(layer_range[2])
                            layer_bins <-
                              layer |> as.vector() |> unique() |> length()
                            # Setup color palette
                            if (layer_bins > 5) {
                              pal <- leaflet$colorNumeric(palettes[i],
                                                           domain = layer_range,
                                                           # reverse = TRUE,
                                                           na.color = NA)
                            } else {
                              pal <- leaflet$colorFactor(
                                "Set1",
                                domain = layer_range,
                                levels = layer_bins,
                                na.color = NA
                              )
                            }
                            
                            # Plot layer ----
                            r_cwr$map <- r_cwr$map |>
                              leaflet$addRasterImage(
                                layer,
                                project = FALSE,
                                group = layer_names[i],
                                opacity = 0.7,
                                colors = pal
                              ) |>
                              addLegendNumeric(
                                pal = pal,
                                values = layer_range,
                                group = layer_names[i],
                                title = titles[i],
                                width = 150,
                                height = 20,
                                orientation = "horizontal",
                                position = "bottomright"
                              )
                            
                            #loaders$hostess$inc(15)
                          }
                          
                          # Plot absences ----
                          if (!is.null(cwr_ls()$Absences)) {
                            r_cwr$map <- r_cwr$map |>
                              leaflet$addAwesomeMarkers(
                                data = cwr_ls()$Absences,
                                # lng = ~ lon,
                                # lat = ~ lat,
                                clusterOptions = leaflet$markerClusterOptions(
                                  iconCreateFunction = leaflet$JS(
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
                            
                            #loaders$hostess$inc(15)
                          }
                          # Plot presences ----
                          if (!is.null(cwr_ls()$Presences)) {
                            r_cwr$map <- r_cwr$map |>
                              leaflet$addAwesomeMarkers(
                                data = cwr_ls()$Presences,
                                # lng = ~ lon,
                                # lat = ~ lat,
                                clusterOptions = leaflet$markerClusterOptions(
                                  iconCreateFunction = leaflet$JS(
                                    "function (cluster) {
                                       var childCount = cluster.getChildCount();
                                       return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>',
                                       className: 'marker-cluster marker-cluster-small', iconSize: new L.Point(40, 40) });}"
                                  )
                                ),
                                group = "Presences",
                                icon = icon.green
                              )
                            
                            #loaders$hostess$inc(15)
                          }
                          # Plot buffer ----
                          if (!is.null(cwr_ls()$Buffer)) {
                            r_cwr$map <- r_cwr$map |>
                              leaflet$addPolygons(data = cwr_ls()$Buffer,
                                                   group = "Buffer")
                          }
                          r_cwr$map <- r_cwr$map |>
                            leaflet$addLayersControl(position = "topleft",
                                                      overlayGroups = layer_names)
                          
                          #loaders$hostess$set(100)
                          w$hide()
                        })
    
    output$map <- leaflet$renderLeaflet(r_cwr$map)
    
    # PNG output for response curves ----
    
    output$response_curve <- shiny$renderImage(
      {
        shiny$req(input$species_response_curves)
        
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
