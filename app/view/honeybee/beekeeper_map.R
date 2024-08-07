box::use(shiny[moduleServer, NS, tagList, tags, uiOutput, renderUI, HTML, observeEvent, reactiveVal, reactive, textOutput, renderText],
         bslib[card, card_header, card_body],
         leaflet[removeLayersControl, setView, leafletOutput, renderLeaflet, leafletProxy, addCircles, removeShape, clearControls],
         htmlwidgets[onRender],
         terra[vect, extract, project, buffer, crop],
         )

box::use(
  app/logic/honeybee/honeybee_beekeeper_map[honeybee_leaflet_map],
)

#' @export
honeybee_map_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    card(
      class = "ms-md-3 card-shadow",
      title = "input_map",
      id = ns("input_map"),
      full_screen = TRUE,
      card_header(
        tags$h2(
          class = "card_title",
          "Input Map"
      )),
      card_body(
        id = ns("map_input_card"),
        tags$div(
          class = "row d-flex justify-content-between",
          tags$div(
            class = "col-lg-6 col-sm-12",
            tags$p(
              tags$b("Click the"),
              shiny::icon("location-dot"),
              tags$b("icon"),
              "and",
              tags$b("select"),
              "desired placement on the map."
            ),
            uiOutput(ns("map_coordinates"),),
          ),
          tags$div(
            class = "col-lg-6 col-sm-12",
            leafletOutput(ns("map_mini"), height = "200px"),
          )
        ),
        leafletOutput(ns("map_plot"),
                      height = "500px"),
        textOutput(ns("acknowledgment"))
      )
    ),
  )
}

#' @export
honeybee_map_server <- function(id,
                                leaflet_map,
                                experiment_list,
                                map,
                                map_acknowledgment = reactiveVal("Land Use Classification 2016 (Preidl et al. RSE 2020)")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    coordinates_text <- reactiveVal()
    out <- reactiveVal(NULL)
    zoomed_map <- reactiveVal(NULL)
    
    output$acknowledgment <- renderText(map_acknowledgment())
    
    observeEvent(leaflet_map(), {
      output_map <- leaflet_map() |>
        onRender(
          paste0(
            "
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('#",
      ns("map_plot"),
      " input:checked')[0].nextSibling.innerText.substr(1);
            var selectedClass = selectedGroup.replace(' ', '');
            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedClass)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"
          )
        )
      
      output$map_plot <- renderLeaflet(output_map)
    })
    
    observeEvent(input$map_plot_draw_new_feature,
                 {
                   lat <- input$map_plot_draw_new_feature$geometry$coordinates[[2]]
                   long <-
                     input$map_plot_draw_new_feature$geometry$coordinates[[1]]
                   
                   leafletProxy("map_plot", session) |>
                     removeShape(layerId = "circle") |>
                     addCircles(
                       lng = long,
                       lat = lat,
                       radius = 3000,
                       layerId = "circle"
                     )
                   
                   
                   if (length(input$map_plot_draw_new_feature) > 0) {
                     # https://www.paulamoraga.com/book-spatial/the-terra-package-for-raster-and-vector-data.html#vector-data-1
                     pts_long <- c(long)
                     pts_lat <- c(lat)
                     
                     pts_longlat <- cbind(pts_long, pts_lat)
                     
                     pts <- vect(pts_longlat, crs = "epsg:4326") |>
                       project("epsg:3857")
                     
                     extracted <- extract(map(), pts)
                     
                     if (is.na(extracted$category) || extracted$category == "Unclassified") {
                       HTML(
                         paste(
                           "<span class='text-danger'>",
                           "WARNING! Selected location is outside boundaries.",
                           "</span>"
                         )
                       ) |>
                         coordinates_text()
                       
                       out(NULL)
                       zoomed_map(NULL)
                     } else {
                       HTML(
                         paste(
                           "Selected coordinates are: <br>",
                           "Latitude: ",
                           lat,
                           "<br>",
                           "Longitude: ",
                           long,
                           "<br>"
                         )
                       ) |>
                         coordinates_text()
                       
                       # Choose area around the selected point
                       clip_buffer <- buffer(pts, 3000)
                       # Crop area from the map and create unscaled map for minimap
                       crop(map(), clip_buffer) |>
                         honeybee_leaflet_map(main_map_features = FALSE, scale = FALSE) |>
                         zoomed_map()

                       observeEvent(zoomed_map(), {
                         output_zoomed <- zoomed_map() |>
                           setView(
                             long,
                             lat,
                             zoom = 13
                           )


                         leafletProxy("map_mini", session) |>
                           removeShape(layerId = "circle") |>
                           addCircles(
                             lng = long,
                             lat = lat,
                             radius = 3000,
                             layerId = "circle"
                           )

                         output$map_mini <- renderLeaflet(output_zoomed)
                       })

                       # Sent coordinates to out reactive value to use it outside module
                       data.frame(lat = lat,
                                  lon = long) |>
                         out()
                     }
                   } else {
                     coordinates_text("No location selected.")
                     out(NULL)
                     zoomed_map(NULL)
                   }
                 })
    
    output$map_coordinates <- renderUI(coordinates_text())
    
    observeEvent(experiment_list(),
                 {
                   # Add code to add awesomemarker to the map with the name of the list values in the label.
                   new_name <- names(experiment_list)[length(experiment_list)]
                 })
    
    
    reactive(out())
  })
}
