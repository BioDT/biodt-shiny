box::use(shiny[moduleServer, NS, tagList, tags, uiOutput, renderUI, HTML, observeEvent, reactiveVal, reactive, textOutput, renderText],
         bslib[card, card_header, card_body],
         leaflet[leafletOutput, renderLeaflet, leafletProxy, addCircles, removeShape],
         htmlwidgets[onRender],
         terra[vect, extract, project])

#' @export
honeybee_map_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    card(
      class = "ms-md-3 card-shadow",
      title = "input_map",
      id = ns("input_map"),
      full_screen = TRUE,
      card_header(tags$h5("Input Map"),),
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
                     
                     print(pts)
                     extracted <- extract(map(), pts)
                     print(extracted)
                     
                     if (is.na(extracted$category) ||
                         extracted$category == "Unclassified") {
                       HTML(
                         paste(
                           "<span class='text-danger'>",
                           "WARNING! Selected location is outside boundaries.",
                           "</span>"
                         )
                       ) |>
                         coordinates_text()
                       
                       out(NULL)
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
                       
                       data.frame(lat = lat,
                                  lon = long) |>
                         out()
                     }
                   } else {
                     coordinates_text("No location selected.")
                     out(NULL)
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
