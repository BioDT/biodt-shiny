box::use(
  shiny[moduleServer, NS, tagList, tags, uiOutput, renderUI, HTML, observeEvent, reactiveVal, reactive, textOutput, renderText],
  bslib[card, card_header, card_body],
  leaflet[leafletOutput, renderLeaflet, leafletProxy, addCircles, removeShape],
  htmlwidgets[onRender],
)

#' @export
honeybee_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      class = "ms-md-3 card-shadow",
      title = "input_map",
      id = ns("input_map"),
      full_screen = TRUE,
      card_header(tags$h5("Input Map"), ),
      card_body(
        id = ns("map_input_card"),
        tags$div(
          class = "row d-flex justify-content-between",
          tags$div(
            class = "col-lg-6 col-sm-12",
            tags$p("First, click the placement icon and select desired placement on the map."),
            uiOutput(
              ns("map_coordinates"),
            ),
          )
        ),
        leafletOutput(ns("map_plot"),
          height = "500px"
        ),
        textOutput(
          ns("acknowledgment")
        )
      )
    ),
  )
}

#' @export
honeybee_map_server <- function(id,
                                leaflet_map,
                                experiment_list,
                                map_acknowledgment = reactiveVal("Land Use Classification 2016 (Preidl et al. RSE 2020)")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    coordinates_text <- reactiveVal()
    out <- reactiveVal(NULL)

    output$acknowledgment <- renderText(map_acknowledgment())


    observeEvent(leaflet_map(), {
      output_map <- leaflet_map() |>
        onRender(paste0("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('#", ns("map_plot"), " input:checked')[0].nextSibling.innerText.substr(1);
            var selectedClass = selectedGroup.replace(' ', '');
            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedClass)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"))

      output$map_plot <- renderLeaflet(output_map)
    })

    observeEvent(
      input$map_plot_draw_new_feature,
      {
        leafletProxy("map_plot", session) |>
          removeShape(layerId = "circle") |>
          addCircles(
            lng = input$map_plot_draw_new_feature$geometry$coordinates[[1]],
            lat = input$map_plot_draw_new_feature$geometry$coordinates[[2]],
            radius = 3000,
            layerId = "circle"
          )
        if (length(input$map_plot_draw_new_feature) > 0) {
          HTML(
            paste(
              "Selected coordinates are: <br>",
              "Latitude: ",
              input$map_plot_draw_new_feature$geometry$coordinates[[2]],
              "<br>",
              "Longitude: ",
              input$map_plot_draw_new_feature$geometry$coordinates[[1]],
              "<br>"
            )
          ) |>
            coordinates_text()

          data.frame(
            lat = input$map_plot_draw_new_feature$geometry$coordinates[[2]],
            lon = input$map_plot_draw_new_feature$geometry$coordinates[[1]]
          ) |>
            out()
        } else {
          coordinates_text("No location selected.")
          out(NULL)
        }
      }
    )

    output$map_coordinates <- renderUI(coordinates_text())

    observeEvent(
      experiment_list(),
      {
        # Add code to add awesomemarker to the map with the name of the list values in the label.
        new_name <- names(experiment_list)[length(experiment_list)]
      }
    )
    
    
    reactive(out())
  })
}
