box::use(
  shiny[moduleServer, NS, tagList, tags, uiOutput, renderUI, HTML, observeEvent, reactiveVal, reactive],
  bslib[card, card_header, card_body],
  leaflet[leafletOutput, renderLeaflet, leafletProxy, addCircles, removeShape]
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
            class = "col-lg-4 col-sm-12",
            uiOutput(
              ns("map_coordinates"),
            ),
          )
        ),
        leafletOutput(ns("map_plot"),
                      height = "500px")
      )
    ),
  )
}

#' @export
honeybee_map_server <- function(id,
                                leaflet_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    coordinates_text <- reactiveVal()
    out <- reactiveVal(NULL)
    
    observeEvent(leaflet_map(), {
      output$map_plot <- renderLeaflet(leaflet_map())
    })

   observeEvent(
      input$map_plot_draw_new_feature,
      {
        leafletProxy("map_plot", session) |>
        removeShape(layerId = "circle") |>
        addCircles(
          lng = input$map_plot_draw_new_feature$geometry$coordinates[[1]],
          lat = input$map_plot_draw_new_feature$geometry$coordinates[[2]],
          radius = 5000,
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

    reactive(out())
  })
}
