box::use(
  shiny[moduleServer, NS, tags, actionButton, observeEvent, req],
  bslib[card, card_header],
  shinyjs[disabled, disable, enable],
  purrr[is_empty],
  terra[vect, project]
)

#' @export
beekeeper_control_ui <- function(id) {
  ns <- NS(id)
  card(
    id = ns("control"),
    class = "mt-2 mx-md-3 card-shadow",
    card_header(
      tags$div(
        class = "row d-flex justify-content-between align-items-center my-1",
        tags$div(
          class = "col-md-8 col-sm-12 me-auto",
          tags$h5("Honeybee Beekeeper Case"),
        ),
        tags$div(
          class = "col-md-4 col-sm-12 d-flex flex-row justify-content-end",
          disabled(
            actionButton(
              ns("run_simulation"),
              label = "Run simulation",
              width = "100%",
              class = "btn-secondary",
              style = "max-width: 200px"
            )
          ),
          # shinyjs::disabled(
          #   shiny::actionButton(
          #     ns("load_resources"),
          #     label = "Update resources",
          #     width = "100%",
          #     class = "btn-secondary ms-1",
          #     style = "max-width: 200px"
          #   )
          # )
        )
      )
    )
  )
}

#' @export
beekeeper_control_server <- function(id,
                                     coordinates,
                                     lookup,
                                     parameters,
                                     landuse_map,
                                     w) {
  moduleServer(id, function(input, output, session) {
  
    # Prepare directory for results ----
    # Non-persistent data solution
    # Making a beekeeper dir in the temp
    temp_dir <-  tempdir() |>
      file.path("beekeeper")
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir)
    }
    
    # Run workflow button ----
    observeEvent(
      coordinates(),
      {
        print(coordinates())
        if (!is_empty(coordinates()) &
            !is_empty(lookup()) &
            !is_empty(parameters())) {
          enable("run_simulation")
        } else {
          disable("run_simulation")
        }
      }
    )
    
    # Workflow execution ----
    observeEvent(
      input$run_simulation,
      {
        # Start waiter ----
        w$show()
        
        # Check data ----
        req(
          coordinates(),
          lookup(),
          parameters()
        )
        # Prepare folder structure ----
        
        
        
        # Prepare input data ----
        
        print(coordinates())
        print(lookup())
        print(parameters())
        
        bee_location <- coordinates() |>
          vect(
            geom = c("lon", "lat"),
            crs = "EPSG:4326") |>
          project(landuse_map)
        
        # create buffer around Beehave Location
        clip_buffer <- buffer(coordinates(),
                              width = buffer_size)
        # ... and clip raster to buffer
        location_area <- crop(landuse_map,
                              clip_buffer)
        
        # Run workflow ----
        
        # Update output data ----
        
        # Hide waiter ----
        w$hide()
      }
    )
    
  })
}
