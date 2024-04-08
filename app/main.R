box::use(
  shiny,
  bslib[bs_theme, page_navbar, nav_menu, nav_item, nav_panel],
  shinyjs[useShinyjs],
  waiter[useWaiter, useHostess],
  cicerone[use_cicerone],
)

box::use(
  app / view / info[mod_info_ui],
  app / view / honeybee / honeybee_main[honeybee_ui, honeybee_server],
  app / view / grassland / grassland_main[grassland_ui, grassland_server],
)

# App theme ----
#' @export
biodt_theme <- bs_theme(
  version = 5,
  primary = "#bc6c25",
  secondary = "#414f2f",
  info = "#DDA15E",
  warning = "#6E3E18",
  success = "#f8f2e4",
  bg = "#fff",
  fg = "#414f2f",
  bootswatch = "bootstrap"
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$bootstrapPage(
    theme = biodt_theme,
    # Head ----
    shiny$tags$head(
      shiny$tags$link(rel = "shortcut icon", href = "static/favicon.ico"),
      useShinyjs(),
      useWaiter(),
      useHostess(),
      use_cicerone()
    ),
    # Body ----
    # Main navbar----
    page_navbar(
      window_title = "BioDT",
      title = shiny$div(shiny$a(
        href = "#",
        shiny$img(
          src = "static/logo.svg",
          height = "70px",
          style = "padding-right: 20px"
        ),
      )),
      id = "navbar",
      theme = biodt_theme,
      bg = "#fff",
      fillable = TRUE,
      # must be true
      collapsible = TRUE,
      fluid = TRUE,
      ## Info - main menu item ----
      nav_panel(
        title = "Info",
        value = "info",
        icon = shiny$icon("circle-info"),
        class = "container-fluid index-info",
        mod_info_ui("info")
      ),
      ## Digital Twins - main menu item ----
      nav_menu(
        title = "Digital Twins",
        align = "left",
        icon = shiny$icon("people-group"),
        ### Species response to environment - menu subitem ----
        nav_item(
          shiny::tags$div(
            class = "p-2",
            shiny$icon("temperature-arrow-up"),
            shiny::tags$strong("Species response to environmental change")
          )
        ),
        nav_panel(
          title = "Grassland dynamics",
          grassland_ui(
            ns("grassland"),
            theme = biodt_theme
          )
        ),
        ### Species interactions (themselves, human) - menu subitem ----
        nav_item(
          shiny$div(
            class = "p-2",
            shiny$div(
              shiny$icon("bugs"),
              shiny$strong("Species interactions with each other and with humans"),
              style = "width: 450px"
            ),
          )
        ),
        nav_panel(
          title = "Honeybee",
          class = "p-0",
          honeybee_ui(ns("honeybee_main"),
            theme = biodt_theme
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- shiny$reactiveValues(
      biodt_theme = biodt_theme
    )
    # Honeybee pDT ----
    honeybee_server(
      "honeybee_main",
      r
    )
    # Grassland pDT ----
    grassland_server("grassland", r)
  })
}
