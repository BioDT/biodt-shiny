box::use(
  shiny,
  bslib[bs_theme, page_navbar, nav_menu, nav_spacer, nav_item, nav_panel],
  shinyjs[useShinyjs],
  waiter[useWaiter, useHostess, waiterShowOnLoad, waiter_hide, spin_loaders],
  cicerone[use_cicerone],
  stringi[stri_rand_strings],
  htmltools[includeScript],
  config,
)

box::use(
  app/view/info[mod_info_ui],
  app/view/acknowledgements[mod_acknowledgements_ui],
  app/view/tests[mod_tests_ui],
  app/view/honeybee/honeybee_main[honeybee_ui, honeybee_server],
  app/view/grassland/grassland_main[grassland_main_ui, grassland_main_server],
)

shiny$enableBookmarking("server")
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
  ns <- shiny::NS(id)
  
  main_menu_ui_prod <- function() {
    nav_item(
      ### Species interactions - Honeybee menu subitem ----
      shiny$div(
        class = "p-2",
        shiny$div(
          shiny$icon("bugs"),
          shiny$strong("Species interactions with each other and with humans"),
          style = "width: 450px"
        ),
      )
    )
    nav_panel(
      title = "Honeybee",
      class = "p-0",
      honeybee_ui(ns("honeybee_main"),
        theme = biodt_theme
      )
    )
  }

  man_menu_ui_dev <- function() {
    nav_item(
      ## Species response to environment - menu subitem ----
      shiny$tags$div(
        class = "p-2",
        shiny$icon("temperature-arrow-up"),
        shiny$tags$strong("Species response to environmental change")
      )
    )
    nav_panel(
      class = "p-0",
      title = "Grassland Dynamics",
      grassland_main_ui(
        ns("grassland_main")
      )
    )
    ## Species interactions (themselves, human) - menu subitem ----
    nav_item(
      shiny$div(
        class = "p-2",
        shiny$div(
          shiny$icon("bugs"),
          shiny$strong("Species interactions with each other and with humans"),
          style = "width: 450px"
        ),
      )
    )
    nav_panel(
      title = "Honeybee",
      class = "p-0",
      honeybee_ui(ns("honeybee_main"),
        theme = biodt_theme
      )
    )
  }

  env_active <- Sys.getenv("R_CONFIG_ACTIVE")
  print("Sys.getenv(`R_CONFIG_ACTIVE`):::")
  print(Sys.getenv("R_CONFIG_ACTIVE"))

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
    waiterShowOnLoad(
      html = spin_loaders(
        id = 19,
        color = "#414f2f"
      ),
      color = "rgba(256,256,256,0.9)"
    ),
    includeScript("app/js/popover.js"),
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

        if (env_active == "prod") {
          main_menu_ui_prod()
        } else {
          man_menu_ui_dev()
        }
      ),
      nav_spacer(),
      ## Acknowledgements - main menu item ----
      nav_panel(
        title = "Acknowledgements",
        value = "acknowledgements",
        icon = shiny$icon("users-gear"),
        class = "container-fluid index-info",
        mod_acknowledgements_ui("info")
      ),
      nav_panel(
        title = "Tests",
        value = "tests",
        icon = shiny$icon("microscope"),
        class = "container-fluid index-info",
        mod_tests_ui("tests")
      )
      #,
      # nav_item(
      #   shiny$bookmarkButton()
      # )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_path <- Sys.getenv("BASE_PATH")

    session_dir <- file.path(
      paste0(base_path, "shared"),
      paste0(
        Sys.time() |> format(format = "%Y-%m-%d_%H-%M-%S"),
        "_",
        stri_rand_strings(1, 8)
      )
    )

    r <- shiny$reactiveValues(
      biodt_theme = biodt_theme
    )
    # Honeybee pDT ----
    honeybee_server(
      "honeybee_main",
      session_dir
    )
    # Grassland pDT ----
    # grassland_main_server("grassland_main")

    waiter_hide()
  })
}
