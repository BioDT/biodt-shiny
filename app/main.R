box::use(
  shiny,
  bslib[nav_select, bs_theme, page_navbar, nav_menu, nav_spacer, nav_item, nav_panel],
  shinyjs[useShinyjs],
  waiter[useWaiter, useHostess, waiterShowOnLoad, waiter_hide, spin_loaders],
  cicerone[use_cicerone],
  stringi[stri_rand_strings],
  htmltools[includeScript],
  config,
  shiny.i18n[Translator, usei18n, update_lang],
)

box::use(
  app/view/info[mod_info_ui],
  app/view/acknowledgements[mod_acknowledgements_ui],
  app/view/honeybee/honeybee_main[honeybee_ui, honeybee_server],
  app/view/grassland/grassland_main[grassland_main_ui, grassland_main_server],
  app/view/disease_outbreaks/disease_outbreaks_main[
    disease_outbreaks_main_ui,
    disease_outbreaks_main_server
  ],
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

env_active <- Sys.getenv("R_CONFIG_ACTIVE")

i18n <- Translator$new(translation_json_path = "app/translations/translations.json")
i18n$set_translation_language("en")

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
      use_cicerone(),
      usei18n(i18n)
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
      title = shiny$actionLink(
        inputId = ns("biodt_logo"),
          shiny$img(
            src = "static/logo.svg",
            height = "70px",
            style = "padding-right: 20px"
          ),
      ),
      id = ns("navbar"),
      theme = biodt_theme,
      bg = "#fff",
      fillable = TRUE,
      # must be true
      collapsible = TRUE,
      fluid = TRUE,
      ## Info - main menu item ----
      nav_panel(
        title = i18n$translate("Info"),
        value = "info",
        icon = shiny$icon("circle-info"),
        class = "container-fluid index-info",
        mod_info_ui("info", i18n)
      ),
      ## Digital Twins - main menu item ----
      nav_menu(
        title = i18n$translate("Digital Twin"),
        align = "left",
        icon = shiny$icon("people-group"),
        if (env_active == "dev") {
          nav_item(
            ## Species response to environment - menu subitem ----
            shiny$tags$div(
              class = "p-2",
              shiny$icon("temperature-arrow-up"),
              shiny$tags$strong(i18n$translate("Species response to environmental change"))
            )
          )
        },
        if (env_active == "dev") {
          nav_panel(
            class = "p-0",
            title = i18n$translate("Grassland Dynamics"),
            grassland_main_ui(
              ns("grassland_main")
            )
          )
        },
        ## Species interactions (themselves, human) - menu subitem ----
        nav_item(
          shiny$div(
            class = "p-2",
            shiny$div(
              shiny$icon("bugs"),
              shiny$strong(i18n$translate("Species interactions with each other and with humans")),
              style = "width: 450px"
            ),
          )
        ),
        nav_panel(
          title = i18n$translate("Honeybee"),
          class = "p-0",
          honeybee_ui(ns("honeybee_main"),
            theme = biodt_theme
          )
        ),
        if (env_active == "dev") {
          nav_panel(
            title = i18n$translate("Disease Outbreaks"),
            class = "p-0",
            disease_outbreaks_main_ui(ns("disease_outbreaks_main_ui"), i18n)
          )
        }
      ),
      nav_spacer(),
      ## Acknowledgements - main menu item ----
      nav_panel(
        title = i18n$translate("Acknowledgements"),
        value = "acknowledgements",
        icon = shiny$icon("users-gear"),
        class = "container-fluid index-info",
        mod_acknowledgements_ui("info")
      ),
      if (env_active == "dev") {
        nav_item(
          shiny$bookmarkButton(),
          shiny$selectInput(
            ns("selected_language"),
            shiny$span(), # shiny$p(i18n$translate("Language:")),
            choices = i18n$get_languages(),
            selected = i18n$get_key_translation(),
            width = "75px"
          )
        )
      }
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

    shiny$observeEvent(input$selected_language, {
      update_lang(input$selected_language)
    })

    # Honeybee pDT ----
    honeybee_server(
      "honeybee_main",
      session_dir
    )
    # Grassland pDT ----
    # grassland_main_server("grassland_main")

    shiny$observeEvent(input$biodt_logo, {
      nav_select(id = "navbar",
                 selected = "info",
                 session = session)
    })

    waiter_hide()
  })
}
