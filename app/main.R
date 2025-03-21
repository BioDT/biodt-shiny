box::use(
  shiny,
  bslib[
    nav_select,
    bs_theme,
    page_navbar,
    nav_menu,
    nav_spacer,
    nav_item,
    nav_panel,
    page
  ],
  shinyjs[useShinyjs],
  waiter[useWaiter, useHostess, waiterShowOnLoad, waiter_hide, spin_loaders],
  cicerone[use_cicerone],
  stringi[stri_rand_strings],
  htmltools[includeScript],
  config,
  shiny.i18n[Translator, usei18n, update_lang],
)

box::use(
  app / view / info[mod_info_ui, mod_info_server],
  app / view / acknowledgements[mod_acknowledgements_ui],
  app / view / honeybee / honeybee_main[honeybee_ui, honeybee_server],
  app /
    view /
    grassland /
    grassland_main[grassland_main_ui, grassland_main_server],
  app / view / ces / ces_main[ces_ui, ces_server],
  app /
    view /
    disease_outbreaks /
    disease_outbreaks_main[
      disease_outbreaks_main_ui,
      disease_outbreaks_main_server
    ],
  app / view / cwr / cwr_main[mod_cwr_server, mod_cwr_ui],
  app / view / ias / ias_main[ias_ui, ias_main_server],
  app / view / rtbm / rtbm_main[rtbm_ui, rtbm_main_server],
)

shiny$enableBookmarking("server")
# App theme ----
#' @export
biodt_theme <- bs_theme(
  version = 5,
  primary = "#A86200",
  secondary = "#414f2f",
  info = "#DDA15E",
  warning = "#6E3E18",
  success = "#f8f2e4",
  bg = "#fff",
  fg = "#414f2f",
  bootswatch = "bootstrap"
)

env_active <- Sys.getenv("R_CONFIG_ACTIVE")

i18n <- Translator$new(
  translation_json_path = "app/translations/translations.json"
)
i18n$set_translation_language("en")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  page(
    # shiny$bootstrapPage(
    theme = biodt_theme,
    # Head ----
    shiny$tags$head(
      shiny$tags$link(rel = "shortcut icon", href = "static/favicon.ico"),
      shiny$tags$html(lang = "en"),
      useShinyjs(),
      useWaiter(),
      useHostess(),
      use_cicerone(),
      usei18n(i18n),
      includeScript("app/js/tab-index.js"),
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
          style = "padding-right: 20px",
          alt = "Biodiversity Digital Twin",
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
        icon = shiny$icon("circle-info", `aria-hidden` = "true"),
        class = "container-fluid index-info",
        mod_info_ui(ns("info"), i18n)
      ),
      ## Digital Twins - main menu item ----
      nav_menu(
        title = i18n$translate("Digital Twin"),
        align = "left",
        icon = shiny$icon("people-group", `aria-hidden` = "true"),
        nav_item(
          ## Species response to environment - menu subitem ----
          shiny$tags$div(
            class = "p-2",
            shiny$icon("temperature-arrow-up", `aria-hidden` = "true"),
            shiny$tags$strong(i18n$translate(
              "Species response to environmental change"
            ))
          )
        ),
        nav_panel(
          class = "p-0",
          title = i18n$translate("Grassland Dynamics"),
          value = "Grassland",
          grassland_main_ui(
            ns("grassland_main"),
            i18n
          )
        ),
        nav_panel(
          class = "p-0",
          title = i18n$translate("Cultural Ecosystem Services"),
          value = "CES",
          ces_ui(
            ns("ces_main"),
            i18n
          )
        ),
        nav_item(
          ## Species response to environment - menu subitem ----
          shiny$tags$div(
            class = "p-2",
            shiny$icon("temperature-arrow-up"),
            shiny$tags$strong(i18n$translate(
              "Species response to environmental change"
            ))
          )
        ),
        nav_panel(
          class = "p-0",
          title = i18n$translate(
            "Crop wild relatives and genetic resources for food security"
          ),
          mod_cwr_ui(
            ns("cwr_main"),
            i18n
          )
        ),
        if (env_active == "dev") {
          nav_panel(
            class = "p-0",
            title = i18n$translate(
              "Real-time Bird Monitoring with Citizen Science Data"
            ),
            rtbm_ui(
              ns("rtbm_main"),
              i18n
            )
          )
        },
        ## Species interactions (themselves, human) - menu subitem ----
        nav_item(
          shiny$div(
            class = "p-2",
            shiny$div(
              shiny$icon("bugs", `aria-hidden` = "true"),
              shiny$strong(i18n$translate(
                "Species interactions with each other and with humans"
              )),
              style = "width: 450px"
            ),
          )
        ),
        nav_panel(
          title = i18n$translate("Honeybee"),
          value = "Honeybee",
          class = "p-0",
          honeybee_ui(
            ns("honeybee_main"),
            theme = biodt_theme,
            i18n
          )
        ),
        nav_panel(
          title = i18n$translate("Disease Outbreaks"),
          class = "p-0",
          disease_outbreaks_main_ui(ns("disease_outbreaks_main"), i18n)
        ),
        if (env_active == "dev") {
          ## Dynamics and threats from and for species of policy concern ----
          nav_item(
            shiny$div(
              class = "p-2",
              shiny$div(
                shiny$icon("bugs", `aria-hidden` = "true"),
                shiny$strong(i18n$translate(
                  "Dynamics and threats from and for species of policy concern"
                )),
                style = "width: 450px"
              ),
            )
          )
        },
        if (env_active == "dev") {
          nav_panel(
            title = i18n$translate("Invasive Alien Species"),
            class = "p-0",
            ias_ui(ns("ias_main"), i18n)
          )
        },
      ),
      nav_spacer(),
      ## Acknowledgements - main menu item ----
      nav_panel(
        title = i18n$translate("Acknowledgements"),
        value = "acknowledgements",
        icon = shiny$icon("users-gear", `aria-hidden` = "true"),
        class = "container-fluid index-info",
        mod_acknowledgements_ui("info")
      ),
      if (env_active == "dev") {
        nav_item(
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

    session_dir <- file.path(
      config$get("base_path"),
      paste0(
        Sys.time() |> format(format = "%Y-%m-%d_%H-%M-%S"),
        "_",
        stri_rand_strings(1, 8)
      )
    )

    r <- shiny$reactiveValues(
      biodt_theme = biodt_theme
    )

    # Language change support see shiny.i18n
    shiny$observeEvent(input$selected_language, {
      update_lang(input$selected_language)
    })

    # Info page ----
    mod_info_server(
      "info",
      main_session = session
    )
    # CWR pDT ----
    mod_cwr_server(
      "cwr_main",
      i18n
    )

    # Honeybee pDT ----
    honeybee_server(
      "honeybee_main",
      session_dir
    )
    # Grassland pDT ----
    grassland_main_server(
      "grassland_main"
    )
    # Cultural Ecosystem Services pDT ----
    ces_server(
      "ces_main"
    )
    # Disease Outbreaks pDT ----
    disease_outbreaks_main_server("disease_outbreaks_main", session_dir)
    # Invasie Alien Species pDT ----
    ias_main_server("ias_main")
    # Real-time Bird Monitoring pDT ----
    rtbm_main_server("rtbm_main")

    shiny$observeEvent(input$biodt_logo, {
      nav_select(
        id = "navbar",
        selected = "info",
        session = session
      )
    })

    waiter_hide()
  })
}
