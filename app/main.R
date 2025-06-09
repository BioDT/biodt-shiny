# Core packages
box::use(
  # Core Shiny
  shiny,
  config,
)

# UI Framework
box::use(
  bslib[
    bs_theme,
    page_navbar,
    nav_menu,
    nav_spacer,
    nav_item,
    nav_panel,
    page
  ],
)

# UI Enhancements
box::use(
  # Basic enhancements
  shinyjs[useShinyjs],
  cicerone[use_cicerone],
  htmltools[includeScript],
  stringi[stri_rand_strings],
  waiter[
    useWaiter,
    useHostess,
    waiterShowOnLoad,
    waiter_hide,
    spin_loaders
  ],
)

# Internationalization
box::use(
  shiny.i18n[
    Translator,
    usei18n,
    update_lang
  ],
)

# General Information Views
box::use(
  app/view/info[
    mod_info_ui,
    mod_info_server
  ],
  app/view/acknowledgements[mod_acknowledgements_ui],
)

# Honeybee Beekeeper pDT & Grassland Dynamics
box::use(
  app/view/honeybee/honeybee_main[
    honeybee_ui,
    honeybee_server
  ],
  app/view/grassland/grassland_main[
    grassland_main_ui,
    grassland_main_server
  ],
)

# Cultural Ecosystem Services (CES) pDT
box::use(
  app/view/ces/ces_main[
    ces_ui,
    ces_server
  ],
)

# Disease Outbreaks pDT and Crop Wild Relatives (CWR) pDT
box::use(
  app/view/disease_outbreaks/disease_outbreaks_main[
    disease_outbreaks_main_ui,
    disease_outbreaks_main_server
  ],
  app/view/cwr/cwr_main[
    mod_cwr_ui,
    mod_cwr_server
  ],
)

# Forest Services pDT and Real Time Bird Monitoring (RTBM) pDT
box::use(
  app/view/forest/forest_main[
    forest_main_ui,
    forest_main_server
  ],
  app/view/rtbm/rtbm_main[
    rtbm_ui,
    rtbm_server
  ],
)

# Initialize theme
biodt_theme <- bs_theme(
  version = 5,
  primary = "#414f2f",
  secondary = "#999900",
  success = "#f8f2e4",
  bg = "#fff",
  fg = "#414f2f",
  bootswatch = "bootstrap"
)

env_active <- Sys.getenv("R_CONFIG_ACTIVE")

# Initialize translations
i18n <- Translator$new(
  translation_json_path = "app/translations/translations.json"
)
i18n$set_translation_language("en")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  page(
    theme = biodt_theme,
    # Head ----
    shiny$tags$head(
      shiny$tags$link(
        rel = "shortcut icon",
        href = "static/favicon.ico"
      ),
      useShinyjs(),
      useWaiter(),
      useHostess(),
      use_cicerone(),
      usei18n(i18n),
      includeScript("app/js/tab-index.js"),
      includeScript("app/js/tab-switcher.js"),
    ),
    waiterShowOnLoad(
      html = spin_loaders(
        id = 19,
        color = "#414f2f"
      ),
      color = "#414f2f"
    ),

    # --- Navigation Menu ---
    page_navbar(
      title = tags$div(
        class = "logo-text",
        tags$img(
          src = "static/img/biodt_logo_notext_transparent.png",
          height = "50px",
          alt = "BioDT logo"
        ),
        tags$span(
          i18n$translate("Biodiversity Digital Twin")
        ),
      ),
      underline = TRUE,
      bg = "#fff",
      fluid = TRUE,

      # Info Menu ----
      nav_panel(
        title = i18n$translate("Info"),
        value = "Info",
        icon = shiny$icon("circle-info"),
        mod_info_ui(
          ns("info"),
          i18n
        )
      ),

      # Digital Twin Menu ----
      nav_menu(
        title = i18n$translate("Digital Twin"),
        align = "left",
        icon = shiny$icon("people-group", `aria-hidden` = "true"),

        # --- Species response to environment ---
        nav_item(
          shiny$div(
            class = "p-2",
            shiny$icon("temperature-arrow-up", `aria-hidden` = "true"),
            shiny$tags$strong(
              i18n$translate("Species response to environmental change")
            )
          )
        ),

        # --- Grassland Dynamics ---
        nav_panel(
          class = "p-0",
          title = i18n$translate("Grassland Dynamics"),
          value = "Grassland",
          grassland_main_ui(
            ns("grassland_main"),
            i18n
          )
        ),

        # --- Forest Biodiversity ---
        nav_panel(
          class = "p-0",
          title = i18n$translate("Forest Biodiversity Dynamics"),
          value = "Forest",
          forest_main_ui(
            ns("forest_main"),
            i18n
          )
        ),

        # --- Bird Monitoring ---
        nav_panel(
          class = "p-0",
          title = i18n$translate(
            "Real-time Bird Monitoring with Citizen Science Data"
          ),
          value = "rtbm",
          rtbm_ui(
            ns("rtbm_main"),
            i18n
          )
        ),

        # --- CES ---
        nav_panel(
          class = "p-0",
          title = i18n$translate("Cultural Ecosystem Services"),
          value = "CES",
          ces_ui(
            ns("ces_main"),
            i18n
          )
        ),

        # --- Environmental Change ---
        nav_item(
          shiny$tags$div(
            class = "p-2",
            shiny$icon("temperature-arrow-up"),
            shiny$tags$strong(
              i18n$translate("Species response to environmental change")
            )
          )
        ),

        # --- CWR ---
        nav_panel(
          class = "p-0",
          title = i18n$translate(
            "Crop wild relatives and genetic resources for food security"
          ),
          value = "cwr",
          mod_cwr_ui(
            ns("cwr_main"),
            i18n
          )
        ),

        # --- Disease Outbreaks ---
        nav_panel(
          class = "p-0",
          title = i18n$translate("Disease Outbreaks"),
          value = "disease",
          disease_outbreaks_main_ui(
            ns("disease_outbreaks_main"),
            i18n
          )
        ),

        # --- Species Interactions ---
        nav_item(
          shiny$div(
            class = "p-2",
            shiny$div(
              shiny$icon("bugs", `aria-hidden` = "true"),
              shiny$strong(
                i18n$translate(
                  "Species interactions with each other and with humans"
                )
              ),
              style = "width: 450px"
            ),
          )
        ),

        # --- Honeybee ---
        nav_panel(
          title = i18n$translate("Honeybee"),
          value = "Honeybee",
          class = "p-0",
          honeybee_ui(
            ns("honeybee_main"),
            theme = biodt_theme,
            i18n
          )
        )
      ),

      # --- About Menu ---
      nav_menu(
        title = i18n$translate("About"),
        align = "left",
        icon = shiny$icon("user"),
        nav_panel(
          title = i18n$translate("Acknowledgements"),
          value = "acknowledgements",
          mod_acknowledgements_ui(
            ns("acknowledgements"),
            i18n
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Handle main actions
    mod_info_server("info", session)
    grassland_main_server("grassland_main")
    forest_main_server("forest_main")
    rtbm_server("rtbm_main")
    ces_server("ces_main")
    mod_cwr_server("cwr_main")
    disease_outbreaks_main_server("disease_outbreaks_main")
    honeybee_server("honeybee_main")
  })
}
