box::use(
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
  shinyjs[
    useShinyjs,
    showLog
  ],
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
  app /
    view /
    info[
      mod_info_ui,
      mod_info_server
    ],
  app / view / acknowledgements[mod_acknowledgements_ui],
)

# Honeybee Beekeeper pDT & Grassland Dynamics & Invasive Alien Species (IAS) pDT
box::use(
  app /
    view /
    honeybee /
    honeybee_main[
      honeybee_ui,
      honeybee_server
    ],
  app /
    view /
    grassland /
    grassland_main[
      grassland_main_ui,
      grassland_main_server
    ],
  app /
    view /
    ias /
    ias_main[
      ias_main_ui,
      ias_main_server
    ],
)

# Cultural Ecosystem Services (CES) pDT
box::use(
  app /
    view /
    ces /
    ces_main[
      ces_ui,
      ces_server
    ],
)

# Disease Outbreaks pDT and Crop Wild Relatives (CWR) pDT
box::use(
  app /
    view /
    disease_outbreaks /
    disease_outbreaks_main[
      disease_outbreaks_main_ui,
      disease_outbreaks_main_server
    ],
  app /
    view /
    cwr /
    cwr_main[
      mod_cwr_ui,
      mod_cwr_server
    ],
)

# Forest Services pDT and Real Time Bird Monitoring (RTBM) pDT
box::use(
  app /
    view /
    forest /
    forest_main[
      forest_main_ui,
      forest_main_server
    ],
  app /
    view /
    rtbm /
    rtbm_main[
      rtbm_main_ui,
      rtbm_main_server
    ],
)

# Initialize theme
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

# Initialize translations
i18n <- Translator$new(
  translation_json_path = "app/translations/multilingual_translations.json"
)
i18n$set_translation_language("en")


# Main UI ----
## a proper solution found here: https://deanattali.com/shinyjs/advanced#usage-navbarpage
## + https://shiny.posit.co/r/articles/build/templates/

wholepage <-
  tagList(
    # --- Navigation Menu ---
    page_navbar(
      html_head,
      # Digital Twin Menu ----
      nav_menu(
        title = i18n$translate("Digital Twin"),
        align = "left",
        icon = shiny$icon("people-group", `aria-hidden` = "true"),

        ## --- Species response to environment ---
        nav_item(
          shiny$div(
            class = "p-2",
            shiny$icon("temperature-arrow-up", `aria-hidden` = "true"),
            shiny$tags$strong(
              i18n$translate("Species response to environmental change")
            )
          )
        ),

        ## --- UI: Grassland Dynamics ---
        nav_panel(
          class = "p-0",
          title = i18n$translate("Grassland Dynamics"),
          value = "Grassland",
          grassland_main_ui(
            ns("grassland_main"),
            i18n
          )
        ),

        ## --- UI: Forest Biodiversity ---
        nav_panel(
          class = "p-0",
          title = i18n$translate("Forest Biodiversity Dynamics"),
          value = "Forest",
          forest_main_ui(
            ns("forest_main"),
            i18n
          )
        ),

        ## --- UI: Bird Monitoring ----
        nav_panel(
          class = "p-0",
          title = i18n$translate(
            "Real-time Bird Monitoring with Citizen Science Data"
          ),
          value = "rtbm",
          rtbm_main_ui(
            ns("rtbm_main"),
            i18n
          )
        ),

        ## --- UI: Cultural Ecosystem Services ----
        nav_panel(
          class = "p-0",
          title = i18n$translate("Cultural Ecosystem Services"),
          value = "CES",
          ces_ui(
            ns("ces_main"),
            i18n
          )
        ),

        ## --- UI: Environmental Change ----
        nav_item(
          shiny$tags$div(
            class = "p-2",
            shiny$icon("temperature-arrow-up"),
            shiny$tags$strong(
              i18n$translate("Species response to environmental change")
            )
          )
        ),

        ## --- UI: CWR ----
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

        ## --- UI: Disease Outbreaks ----
        nav_panel(
          class = "p-0",
          title = i18n$translate("Disease Outbreaks"),
          value = "disease",
          disease_outbreaks_main_ui(
            ns("disease_outbreaks_main"),
            i18n
          )
        ),

        ## --- UI: Species Interactions ----
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

        ## --- UI: Honeybee ----
        nav_panel(
          title = i18n$translate("Honeybee"),
          value = "Honeybee",
          class = "p-0",
          honeybee_ui(
            ns("honeybee_main"),
            i18n
          )
        ),

        nav_panel(
          title = i18n$translate("Disease Outbreaks"),
          value = "disease",
          class = "p-0",
          disease_outbreaks_main_ui(ns("disease_outbreaks_main"), i18n)
        ),

        ## --- UI: Dynamics and threats from and for species of policy concern ----
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
        ),

        ## --- UI: Invasive Alien Species ----
        nav_panel(
          title = i18n$translate("Invasive Alien Species"),
          value = "ias",
          class = "p-0",
          ias_main_ui(
            ns("ias_main"),
            i18n
          )
        ),
      ),
      nav_spacer(),

      # Acknowledgements - main menu item ----
      nav_panel(
        title = i18n$translate("Acknowledgements"),
        value = "acknowledgements",
        icon = shiny$icon("users-gear", `aria-hidden` = "true"),
        class = "container-fluid index-info",
        mod_acknowledgements_ui(
          "info",
          i18n
        ),
        shiny$tags$div(
          class = "text-center",
          shiny$tags$code(
            paste(i18n$get_key_translation())
          )
        )
      ),

      # Lang select - main menu item ----
      nav_item(
        shiny$selectInput(
          ns("selected_language"),
          label = NULL,
          width = "75px",
          choices = i18n$get_languages(),
          selected = i18n$get_key_translation()
        )
      ),
      title = shiny$actionLink(
        inputId = ns("biodt_logo"),
        shiny$img(
          src = "static/logo.svg",
          height = "70px",
          style = "padding-right: 20px",
          alt = i18n$translate("Biodiversity Digital Twin"),
        ),
      ),

      # Info Menu ----
      nav_panel(
        title = i18n$translate("Info"),
        value = "info",
        icon = shiny$icon("circle-info", `aria-hidden` = "true"),
        class = "container-fluid index-info",
        mod_info_ui(ns("info"), i18n)
      ),
      id = ns("navbar"),
      theme = biodt_theme,
      window_title = "Biodiversity Digital Twin",
      bg = "#fff",
      fillable = TRUE,
      collapsible = TRUE, # must be true
      fluid = TRUE,
      lang = i18n$get_key_translation()
    )
  )

# !!!! TO DO TO FINISH !!!! ALL THIS COMMENTED STUFF BELOW TRANSFER INTO THE html-main.html template:: ----
## !!! ALSO DON'T FORGET WHAT IS IN THESE TWO VIGNETTES !!!
## important for including JS properly:
# ?htmltools::renderDocument
# ?htmltools::htmlDependency
# VIZ ALSO INNERS OF shiny::htmlTemplate WHEN CHECKED IN CONSOLE!!!!!
#   # Head ----
# TODO figure out, how to include this:   ns <- NS(id)
#   html_head <- shiny$tags$head(
#     shiny$tags$link(
#       rel = "shortcut icon",
#       href = "static/favicon.ico"
#     ),
#     useShinyjs(),
#     useWaiter(),
#     waiterShowOnLoad(
#       html = spin_loaders(
#         id = 19,
#         color = "#414f2f"
#       ),
#       color = "#414f2f"
#     ),
#     useHostess(),
#     use_cicerone(),
#     usei18n(i18n),
#   )

#' @export
ui <- function(id) {
  shiny::htmlTemplate(
    "html-main.html",
    id = id,
    wholepage = wholepage,
    lang = i18n$get_key_translation()
  )
}

#' @export
server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    showLog("main server started")

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
      print("Language changed")
      update_lang(input$selected_language)
      # shinyjs::runjs(paste0("document.documentElement.lang = '", input$selected_language, "';"))
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
      session_dir,
      i18n
    )
    # Grassland pDT ----
    grassland_main_server(
      "grassland_main",
      i18n
    )
    # Forest pDT ----
    forest_main_server(
      "forest_main",
      i18n
    )
    # Cultural Ecosystem Services pDT ----
    ces_server(
      "ces_main",
      i18n
    )
    # Disease Outbreaks pDT ----
    disease_outbreaks_main_server(
      "disease_outbreaks_main",
      session_dir,
      i18n
    )
    # Invasive Alien Species pDT ----
    ias_main_server(
      "ias_main",
      i18n
    )
    # Real-time Bird Monitoring pDT ----
    rtbm_main_server(
      "rtbm_main",
      i18n
    )

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
