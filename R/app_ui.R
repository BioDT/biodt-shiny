#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom waiter autoWaiter
#' @importFrom htmltools css
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    
    # Main (page) navbar----
    bslib::page_navbar(
      window_title = "BioDT",
      title = div(
        shiny::a(
          href = "https://www.biodt.eu",
          img(src = "www/logo.svg",
            height = "80px",
            style = "padding-right: 20px"
          ),
        )
      ),
      id = "navbar",
      theme = biodt_theme,
      bg = "#fff",
      fillable = TRUE, # must be true
      collapsible = TRUE,
      fluid = TRUE,
      
      ## Info - main menu item ----
      
        
      nav_panel(title = "Info",
                value = "info",
                icon = shiny::icon("circle-info"),
                class = "container-fluid info",
                mod_info_ui("info")),
      
      ## Digital Twins - main menu item ----
      nav_menu(
        title = "Digital Twins",
        align = "left",
        icon = shiny::icon("people-group"),
        #class = "nav_menu_dts",
        
        ### Species response to environment - menu subitem ----
        nav_item(

          div(
            class = "p-2",
            icon("temperature-arrow-up"),
            strong(
              "Species response to environmental change"
            ),
          )
        ),
        nav_panel(title = "Biodiversity dynamics"),
        nav_panel(title = "Grassland dynamics",
                  mod_grassland_ui("grassland")),
        nav_panel(
          title = "Ecosystem services",
          mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")
        ),

        ### Biodiversity - menu subitem ----
        nav_item(
          div(
            div(
              class = "p-2",
              hr(),
              icon("wheat-awn-circle-exclamation"),
              strong(
                "Genetically detected biodiversity"
              ),
            )
          )
        ),
        nav_panel(
          title = "Crop wild relatives and genetic resources for food security",
          mod_cwr_ui("cwr")),
        nav_panel(title = "DNA detected biodiversity, poorly known habitats"),

        ### Species of policy concern - menu subitem ----
        nav_item(
          div(
            class = "p-2",
            hr(),
            icon("bowl-food"),
            strong(
              "Threats from and for species of policy concern"
            ),
          )
        ),
        nav_panel(title = "Invasive Species",
                   class = "p-0",
                   mod_ias_ui("ias")),
        
        ### Species interactions (themselves, human) - menu subitem ----
        nav_item(
          div(
            class = "p-2",
            hr(),
            icon("bugs"),
            strong(
              "Species interactions with each other and with humans"
            ),
          )
        ),
        nav_panel(title = "Pollinators",
                  class = "p-0",
                  mod_beehave_ui("beehave")),
        nav_panel(title = "Disease Outbreaks")
    
      ),
      bslib::nav_spacer(),
      
      
      ## User (+ computations) - main menu item ----
      nav_menu(
        title = "User",
        align = "right",
        icon = icon("user"),
        nav_panel(
          title = "Computations",
          icon = shiny::icon("microchip"),
          mod_computations_ui("computations")),
        bslib::nav_spacer(),
        nav_panel(
          title = "Login",
          value = "nav_login",
          icon = shiny::icon("arrow-right-to-bracket"),
          mod_login_ui("login_pass")),
        bslib::nav_spacer(),
      )

      # nav_menu(
      #   title = "User",
      #   icon = icon("user"),
        # nav_item(
        #   actionButton(
        #     "login_button",
        #     "Login",
        #     width = "100%",
        #     icon = icon("arrow-right-to-bracket"),
        #     class = "btn-navbar"
        #   )
        # ),
        # shinyjs::hidden(
        #   nav_item(
        #     actionButton(
        #       "logout_button",
        #       "Logout",
        #       width = "100%",
        #       icon = icon("arrow-right-from-bracket"),
        #       class = "btn-navbar"
        #     )
        #   )
        # )
      # )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter useWaiter useHostess
#' @importFrom cicerone use_cicerone
#' @import bslib
#' @noRd

golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))
  
  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"),
                     app_title = "BioDTShiny"),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    waiter::useHostess(),
    cicerone::use_cicerone(),
    
  )
  
}


# App theme ----
biodt_theme <- bslib::bs_theme(
  primary = "#bc6c25",
  secondary = "#414f2f",
  bg = "#fff",
  fg = "#414f2f",
  version = 5,
  bootswatch = "bootstrap"
)
