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
    
    # main (page) navbar----
    bslib::page_navbar(
      window_title = "BioDT",
      title = div(
        shiny::a(
          href = "https://www.biodt.eu",
          img(src = "www/logo.svg",
            height = "80px",
            style = "padding-right: 20px"
          ),
          #width = "500px" # TODO
        )
      ),
      id = "navbar",
      theme = biodt_theme,
      bg = "#fff",
      fillable = TRUE,
      collapsible = TRUE,
      nav_panel(title = "Info",
                value = "info",
                class = "container-fluid info",
                mod_info_ui("info")),
      nav_menu(
        title = "Species response to environmental change",
        nav_panel(title = "Biodiversity dynamics"),
        nav_panel(title = "Grassland dynamics",
                  mod_grassland_ui("grassland")),
        nav_panel(
          title = "Ecosystem services",
          mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")
        )
      ),
      nav_menu(
        title = "Genetically detected biodiversity",
        nav_panel(title = "Crop wild relatives and genetic resources for food security",
                  mod_cwr_ui("cwr")),
        nav_panel(title = "DNA detected biodiversity, poorly known habitats")
      ),
      nav_menu(title = "Dynamics and threats from and for species of policy concern",
               nav_panel(title = "Invasive Species",
                         class = "p-0",
                         mod_ias_ui("ias"))),
      nav_menu(
        title = "Species interactions with each other and with humans",
        nav_panel(title = "Pollinators",
                  class = "p-0",
                  mod_beehave_ui("beehave")),
        nav_panel(title = "Disease Outbreaks")
      ),
      bslib::nav_item(
        div(
          id = "DTnavContainer",
          class = "card shadow",
          style = css(
            # z_index = 1059,
            # position = "fixed",
            # top = "1rem", 
            # right = "1rem",
            width = "18rem", 
            max_height = "80vh",
            height = "auto"
          ),
          div(
            id = "DTnavHeader",
            class = "move-grabber",
            "data-target" = "#DTnavContainer",
            class = "card-header font-weight-bold bg-secondary px-3 py-2", # TODO text-light
            "Digital Twins",
            tags$div(
              id = "DTnavToggle", class = "float-left",
               "data-toggle" = "collapse",
               "data-target" = "#DTnavAccordion",
               "data-bs-toggle" = "collapse",
               "data-bs-target" = "#DTnavAccordion",
               style = css(cursor = "pointer"),
               tags$span(),
               # bs_dependency_defer(themer_css_dependency) # TODO
            )
          ),
          div(
            id = "DTnavAccordion", 
            class = "collapse show accordion",
            # class = if (version >= 5) "accordion", # TODO
            style = css(overflow_y = "auto"),
            
            # accordion item - Species response----
            div(
              class = "accordion-item",
              div(
                class = "accordion-header",
                tags$button(
                  class = "accordion-button",
                  class = "collapsed", # if (i != 1) "collapsed", # TODO
                  "data-toggle" = "collapse",
                  "data-target" = "#speciesResponse",
                  "data-bs-toggle" = "collapse",
                  "data-bs-target" = "#speciesResponse",
                  "Species response to environmental change"
                  # opt_name,     # TODO opt_name <- names(opts)[[i]]
                )
              ),
              div(
                id = "speciesResponse", 
                class = "show", # if (i == 1) "show" else "collapse", # TODO show/collapse logic ----
                "data-parent" = "#DTnavAccordion",
                "data-bs-parent" = "#DTnavAccordion",
                class = "accordion-collapse",
                div(
                  class = "accordion-body",
                  # controls <- lapply(seq_along(opts[[i]]), function(j) {
                  #   make_control(names(opts[[i]])[[j]], opts[[i]][[j]])
                  # })
                  
                )
              )
            ),
            
            # accordion item - biodiversity----
            div(
              class = "accordion-item",
              div(
                class = "accordion-header",
                tags$button(
                  class = "accordion-button",
                  class = "collapsed", # if (i != 1) "collapsed", # TODO
                  "data-toggle" = "collapse",
                  "data-target" = "#biodiversity",
                  "data-bs-toggle" = "collapse",
                  "data-bs-target" = "#biodiversity",
                  "Genetically detected biodiversity"
                  # opt_name,     # TODO opt_name <- names(opts)[[i]]
                )
              ),
              div(
                id = "biodiversity", 
                class = "collapse", # if (i == 1) "show" else "collapse", # TODO show/collapse logic ----
                "data-parent" = "#DTnavAccordion",
                "data-bs-parent" = "#DTnavAccordion",
                class = "accordion-collapse",
                div(
                  class = "accordion-body",
                  # controls <- lapply(seq_along(opts[[i]]), function(j) {
                  #   make_control(names(opts[[i]])[[j]], opts[[i]][[j]])
                  # })
                )
              )
            ),
            
            # accordion item - Species policy concern----
            div(
              class = "accordion-item",
              div(
                class = "accordion-header",
                tags$button(
                  class = "accordion-button",
                  class = "collapsed", # if (i != 1) "collapsed", # TODO
                  "data-toggle" = "collapse",
                  "data-target" = "#speciesPolicyConcern",
                  "data-bs-toggle" = "collapse",
                  "data-bs-target" = "#speciesPolicyConcern",
                  "Dynamics and threats from and for species of policy concern"
                  # opt_name,     # TODO opt_name <- names(opts)[[i]]
                )
              ),
              div(
                id = "speciesPolicyConcern", 
                class = "collapse", # if (i == 1) "show" else "collapse", # TODO show/collapse logic ----
                "data-parent" = "#DTnavAccordion",
                "data-bs-parent" = "#DTnavAccordion",
                class = "accordion-collapse",
                div(
                  class = "accordion-body",
                  # controls <- lapply(seq_along(opts[[i]]), function(j) {
                  #   make_control(names(opts[[i]])[[j]], opts[[i]][[j]])
                  # })
                )
              )
            ),
            
            
            
          )
        )
      ),
      bslib::nav_spacer(),
      nav_panel(title = "Computations",
                mod_computations_ui("computations")),
      nav_panel(title = "Login",
                value = "nav_login",
                icon = icon("arrow-right-to-bracket"),
                mod_login_ui("login_pass"))
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
