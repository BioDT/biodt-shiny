#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    
    page_navbar(
      title = "BioDT",
      theme = bs_theme(version = 5,
                       bootswatch = "cosmo"),
      fillable = TRUE,
      nav_menu(
        title = "Species response to environmental change",
        nav_panel(title = "Biodiversity dynamics"),
        nav_panel(
          title = "Ecosystem services",
          mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")
        )
      ),
      nav_menu(
        title = "Genetically detected biodiversity",
        nav_panel(title = "Crop wild relatives and genetic resources for food security"),
        nav_panel(title = "DNA detected biodiversity, poorly known habitats")
      ),
      nav_menu(title = "Dynamics and threats from and for species of policy concern",
               nav_panel(title = "Invasive Species")),
      nav_menu(
        title = "Species interactions with each other and with humans",
        nav_panel(title = "Pollinators",
                  mod_beehave_ui("beehave")),
        nav_panel(title = "Disease Outbreaks")
      ),
      nav_menu(title = "Computations"),
      nav_menu(title = "Info"),
      nav_menu(title = "User",
               icon = icon("user"),
               nav_item(actionButton(
                 "login_button",
                 "Login",
                 width = "100%",
                 icon = icon("arrow-right-to-bracket")
               )),
               nav_item(
                 actionButton("logout_button",
                              "Logout",
                              width = "100%",
                              icon = icon("arrow-right-from-bracket"))
               ))
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
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "BioDTShiny")
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert()
            )
            }
