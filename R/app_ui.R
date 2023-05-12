#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

      navbarPage("App Title",
                 navbarMenu(""),
                 navbarMenu("Species response to environmental change",
                            tabPanel("Biodiversity dynamics"),
                            tabPanel("Ecosystem services")),
                 navbarMenu("Genetically detected biodiversity",
                            tabPanel("Crop wild relatives and genetic resources for food security"),
                            tabPanel("DNA detected biodiversity, poorly known habitats"),
                 navbarMenu("Dynamics and threats from and for species of policy concern",
                            tabPanel("Endangered species"),
                            tabPanel("Invasive Species")),
                 navbarMenu("Species interactions with each other and with humans",
                            tabPanel("Pollinators"),
                            tabPanel("Disease Outbreaks"))
                 )
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
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BioDTShiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
