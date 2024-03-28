box::use(
  shiny,
  bslib[bs_theme],
  shiny.router[router_ui, router_server, route, route_link],
  view/info[mod_info_ui],
)


# App theme ----
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

# Content ----
link1 <-
  shiny$tags$div(shiny$tags$h3("Release"))
link2 <- shiny$tags$div(
  style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
  shiny$tags$h1(
    shiny$tags$a("Check out Rhino docs!", href = "https://appsilon.github.io/rhino/")
  )
)


#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$bootstrapPage(
    theme = biodt_theme,
    shiny$tags$nav(
      class = "navbar navbar-expand-lg",
      shiny$tags$div(
        class = "container-fluid",
        shiny$tags$a(
          class = "navbar-brand",
          href = route_link("/"),
          shiny$tags$img(
            src = "static/logo.svg",
            style = "height: 60px;"
          )
        ),
        shiny$tags$div(
          class = "collapse navbar-collapse",
          id = "digitaltwins",
          shiny$tags$ul(
            class = "navbar-nav me-auto mb-2 mb-lg-0",
            shiny$tags$li(
              class = "nav-item",
              shiny$tags$a(
                class = "nav-link", href = route_link("/"), "Info"
              )
            ),
            shiny$tags$li(
              class = "nav-item",
              shiny$tags$a(
                class = "nav-link dropdown-toggle",
                href = "#",
                role = "button",
                `data-bs-toggle` = "dropdown",
                `aria-expanded` = "false",
                "Digital Twins"
              ),
              shiny$tags$ul(
                class = "dropdown-menu",
                shiny$div(
                  class = "p-2",
                  # hr(),
                  shiny$div(
                    shiny$icon("bugs", class = "p-1"),
                    shiny$tags$strong("Species interactions with each other and with humans"),
                    style = "width: 500px"
                  ),
                ),
                shiny$tags$li(
                  shiny$tags$a(
                    class = "dropdown-item",
                    href = route_link("page"),
                    "Honeybee"
                  )
                )
              )
            )
          )
        )
      )
    ),
    router_ui(
      route("/", mod_info_ui(ns("info"))),
      route("page", link2)
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    router_server()
  })
}
