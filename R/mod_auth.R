#' login UI module
#'
#' Shiny UI Module for use with \link{mod_login_server}
#' This is taken from the shinyauthr package authored by Paul Campbell, \email{pacampbell91@gmail.com} and found at \url{https://github.com/PaulC91/shinyauthr}.
#' Mainly the login logic was adjusted for the needs of this project.
#'
#' Call via \code{mod_login_ui("your_id")}
#'
#' @param id Shiny id
#' @param title header title for the login panel
#' @param user_title label for the user name text input
#' @param pass_title label for the password text input
#' @param login_title label for the login button
#' @param error_message message to display after failed login
#' @param additional_ui additional shiny UI element to add below login button. Wrap multiple inside \code{shiny::tagList()}
#'
#' @return Shiny UI
#'
#' @author Tomas Martinovic \email{tomas.martinovic@vsb.cz}
#'
mod_login_ui <- function(id,
                         title = "Login please",
                         user_title = "Username",
                         pass_title = "Password",
                         login_title = "Login",
                         additional_ui = NULL) {
  ns <- shiny::NS(id)
  
  shiny::div(
    id = ns("panel"),
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    shiny::wellPanel(
      shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
      
      shiny::textInput(
        ns("username"),
        shiny::tagList(shiny::icon("user"),
                       user_title),
        value = ""
      ),
      
      shiny::passwordInput(
        ns("password"),
        shiny::tagList(shiny::icon("unlock-alt"), pass_title),
        value = ""
      ),
      
      shiny::div(style = "text-align: center;",
                 actionButton(ns("button"),
                              login_title),
                 class = "primary"),
      
      additional_ui,
      
      uiOutput(ns("error_message"))
    )
  )
}

#' login server module
#'
#' Shiny authentication module for use with \link{mod_login_ui}
#'
#' Call via \code{shiny::callModule("your_id", ...)}
#'
#' @param id session id
#'
#' @return The module will return a reactive 2 element list to your main application.
#'   First element \code{user_auth} is a boolean indicating whether there has been
#'   a successful login or not. Second element \code{info} will be the data frame provided
#'   to the function, filtered to the row matching the successfully logged in username.
#'   When \code{user_auth} is FALSE \code{info} is NULL.
#'
#' @importFrom shiny observeEvent
#'
#' @author Tomas Martinovic \email{tomas.martinovic@vsb.cz}
mod_login_server <- function(id,
                             r) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 r_login <- reactiveValues(error_msg = NULL)
                 
                 shiny::observeEvent(input$button,
                                     priority = 100,
                                     {
                                       golem::print_dev("Running authentication.")
                                       lexis_client <- httr2::oauth_client(
                                         id = "LEXIS_R4LEXIS",
                                         secret = httr2::obfuscated(
                                           "sis6S3epq7IKIG6g9kbg-jVfNlJVBk1f5X66fWyQU-SPiZiqSNOKKR2tbhmzs69u"
                                         ),
                                         token_url = "https://aai.lexis.tech/auth/realms/LEXIS_AAI/protocol/openid-connect/token"
                                       )
                                       
                                       tryCatch({
                                         r$lexis_token <- httr2::oauth_flow_password(client = lexis_client,
                                                                                     username = input$username,
                                                                                     password = input$password,
                                                                                     scope = "openid")
                                       },
                                       error = function(e) {r_login$error_msg <- shiny::tags$p("Authentication error!",
                                                                                               style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center")
                                        print(e)})
                                       
                                       if (!is.null(r$lexis_token)) {
                                         r_login$error_msg <- shiny::tags$p("Login Successful! You can return to work on other pages.",
                                                                            style = "color: green; font-weight: bold; padding-top: 5px;", class = "text-center")
                                         r$show_login <- FALSE
                                       }
                                     })
                 #r_login$error_msg <- 
                 output$error_message <- renderUI({
                   r_login$error_msg
                   
                 })
                 
               })
}
