#' Shiny UI
#' 
#' Core UI of package.
#' 
#' @param req The request object.
#' 
#' @import shiny
#' @importFrom bslib bs_theme
#' 
#' @keywords internal
ui <- function(req){
	fluidPage(
		theme = bs_theme(version = 5),
		header = list(assets()),
		title = "shinyGreeter",
		id = "main-menu",
		tabPanel(
			"First tab",
			shiny::h1("First tab")
		),
		tabPanel(
			"Second tab",
			shiny::h1("Second tab"),
      textInput("name", "What is your name?"),
      actionButton("greet", "Greet"),
      textOutput("greeting")
		)
	)
}

#' Assets
#' 
#' Includes all assets.
#' This is a convenience function that wraps
#' [serveAssets] and allows easily adding additional
#' remote dependencies (e.g.: CDN) should there be any.
#' 
#' @importFrom shiny tags
#' 
#' @keywords internal
assets <- function(){
	list(
		serveAssets(), # base assets (assets.R)
		tags$head(
			# Place any additional depdendencies here
			# e.g.: CDN
		)	
	)
}
