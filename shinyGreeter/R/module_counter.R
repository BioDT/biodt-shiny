#' counter UI
#' 
#' @param id Unique id for module instance.
#' 
#' @keywords internal
counterUI <- function(id){
	ns <- NS(id)

	tagList(
		h2("counter")
	)
}

#' counter Server
#' 
#' @param id Unique id for module instance.
#' 
#' @keywords internal
counter_server <- function(id){
	moduleServer(
		id,
		function(
			input, 
			output, 
			session
			){
				
				ns <- session$ns
				send_message <- make_send_message(session)

				# your code here
		}
	)
}

# UI
# counterUI('id')

# server
# counter_server('id')
