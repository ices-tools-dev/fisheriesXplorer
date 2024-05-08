#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  mod_landing_page_server("landing_page_1", parent_session = session)
  
  mod_landing_page_server("landing_page_2")
}
