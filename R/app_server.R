#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  mod_landing_page_server("landing_page_1", parent_session = session)
  
  mod_stock_status_server("stock_status_1")
  mod_vms_server("vms_1")
  mod_bycatch_server("bycatch_1")
}
