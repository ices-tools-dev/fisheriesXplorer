#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  app_date <- stringr::str_split(date(), pattern = " ")[[1]]
  cap_year <- app_date[5]
  cap_month <- app_date[2]
  
  
  mod_navigation_page_server("navigation_page_1", parent_session = session)
  mod_overview_server("overview_1")
  mod_landings_server("landings_1")
  mod_stock_status_server("stock_status_1", cap_year, cap_month)
  mod_vms_server("vms_1")
  mod_bycatch_server("bycatch_1")
}
