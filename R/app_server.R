#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom stringr str_split
#' @noRd
app_server <- function(input, output, session) {
  
  showModal(modalDialog(
    title = "Important message",
    HTML("Welcome to the development version of the fisheriesXplorer application. <u>The contents are indicative and should not be quoted or used elsewhere</u>.")
  ))
  
  app_date <- str_split(date(), pattern = " ")[[1]]
  cap_year <- app_date[5]
  cap_month <- app_date[2]
  
  selected_ecoregion <- reactiveVal(NULL)
  
  mod_navigation_page_server("navigation_page_1", parent_session = session, selected_ecoregion = selected_ecoregion) 
  mod_overview_server("overview_1", selected_ecoregion = selected_ecoregion)
  mod_landings_server("landings_1", cap_year, cap_month, selected_ecoregion = selected_ecoregion)
  mod_stock_status_server("stock_status_1", cap_year, cap_month, selected_ecoregion = selected_ecoregion)
  mod_mixfish_server("mixfish_1", selected_ecoregion = selected_ecoregion)
  mod_vms_server("vms_1", selected_ecoregion = selected_ecoregion)
  mod_bycatch_server("bycatch_1", selected_ecoregion = selected_ecoregion)
}
