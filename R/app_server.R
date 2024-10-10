#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom stringr str_split
#' @noRd
app_server <- function(input, output, session) {
  
  # Show welcome modal
  showModal(modalDialog(
    title = "Important message",
    HTML("Welcome to the development version of the fisheriesXplorer application. <u>The contents are indicative and should not be quoted or used elsewhere</u>.")
  ))
  
  # Extract current date information
  app_date <- str_split(date(), pattern = " ")[[1]]
  cap_year <- app_date[5]
  cap_month <- app_date[2]
  
  # Reactive value to store selected ecoregion
  selected_ecoregion <- reactiveVal(NULL)
  

  # Initialize Modules
  mod_navigation_page_server("navigation_page_1", parent_session = session, selected_ecoregion = selected_ecoregion)
  mod_overview_server("overview_1", selected_ecoregion)
  mod_landings_server("landings_1", cap_year, cap_month, selected_ecoregion)
  mod_stock_status_server("stock_status_1", cap_year, cap_month, selected_ecoregion)
  mod_mixfish_server("mixfish_1", selected_ecoregion)
  mod_vms_server("vms_1", selected_ecoregion)
  mod_bycatch_server("bycatch_1", selected_ecoregion)
  
  # Observer to manage dynamic navbar tabs
  observeEvent(selected_ecoregion(), {
    req(selected_ecoregion())
    
    # Define all possible dynamic tabs
    all_tabs <- c("Overview", "Landings", "Stock Status", "Mixed Fisheries", "VMS", "Bycatch")
    
    # Remove existing dynamic tabs if any
    lapply(all_tabs, function(tab_name) {
      removeTab(inputId = "main-navbar", target = tab_name)
    })
    
    # Fetch the configuration for the selected ecoregion
    ecoregion_config <- config$ecoregions[[selected_ecoregion()]]$tabs
    
    if (!is.null(ecoregion_config)) {
      # Iterate through each tab in the config and insert it
      for (tab_key in names(ecoregion_config)) {
        tab_name <- tools::toTitleCase(gsub("_", " ", tab_key))
        
        # Ensure tab_name is in the list of all_tabs
        if (tab_name %in% all_tabs) {
          # Generate the UI using the corresponding module
          tab_ui <- switch(tab_key,
            "overview" = mod_overview_ui("overview_1", ecoregion_config[[tab_key]]$sub_tabs),
            "landings" = mod_landings_ui("landings_1", ecoregion_config[[tab_key]]$sub_tabs),
            "stock_status" = mod_stock_status_ui("stock_status_1", ecoregion_config[[tab_key]]$sub_tabs),
            "mixed_fisheries" = mod_mixfish_ui("mixfish_1"),
            "vms" = mod_vms_ui("vms_1", ecoregion_config[[tab_key]]$sub_tabs),
            "bycatch" = mod_bycatch_ui("bycatch_1", ecoregion_config[[tab_key]]$sub_tabs)
          )
          
          if (!is.null(tab_ui)) {
            # Insert the tab after the "Home" tab
            appendTab(
              inputId = "main-navbar",
              tab = tabPanel(tab_name, tab_ui),
              select = FALSE
            )
          }
        }
      }
    }
  }, ignoreNULL = FALSE)
}