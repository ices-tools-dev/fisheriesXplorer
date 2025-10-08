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
  
  # shared container
  shared <- reactiveValues(
    SID = NULL,
    SAG = NULL,
    clean_status = NULL
  )
  # wrapper reactive: runs all webservice calls at once
  fetchData <- reactive({
    withProgress(message = paste0("Fetching data for ", selected_ecoregion(), "..."), value = 0, {

      incProgress(0.2, detail = "Getting SID...")
      sid <- tryCatch(
        getSID(
          year = as.integer(format(Sys.Date(), "%Y")),
          EcoR = selected_ecoregion()
        ),
        error = function(e) {
          paste("Error fetching SID:", e$message)
        }
      )

      incProgress(0.5, detail = "Getting SAG...")
      sag <- tryCatch(
        getSAG_ecoregion_new(selected_ecoregion()),
        error = function(e) {
          paste("Error fetching SAG:", e$message)
        }
      )

      incProgress(0.9, detail = "Getting SAG status...")
      status <- tryCatch(
        format_sag_status_new(
          getStatusWebService(selected_ecoregion(), sid), sag
        ),
        error = function(e) {
          paste("Error fetching SAG status:", e$message)
        }
      )

      list(SID = sid, SAG = sag, clean_status = status)
    })
  }) %>% bindCache(selected_ecoregion())   # cache by ecoregion
  
  # update shared values whenever fetchData runs
  observe({
    data <- fetchData()
    shared$SID <- data$SID
    shared$SAG <- data$SAG
    shared$clean_status <- data$clean_status
  })
  
  
  # mod_navigation_page_server("navigation_page_1", parent_session = session, selected_ecoregion = selected_ecoregion) 
  mod_overview_server("overview_1", selected_ecoregion = selected_ecoregion)
  mod_landings_server("landings_1", cap_year, cap_month, selected_ecoregion = selected_ecoregion, shared = shared)
  mod_stock_status_server("stock_status_1", cap_year, cap_month, selected_ecoregion = selected_ecoregion, shared = shared)

  
  sel_mixfish <- mod_mixfish_plot_selection_server(
  "mixfish_selection_1",
  selected_ecoregion = selected_ecoregion
)

mod_mixfish_plot_display_server(
  "mixfish_viz_1",
  plot_name          = sel_mixfish$plot_choice,
  selected_ecoregion = selected_ecoregion,
  selected_subRegion = sel_mixfish$sub_region
)

  mod_vms_server("vms_1", selected_ecoregion = selected_ecoregion)
  mod_bycatch_server("bycatch_1", selected_ecoregion = selected_ecoregion)
}
