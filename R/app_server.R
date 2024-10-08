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
  
    title_html <- tags$a(
    href = "https://ices-tools-dev.shinyapps.io/fisheriesXplorer/",
    tags$img(
      src = "www/negative_ices_logo.png",
      style = "margin-top: -15px; margin-bottom: 0px; padding-right:10px;",
      height = "50px"
    )
  )

  app_date <- str_split(date(), pattern = " ")[[1]]
  cap_year <- app_date[5]
  cap_month <- app_date[2]
  

  selected_ecoregion <- reactiveVal(NULL)

  print("App server function started")
  print(paste("Config loaded:", !is.null(config)))
   observe({
    print(paste("selected_ecoregion value:", selected_ecoregion()))
  })


  
  output$debug_output <- renderText({
    paste("Selected ecoregion:", selected_ecoregion())
  })

  navbar_data <- reactive({
    input$triggerNavbarRender  # This ensures the reactive recalculates when triggerNavbarRender changes
    
    if (is.null(selected_ecoregion())) {
      return(NULL)
    }
    
    if (!is.null(ecoregion_config)) {
  print(paste("Available tabs:", paste(names(ecoregion_config), collapse = ", ")))
  
  for (tab_name in names(ecoregion_config)) {
    tab_config <- ecoregion_config[[tab_name]]
    if (!is.null(tab_config)) {
      tab_ui <- switch(tab_name,
        "overview" = mod_overview_ui("overview_1", tab_config$sub_tabs),
        "landings" = mod_landings_ui("landings_1", tab_config$sub_tabs),
        "stock_status" = mod_stock_status_ui("stock_status_1", tab_config$sub_tabs),
        "mixed_fisheries" = mod_mixfish_ui("mixfish_1"),
        "vms" = mod_vms_ui("vms_1", tab_config$sub_tabs),
        "bycatch" = mod_bycatch_ui("bycatch_1", tab_config$sub_tabs)
      )
      if (!is.null(tab_ui)) {
        navbar_pages <- c(navbar_pages, list(tabPanel(tools::toTitleCase(gsub("_", " ", tab_name)), tab_ui)))
      }
    }
  }
}
  })

  output$dynamic_navbar <- renderUI({
    pages <- navbar_data()
    if (is.null(pages)) {
      return(NULL)
    }
    
    do.call(navbarPage, c(
      list(
        title = title_html,
        position = "static-top",
        collapsible = TRUE,
        fluid = TRUE,
        windowTitle = "fisheriesXplorer",
        id = "nav-page"
      ),
      pages
    ))
  })
  
  mod_navigation_page_server("navigation_page_1", parent_session = session, selected_ecoregion = selected_ecoregion)
  mod_overview_server("overview_1", selected_ecoregion)
  mod_landings_server("landings_1", cap_year, cap_month, selected_ecoregion)
  mod_stock_status_server("stock_status_1", cap_year, cap_month, selected_ecoregion)
  mod_mixfish_server("mixfish_1", selected_ecoregion)
  mod_vms_server("vms_1", selected_ecoregion)
  mod_bycatch_server("bycatch_1", selected_ecoregion)
  
  observeEvent(input$triggerNavbarRender, {
    session$sendCustomMessage("triggerNavbarRender", list())
  })
}
  
