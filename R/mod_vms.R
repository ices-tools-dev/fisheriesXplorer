#' vms UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom glue glue
#' @importFrom ggplot2 ggtitle
#' @importFrom lubridate year
#' @importFrom icesFO plot_effort_map plot_sar_map
#' @importFrom zip zip zipr
#' @importFrom stringr str_starts
mod_vms_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    layout_sidebar(
      bg = "white", fg = "black",
      sidebar = sidebar(
        width = "33vw", bg = "white", fg = "black",
        open = FALSE,
        uiOutput(ns("effort_sar_text")),
        # uiOutput(ns("sar_text"))
      ),
      fluidRow(
        column(
          6,
          card(
            # height = "82vh",
            card_header("Fishing Effort",
                        downloadLink(ns("download_effort_data"),
                                    HTML(paste0("<span class='hovertext' data-hover='Data & Plot image'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                                    )
                        ),
            card_body(
              selectInput(ns("fishing_cat_selector"), "Select fishing gear",
                choices = c("All" = "all", "Beam trawls", "Bottom otter trawls", "Bottom seines", "Dredges", "Pelagic trawls and seines", "Static gears"),
                selected = "All"
              ),
              tags$style(type = "text/css", "#vms_effort_layer {margin-left: auto; margin-right: auto; margin-bottom: auto;  max-width: 97%; height: auto;}"),
              withSpinner(suppressWarnings(uiOutput(ns("vms_effort_layer"), width = "100%", fill = TRUE)))
            )
          )
      ),
      column(
        6,
          card(
            # height = "85vh",
            card_header("Swept Area Ratio",
                        downloadLink(ns("download_sar_data"),
                                     HTML(paste0("<span class='hovertext' data-hover='Data & Plot image'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                        )
            ),
            card_body(
              
              selectInput(ns("sar_layer_selector"), "Select fishing benthic impact layer",
                choices = c("All" = "all", "Surface" = "surface", "Subsurface" = "subsurface"),
                selected = "Surface"
              ),
              tags$style(type = "text/css", "#vms_sar_layer {margin-left: auto; margin-right: auto; margin-bottom: auto;  max-width: 97%; height: auto;}"),
              withSpinner(suppressWarnings(uiOutput(ns("vms_sar_layer"), height = "65vh", width = "100%", fill = TRUE)))
              )
            )
          )
        )
    )
  )
}
#' vms Server Functions
#'
#' @noRd 
mod_vms_server <- function(id, 
    selected_ecoregion,
    bookmark_qs = reactive(NULL)){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ################################## bookmarking #########################################
    # RESTORE once, defer until after first flush, then push up
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("vms")
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "main_tabset", selected = wanted)
          isolate(set_subtab(wanted))
        }, once = TRUE)
      }
    })

    # REPORT on user changes, skip initial default
    observeEvent(input$main_tabset,
      {
        set_subtab(input$main_tabset)
      },
      ignoreInit = TRUE
    )

    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderUI({
      tab <- input$main_tabset
      if (is.null(tab)) tab <- "vms"
      
      date_text <- "November, 2025"

      tagList(
        tags$span(tags$b("Last data update:"), " ", date_text),
        tags$span(" \u00B7 "),
        mod_glossary_float_ui(ns("app_glossary"), link_text = "Glossary", panel_title = "Glossary")
      )
    })
    mod_glossary_float_server(
     "app_glossary",
     terms = reactive({
       df <- select_text(texts, "glossary", NULL) # your texts.rda table
       df[, intersect(names(df), c("term", "definition", "source")), drop = FALSE]
     })
   )

        
    output$vms_effort_layer <- renderUI({
      req(selected_ecoregion, input$fishing_cat_selector)
      render_vms(ecoregion = selected_ecoregion(),
                 gear = input$fishing_cat_selector,
                 vms_layer = "effort",
                 ns = ns)
    })
    
    output$vms_sar_layer <- renderUI({
      req(selected_ecoregion, input$fishing_cat_selector)
      
      render_vms(ecoregion = selected_ecoregion(),
                 gear = input$sar_layer_selector,
                 vms_layer = "sar",
                 ns = ns)
    })
    
    output$download_effort_data <- downloadHandler(
      filename = vms_bundle_filename(selected_ecoregion, vms_layer = "effort"),
      content  = vms_bundle_content(selected_ecoregion, vms_layer = "effort"),
      contentType = "application/zip"
    )
    
    output$download_sar_data <- downloadHandler(
      filename = vms_bundle_filename(selected_ecoregion, vms_layer = "sar"),
      content  = vms_bundle_content(selected_ecoregion, vms_layer = "sar"),
      contentType = "application/zip"
    )
    
    # output$effort_text <- renderUI({
    #   HTML(select_text(texts, "vms", "effort_sidebar"))
    # })
    
    # output$sar_text <- renderUI({
    #   HTML(select_text(texts, "status", "sar_sidebar"))
    # })
    output$effort_sar_text <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("vms_", get_ecoregion_acronym(selected_ecoregion())), "effort_SAR"))    
      )
    })
  })
}
    
## To be copied in the UI
# mod_vms_ui("vms_1")
    
## To be copied in the server
# mod_vms_server("vms_1")

