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
    br(),
    layout_sidebar(
      bg = "white", fg = "black",
      sidebar = sidebar(
        width = "33vw", bg = "white", fg = "black",
        open = FALSE,
        uiOutput(ns("sar_text"))
      ),
      fluidRow(
        column(
          6,
          card(
            height = "85vh",
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
              withSpinner(suppressWarnings(uiOutput(ns("vms_effort_layer"), height = "65vh", width = "100%", fill = T)))
            )
          )
      ),
      column(
        6,
          card(
            height = "85vh",
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
              suppressWarnings(withSpinner(suppressWarnings(uiOutput(ns("vms_sar_layer"), height = "65vh", width = "100%", fill = T))))
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
mod_vms_server <- function(id, selected_ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      "Last update: December 05, 2024" # e.g., "May 26, 2025"
    })
    
  
    
    output$vms_effort_layer <- renderUI({
      req(selected_ecoregion, input$fishing_cat_selector)
      render_vms(ecoregion = selected_ecoregion(),
                 gear = input$fishing_cat_selector,
                 what = "effort",
                 ns = ns)
    })
    
    output$vms_sar_layer <- renderUI({
      req(selected_ecoregion, input$fishing_cat_selector)
      
      render_vms(ecoregion = selected_ecoregion(),
                 gear = input$sar_layer_selector,
                 what = "sar",
                 ns = ns)
    })
    
    output$download_effort_data <- downloadHandler(
      filename = vms_bundle_filename(selected_ecoregion, what = "effort"),
      content  = vms_bundle_content(selected_ecoregion, what = "effort"),
      contentType = "application/zip"
    )
    
    output$download_sar_data <- downloadHandler(
      filename = vms_bundle_filename(selected_ecoregion, what = "sar"),
      content  = vms_bundle_content(selected_ecoregion, what = "sar"),
      contentType = "application/zip"
    )
  })
}
    
## To be copied in the UI
# mod_vms_ui("vms_1")
    
## To be copied in the server
# mod_vms_server("vms_1")

