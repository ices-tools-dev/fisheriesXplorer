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
mod_vms_ui <- function(id, sub_tabs) {
  ns <- NS(id)
  tagList(
    card(
      "Fishing Effort",
      card(
        withSpinner(
          imageOutput(
            ns("effort_layer")
          )
        )
      )
    ),
    card(
      "Fishing Benthic Impact",
      radioButtons(ns("sar_layer_selector"), "Select fishing benthic impact level",
        choices = c("Surface" = "surface", "Subsurface" = "subsurface")
      ),
      card(
        withSpinner(
          imageOutput(
            ns("sar_layer")
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
 
    output$effort_layer <- renderPlot({
     
       plot_effort_map(effort_maps[["Greater North Sea"]], ecoregion[["Greater North Sea"]])+
        ggtitle(paste0("Average MW Fishing hours ", paste(year(Sys.Date())-4, year(Sys.Date()), sep = "-")))
     
      })
    
    output$sar_layer <- renderPlot({
      req(!is.null(input$sar_layer_selector))
     
        plot_sar_map(sar_maps[["Greater North Sea"]], ecoregion[["Greater North Sea"]], what = input$sar_layer_selector) +
          ggtitle(glue("Average {input$sar_layer_selector} swept area ratio ", paste(year(Sys.Date())-4, year(Sys.Date()), sep = "-")))
     
      })
    
    
  })
}
    
## To be copied in the UI
# mod_vms_ui("vms_1")
    
## To be copied in the server
# mod_vms_server("vms_1")
