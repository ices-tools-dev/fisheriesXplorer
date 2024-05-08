#' vms UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vms_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("vms_layer_selector"), "Select water column level where fishing occurs",
                 choices = c("Surface" = "surface", "Bottom" = "bottom")),
    card(imageOutput(ns("vms_layer")))
  )
}
    
#' vms Server Functions
#'
#' @noRd 
mod_vms_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$vms_layer <- renderImage({
      req(!is.null(input$vms_layer_selector))
      path <- file.path(paste0("inst/app/www/sar_", input$vms_layer_selector, ".png"))
      list(src = path)
    }, deleteFile = F)
    
  })
}
    
## To be copied in the UI
# mod_vms_ui("vms_1")
    
## To be copied in the server
# mod_vms_server("vms_1")
