#' landings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_landings_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    radioButtons(ns("landings_layer_selector"), "Select water column level where fishing occurs",
                 choices = c("Main" = "main", "Category" = "category", "Country" = "country", "Gear" = "gear")),
    card(imageOutput(ns("landings_layer")))
  )
}
    
#' landings Server Functions
#'
#' @noRd 
mod_landings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$landings_layer <- renderImage({
      req(!is.null(input$landings_layer_selector))
      path <- file.path(paste0("inst/app/www/landings_", input$landings_layer_selector, ".png"))
      list(src = path)
    }, deleteFile = F)
    
  })
}
    
## To be copied in the UI
# mod_landings_ui("landings_1")
    
## To be copied in the server
# mod_landings_server("landings_1")
