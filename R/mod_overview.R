#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "hidden", 
                id=ns("overview"),
                tabPanel("Map tab", value=ns("tab_map"),
                         card(imageOutput(ns("map"))),
                         actionButton("Next", ns("map2text"))
                         ),
                tabPanel("Text tab", value = ns("tab_topic"))
                
    )
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$map <- renderImage({
      path <- file.path("inst/app/www/ecoregion.png")
    list(src = path)
    }, deleteFile = F)
  
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
