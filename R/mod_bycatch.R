#' bycatch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bycatch_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("bycatch_taxa_selector"), "Select bycatch of seabirds and marine mammals",
                 choices = c("Seabirds" = "seabirds", "Marine mammals" = "mammals")),
    card(imageOutput(ns("bycatch_layer")))
  )
}
    
#' bycatch Server Functions
#'
#' @noRd 
mod_bycatch_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$bycatch_layer <- renderImage({
      req(!is.null(input$bycatch_taxa_selector))
      path <- file.path(paste0("inst/app/www/bycatch_", input$bycatch_taxa_selector, ".png"))
      list(src = path)
    }, deleteFile = F)
  })
}
    
## To be copied in the UI
# mod_bycatch_ui("bycatch_1")
    
## To be copied in the server
# mod_bycatch_server("bycatch_1")
