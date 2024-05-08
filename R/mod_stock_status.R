#' stock_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card_image
mod_stock_status_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Status Summary",
               radioButtons(ns("status_indicator_selector"), "Select status indicator",
                            choices = c("MSY / PA" = "msy", "GES" = "ges")),
               card(imageOutput(ns("status_summary")))),
      tabPanel("Trends-by-groups",
               radioButtons(ns("status_group_selector"), "Select group",
                            choices = c("Elasmobranchs" = "elasmobranch",
                                        "Benthic" = "benthic",
                                        "Crustacean" = "crustacean",
                                        "Demersal" = "demersal",
                                        "Pelagic" = "pelagic")),
               card(imageOutput(ns("status_group"))))
      
    )
 
  )
}
    
#' stock_status Server Functions
#'
#' @noRd 
mod_stock_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$status_summary <- renderImage({
      req(!is.null(input$status_indicator_selector))
      path <- file.path(paste0("inst/app/www/stock_status_", input$status_indicator_selector, ".png"))
      list(src = path)
    }, deleteFile = F)
   
     output$status_group <- renderImage({
      req(!is.null(input$status_group_selector))
      path <- file.path(paste0("inst/app/www/stock_status_", input$status_group_selector, ".png"))
      list(src = path)
    }, deleteFile = F)
  })
}
    
## To be copied in the UI
# mod_stock_status_ui("stock_status_1")
    
## To be copied in the server
# mod_stock_status_server("stock_status_1")
