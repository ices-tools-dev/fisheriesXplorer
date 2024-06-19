#' landings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom icesFO plot_discard_trends plot_discard_current plot_catch_trends
mod_landings_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Landings",
        radioButtons(ns("landings_layer_selector"), "View Landings information organised by:",
                     choices = c("Main landed species" = "COMMON_NAME", "Category" = "GUILD", "Country" = "COUNTRY")),
        card(imageOutput(ns("landings_layer")))
      ),
      tabPanel("Discards",
        card(plotOutput(ns("discard_trends"))),
        card(plotOutput(ns("recorded_discards"))),
        card(plotOutput(ns("all_discards")))
        )
    )
  )
}
    
#' landings Server Functions
#'
#' @noRd 
mod_landings_server <- function(id, cap_year, cap_month){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 

    output$landings_layer <- renderPlot({
      req(!is.null(input$landings_layer_selector))
    
      plotting_params <- list()
      plotting_params$landings <- list("COMMON_NAME" = list("n" = 10,
                                                          type = "line"),
                                       "GUILD"= list("n" = 6,
                                                     type = "line"),
                                       "COUNTRY"= list("n" = 9,
                                                     type = "area"))

      params <- plotting_params$landings[[input$landings_layer_selector]]
      plot_catch_trends(formatted_catch_data, type = input$landings_layer_selector, line_count = params$n, plot_type = params$type, official_catches_year = as.numeric(cap_year))
      
    })
    
    year <- 2022
    
    output$discard_trends <- renderPlot({
      plot_discard_trends(catch_trends, year, cap_year , cap_month, caption = F)
    })
    output$recorded_discards <- renderPlot({
      plot_discard_current(catch_trends, year, cap_year , cap_month, position_letter = "")
    })
    output$all_discards <- renderPlot({
      plot_discard_current(catch_trends, year-1, cap_year , cap_month, position_letter = "" )
    })
    
  })
}
    
## To be copied in the UI
# mod_landings_ui("landings_1")
    
## To be copied in the server
# mod_landings_server("landings_1")
