#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      type = "hidden",
      id = ns("overview"),
      layout_sidebar(
        sidebar = sidebar(
            width = "50%",
            card(br(),
            tags$style(type = "text/css", "#staticMap {margin-left: auto; margin-right: auto; margin-bottom: auto;  max-width: 97%; height: auto;}"),
              withSpinner(imageOutput(ns("staticMap"), width = "95vh", height = "80vh"))#,fill = T)
          )),
        card(min_height = "80vh",
          tabsetPanel(
            tabPanel("Executive Summary",
              card(uiOutput(ns("executive_summary")))
            ),
            tabPanel("Introduction",
              card(uiOutput(ns("introduction")))
            ),
            tabPanel("Who is Fishing", 
              card(uiOutput(ns("who_is_fishing")))
            )
          )
        )
      )
    )
  )
}
       
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$staticMap <- renderImage({
      path <- file.path("inst/app/www/ecoregion.png")
    list(src = path,
        width = "auto",
        height = "auto")
    }, deleteFile = F)
  
    
    output$executive_summary <- renderUI({
      HTML(select_text(texts,"overview","executive_summary"))
    })
    output$introduction <- renderUI({
      HTML(select_text(texts,"overview","introduction"))
    })
    output$who_is_fishing <- renderUI({
      HTML(select_text(texts,"overview","who_is_fishing"))
    })
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
