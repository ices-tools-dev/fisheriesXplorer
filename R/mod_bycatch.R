#' bycatch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bycatch_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(bg = "white", fg = "black", 
      sidebar = sidebar(width = "33vw", bg = "white", fg = "black", 
                        open = F,
                        uiOutput(ns("bycatch_text"))),
      card(height = "85vh",
        card_header(
          div(style = "margin-left: 12px;",
              radioButtons(ns("bycatch_taxa_selector"), "View bycatch of seabirds or marine mammals:",
                           choices = c("Seabirds" = "seabirds", "Marine mammals" = "mammals"),
                           width = "80%", inline = T)
          )
        ),
        card_body(fillable = T,
                  withSpinner(plotOutput(ns("bycatch_layer"), height = "72vh"))
        )
      )
    )
  )
}
    
#' bycatch Server Functions
#'
#' @noRd 
mod_bycatch_server <- function(id, selected_ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })
    
    output$bycatch_text <- renderUI({
      HTML(select_text(texts,"bycatch","sidebar"))
    })
    
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
