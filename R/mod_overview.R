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
      # card(
      # card_body(
      card(
        min_height = "40vw",
        # full_screen = TRUE,
        # card_header("A sidebar layout inside a card"),
        layout_sidebar(
          # fillable = TRUE,
          # height = 600,
          sidebar = sidebar(
            width = "50vw",
            
            # height = "100vw",
            br(),
            withSpinner(imageOutput(ns("map"), width = "95%", height = "auto"))
          ),
          tabsetPanel(
            tabPanel(
              "Executive Summary",
              textOutput(ns("executive_summary"))
            ),
            tabPanel(
              "Introduction",
              textOutput(ns("introduction"))
            ),
            tabPanel(
              "Who is Fishing",
              textOutput(ns("who_is_fishing"))
            )
          )
        )
      )
    )
  )
}
          # layout_column_wrap(
          #   width = 1 / 2,
          #   card(
          #     card_body(
          #         withSpinner(imageOutput(ns("map")))
          #     )
          #   ),
          #   card(
          #     card_body(
          #       tabsetPanel(
          #         tabPanel(
          #           "Executive Summary",
          #           textOutput(ns("executive_summary"))
          #         ),
          #         tabPanel(
          #           "Introduction",
          #           textOutput(ns("introduction"))
          #         ),
          #         tabPanel(
          #           "Who is Fishing",
          #           textOutput(ns("who_is_fishing"))
          #         )
          #       )
          #     )
          #   )
            # tabsetPanel(
            #   tabPanel(
            #     "Executive Summary",
            #     textOutput(ns("executive_summary"))
            #   ),
            #   tabPanel(
            #     "Introduction",
            #     textOutput(ns("introduction"))
            #   ),
            #   tabPanel(
            #     "Who is Fishing",
            #     textOutput(ns("who_is_fishing"))
            #   )
            # )
  #         )
  #       # )
  #     # )
  #   )
  # )
# }






    # tabsetPanel(type = "hidden", 
    #             id=ns("overview"),
    #             tabPanel("Map tab", value=ns("tab_map"),
    #                      card(imageOutput(ns("map")))
    #                      )),
    # tabsetPanel(
    #     tabPanel("Executive Summary",
    #              textOutput(ns("executive_summary"))),
    #     tabPanel("Introduction",
    #              textOutput(ns("introduction"))),
    #     tabPanel("Who is Fishing",
    #              textOutput(ns("who_is_fishing")))
    # )
                    
  # )
# }
    
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
  
    
    output$executive_summary <- renderText({
      select_text(texts,"overview","executive_summary")
    })
    output$introduction <- renderText({
      select_text(texts,"overview","introduction")
    })
    output$who_is_fishing <- renderText({
      select_text(texts,"overview","who_is_fishing")
    })
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
