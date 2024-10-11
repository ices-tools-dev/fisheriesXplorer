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
mod_vms_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(6,
       layout_sidebar(bg = "white", fg = "black",
                      sidebar = sidebar(width = "100%", bg = "white", fg = "black",
                                        open = F,
                                        uiOutput(ns("sar_text"), height = "65vh")),
          card(height = "85vh", full_screen = T,
            card_header("Fishing Effort"),
            card_body(
              selectInput(ns("fishing_cat_selector"), "Select fishing category", 
                choices = c("All"= "all", "Beam trawls", "Bottom otter trawls", "Bottom seines", "Dredges", "Pelagic trawls and seines", "Static gears"),
                selected = "Beam trawls"),
              withSpinner(suppressWarnings(plotOutput(ns("effort_layer"), height = "65vh", width = "100%", fill =T)))
            )
          ))),
      column(6,
       layout_sidebar(bg = "white", fg = "black", 
                      sidebar = sidebar(width = "100%", bg = "white", fg = "black", 
                                        open = F,
                                        uiOutput(ns("benthic_impact_text"))),
            card(height = "85vh", full_screen = T,
              card_header("Fishing Benthic Impact"),
              card_body(
                div(style = "margin-top: 20px; margin-bottom: 14.432px", 
                    radioButtons(ns("sar_layer_selector"), "Select fishing benthic impact level",inline = T,
                  choices = c("Surface" = "surface", "Subsurface" = "subsurface"))
                  ),
                suppressWarnings(withSpinner( suppressWarnings(plotOutput(ns("sar_layer"), height = "65vh", width = "100%", fill =T))))
              )
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
     
       plot_effort_map_app(effort_maps[["Greater North Sea"]], 
                          ecoregion[["Greater North Sea"]], 
                          europe_shape = europe_land_shp, 
                          fishing_category = input$fishing_cat_selector,
                          crs = CRS_LAEA_EUROPE) +
        ggtitle(paste0("Average MW Fishing hours ", paste(year(Sys.Date())-4, year(Sys.Date()), sep = "-")))
     
      })
    
    output$sar_layer <- renderPlot({
      req(!is.null(input$sar_layer_selector))
      plot_sar_map_app(sar_maps[["Greater North Sea"]], 
                       ecoregion[["Greater North Sea"]], 
                       europe_shape = europe_land_shp, 
                       layer = input$sar_layer_selector,
                       crs = CRS_LAEA_EUROPE) +
        ggtitle(glue("Average {input$sar_layer_selector} swept area ratio ", paste(year(Sys.Date())-4, year(Sys.Date()), sep = "-")))
    })
  })
}
    
## To be copied in the UI
# mod_vms_ui("vms_1")
    
## To be copied in the server
# mod_vms_server("vms_1")
