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
    # div(
    #   style = "display: flex; justify-content: space-between; align-items: center;
    #        padding: 10px; font-weight: bold; font-size: 1.2em; margin-bottom: 0px;",
    #   span(textOutput(ns("ecoregion_label"))),
    #   span(textOutput(ns("current_date")))
    # ),
    tags$script(HTML("
      function toggleFullScreen(elem) {
        if (!document.fullscreenElement) {
          elem.requestFullscreen().catch(err => {
            alert('Error attempting to enable fullscreen: ' + err.message);
          });
        } else {
          document.exitFullscreen();
        }
      }
    ")),
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    br(),
    fluidRow(
      column(
        6,
        layout_sidebar(
          bg = "white", fg = "black",
          sidebar = sidebar(
            width = "100%", bg = "white", fg = "black",
            open = F,
            uiOutput(ns("sar_text"), height = "65vh")
          ),
          card(
            height = "85vh",
            card_header("Fishing Effort"),
            card_body(
              selectInput(ns("fishing_cat_selector"), "Select fishing category",
                choices = c("All" = "all", "Beam trawls", "Bottom otter trawls", "Bottom seines", "Dredges", "Pelagic trawls and seines", "Static gears"),
                selected = "All"
              ),
              tags$style(type = "text/css", "#vms_effort_layer {margin-left: auto; margin-right: auto; margin-bottom: auto;  max-width: 97%; height: auto;}"),
              withSpinner(suppressWarnings(uiOutput(ns("vms_effort_layer"), height = "65vh", width = "100%", fill = T)))
            )
          )
        )
      ),
      column(
        6,
        layout_sidebar(
          bg = "white", fg = "black",
          sidebar = sidebar(
            width = "100%", bg = "white", fg = "black",
            open = F,
            uiOutput(ns("benthic_impact_text"))
          ),
          card(
            height = "85vh",
            card_header("SAR"),
            card_body(
              
              selectInput(ns("sar_layer_selector"), "Select fishing benthic impact level",
                choices = c("All" = "all", "Surface" = "surface", "Subsurface" = "subsurface"),
                selected = "Surface"
              ),
              tags$style(type = "text/css", "#vms_sar_layer {margin-left: auto; margin-right: auto; margin-bottom: auto;  max-width: 97%; height: auto;}"),
              suppressWarnings(withSpinner(suppressWarnings(uiOutput(ns("vms_sar_layer"), height = "65vh", width = "100%", fill = T))))
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
    
    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      "Last update: December 05, 2024" # e.g., "May 26, 2025"
    })

    
    output$vms_effort_layer <- renderUI({
      ecoregion <- get_ecoregion_acronym(selected_ecoregion())
      
      gear_name <- str_replace_all(tolower(input$fishing_cat_selector), " ", "_")
      path <- file.path("inst/app/www/vms", paste0(ecoregion, "_effort_", gear_name ,".jpg"))
      
      # You can serve local files using a temporary copy if needed
      tmp <- normalizePath(path)
      
      tags$img(
        id = ns("vms_effort_layer"),  # ID is now really on the <img>
        src = base64enc::dataURI(file = tmp, mime = "image/jpeg"),
        style = "width: 100%; cursor: pointer;",
        onclick = "toggleFullScreen(this)"
      )
    })
    
    
    output$vms_sar_layer <- renderUI({
      
      ecoregion <- get_ecoregion_acronym(selected_ecoregion())
      
      
      path <- file.path("inst/app/www/vms", paste0(ecoregion, "_sar_", input$sar_layer_selector ,".jpg"))
      
      # You can serve local files using a temporary copy if needed
      tmp <- normalizePath(path)
      
      tags$img(
        id = ns("vms_sar_layer"),  # ID is now really on the <img>
        src = base64enc::dataURI(file = tmp, mime = "image/jpeg"),
        style = "width: 100%; cursor: pointer;",
        onclick = "toggleFullScreen(this)"
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_vms_ui("vms_1")
    
## To be copied in the server
# mod_vms_server("vms_1")

