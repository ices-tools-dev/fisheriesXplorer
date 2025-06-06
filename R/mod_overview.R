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
    # div(
    #   style = "display: flex; justify-content: space-between; align-items: center;
    #        padding: 10px; font-weight: bold; font-size: 1.2em; margin-bottom: 0px;",
    #   span(textOutput(ns("ecoregion_label"))),
    #   span(textOutput(ns("current_date")))
    # ),
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    # Fullscreen script added here once
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
    tabsetPanel(
      type = "hidden",
      id = ns("overview"),
        card(min_height = "80vh",
      layout_sidebar(
        fillable = T, bg = "white", fg = "black", 
        sidebar = sidebar(bg = "white", fg = "black",
            width = "50%", 
            card(min_height = "50vh", height = "80vh", full_screen = T, 
              tags$style(type = "text/css", "#staticMap1 {margin-left: auto; margin-right: auto; margin-bottom: auto;  max-width: 97%; height: auto;}"),
                withSpinner(uiOutput(ns("staticMap1"), width = "100%")))
        ),
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
mod_overview_server <- function(id, selected_ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      "Last update: December 05, 2024" # e.g., "May 26, 2025"
    })

  output$staticMap1 <- renderUI({
  ecoregion <- get_ecoregion_acronym(selected_ecoregion())
  path <- file.path("inst/app/www", paste0(ecoregion, ".jpg"))

  # You can serve local files using a temporary copy if needed
  tmp <- normalizePath(path)

  tags$img(
    id = ns("staticMap1"),  # ID is now really on the <img>
    src = base64enc::dataURI(file = tmp, mime = "image/jpeg"),
    style = "width: 100%; cursor: pointer;",
    onclick = "toggleFullScreen(this)"
  )
})
    output$executive_summary <- renderUI({
      HTML(select_text(texts, paste0("overview_",get_ecoregion_acronym(selected_ecoregion())),"executive_summary"))
    })
    output$introduction <- renderUI({
      HTML(select_text(texts,paste0("overview_",get_ecoregion_acronym(selected_ecoregion())),"introduction"))
    })
    output$who_is_fishing <- renderUI({
      HTML(select_text(texts,paste0("overview_",get_ecoregion_acronym(selected_ecoregion())),"who_is_fishing"))
    })
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
