#' mixfish UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom mixfishtools plot_catchScenStk
#' @importFrom datamods select_group_server select_group_ui
mod_mixfish_ui <- function(id){
  ns <- NS(id)
  tagList(
    navset_tab(
      nav_panel("Mixed fisheries projections",
        layout_sidebar(
          sidebar = sidebar(width = "33vw", bg = "white", fg = "black", 
                                    open = FALSE,
                                    uiOutput(ns("mf_projections_text"))),
        card(height = "80vh", full_screen = T,fill = F,
             card_header("Filter by scenario and stock"),
             card_body(fillable = T, fill = T, class = "p-1",
                      select_group_ui(label = NULL,
                               id = ns("my-filters"),
                               params = list(
                                 scenario = list(inputId = "scenario", "Management Scenario:"),
                                 stock = list(inputId = "stock", "Fish Stock"))),
                                  withSpinner(
               plotlyOutput(ns("headline_bars"), height = "65vh"),
               caption = "Getting mix-fish results..."))
        )
        )
      
    )
    )
  )
}
    
#' mixfish Server Functions
#'
#' @noRd 
mod_mixfish_server <- function(id, selected_ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$mf_projections_text <- renderUI({
      HTML(select_text(texts,"mixfish","projection_sidebar"))
    })
    
    ####### headline bar plot
    data_reactive <- reactive({
      # catchScenarioStk
      download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")
      
    })
    
    data_filter_module <- select_group_server(
      id = "my-filters",
      data_r = data_reactive(),
      vars_r = reactive(c("scenario", "stock"))
    )
    
    
    catchRange <- reactiveValues()
    catchRange$df <- download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice","shiny/Figure1_HeadlinePlot_advice.csv")
    
    
    
    observeEvent(input$`my-filters-stock`, {
      
      catchRange$df_filtered <- filter(catchRange$df, stock %in% input$`my-filters-stock`)
    })
    
    
    output$headline_bars <- renderPlotly({
      
      if(is.null(input$`my-filters-stock`)){
        plot_catchScenStk_plotly(data =  data_filter_module(), adv = catchRange$df)
        
      } else {
        plot_catchScenStk_plotly(data =  data_filter_module(), adv = catchRange$df_filtered)
        
      }
      
      
    }) 
  })
}
    
## To be copied in the UI
# mod_mixfish_ui("mixfish_1")
    
## To be copied in the server
# mod_mixfish_server("mixfish_1")
