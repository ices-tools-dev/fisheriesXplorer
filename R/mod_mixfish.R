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
               plotOutput(ns("headline_bars"), height = "65vh"),
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
      catchScenarioStk
      
    })
    
    data_filter_module <- select_group_server(
      id = "my-filters",
      data_r = data_reactive(),
      vars_r = reactive(c("scenario", "stock"))
    )
    
    
    catchRange <- reactiveValues()
    catchRange$df <- rbind(
      data.frame(stock = "COD-NS", advice = 14276, lower = 9701, upper = 14276),
      data.frame(stock = "HAD", advice = 128708, lower = 111702, upper = 128708),
      data.frame(stock = "PLE-EC", advice = 6365, lower = 4594, upper = 6365),
      data.frame(stock = "PLE-NS", advice = 142507, lower = 101854, upper = 195622),
      data.frame(stock = "POK", advice = 49614, lower = 30204, upper = 49614),
      data.frame(stock = "SOL-EC", advice = 1810, lower = 1068, upper = 2069),
      data.frame(stock = "SOL-NS", advice = 15330, lower = 9523, upper = 21805),
      data.frame(stock = "TUR", advice = 3609, lower = 2634, upper = 4564),
      data.frame(stock = "WHG-NS", advice = 88426, lower = 70169, upper = 91703),
      data.frame(stock = "WIT", advice = 1206, lower = 875, upper = 1206)
    )
    
    
    observeEvent(input$`my-filters-stock`, {
      
      catchRange$df_filtered <- filter(catchRange$df, stock %in% input$`my-filters-stock`)
    })
    
    
    output$headline_bars <- renderPlot({
      
      if(is.null(input$`my-filters-stock`)){
        plot_catchScenStk(data =  data_filter_module(), adv = catchRange$df)
        
      } else {
        plot_catchScenStk(data =  data_filter_module(), adv = catchRange$df_filtered)
        
      }
      
      
    }) 
  })
}
    
## To be copied in the UI
# mod_mixfish_ui("mixfish_1")
    
## To be copied in the server
# mod_mixfish_server("mixfish_1")
