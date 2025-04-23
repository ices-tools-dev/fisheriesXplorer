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
mod_mixfish_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Mixed-fisheries headline",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("mf_projections_text"))
          ),
          card(
            height = "80vh", full_screen = T, fill = F,
            card_header("Filter by scenario and stock"),
            card_body(
              fillable = T, fill = T, class = "p-1",
              select_group_ui(
                label = NULL,
                id = ns("my-filters"),
                params = list(
                  scenario = list(inputId = "scenario", label = "Management Scenario:", placeholder = "Select scenario"),
                  stock = list(inputId = "stock", label = "Fish Stock", placeholder = "Select stock")
                )
              ),
              withSpinner(
                plotlyOutput(ns("headline_bars"), height = "65vh"),
                caption = "Getting mix-fish results..."
              )
            )
          )
        )
      ),
      tabPanel(
        "Stock comoposition",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("mf_projections_text"))
          ),
          card(
            height = "80vh", full_screen = T, fill = F,
            card_header("Filter by fleet and year"),
            card_body(
              fillable = T, fill = T, class = "p-1",
              select_group_ui(
                label = NULL,
                id = ns("my-filters_composition"),
                params = list(
                  fleet = list(inputId = "fleet", label = "fleet:", placeholder = "Select fleet"),
                  year = list(inputId = "year", label = "year:", placeholder = "Select year")
                )
              ),
              withSpinner(
                plotOutput(ns("comp_bars"), height = "65vh"),
                caption = "Getting mix-fish results..."
              )
            )
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
    data_reactive_catchScenarioStk <- reactive({
      ecoregion <- selected_ecoregion()
      # NrS_catchScenarioStk
      catchScenarioStk <- catchScenarioStk %>% filter(ecoregion == get_ecoregion_acronym(selected_ecoregion()))
      # download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")
     browser()
    })
    
    data_filter_module <- select_group_server(
      id = "my-filters",
      data_r = data_reactive_catchScenarioStk(),
      vars_r = reactive(c("scenario", "stock"))
    )
    
    
    catchRange <- reactiveValues()
    catchRange$df <- NrS_catchRange
    
    
    
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

    output$comp_bars <- renderPlot({
      # if(is.null(input$`my-filters-stock`)){
      #   plot_catchScenStk_plotly(data =  data_filter_module(), adv = catchRange$df)

      # } else {
      #   plot_catchScenStk_plotly(data =  data_filter_module(), adv = catchRange$df_filtered)
      # }
      library(mixfishtools)
      data(refTable)
      data(stfMtStkSum)
      data <- subset(stfMtStkSum, scenario == "min")

      # add country and area identifiers (if desired)
      tmp <- strsplit(data$metier, ".", fixed = TRUE)
      data$area <- unlist(lapply(tmp, FUN = function(x) {
        ifelse(length(x) == 2, x[2], NA)
      }))
      tmp <- strsplit(data$fleet, "_", fixed = TRUE)
      data$country <- unlist(lapply(tmp, FUN = function(x) {
        ifelse(length(x) == 2, x[1], NA)
      }))
      # replace stock with ICES stock code
      data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]
      # Plot catch composition for each fleet over time
      selectors <- c("year")
      divider <- c("fleet")
      p <- plot_catchComp(data, refTable, filters = NULL, selectors, divider, yvar = "catch")
      p
    })

  })
}
    
## To be copied in the UI
# mod_mixfish_ui("mixfish_1")
    
## To be copied in the server
# mod_mixfish_server("mixfish_1")
