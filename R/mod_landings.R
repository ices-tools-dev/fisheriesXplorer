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
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom shinycssloaders withSpinner

mod_landings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Landings",
        layout_sidebar(bg = "white", fg = "black", 
          sidebar = sidebar(width = "33vw", bg = "white", fg = "black", 
                            open = FALSE,
                            uiOutput(ns("landings_text"))),
          card(height = "85vh",
            card_header(
              div(style = "margin-left: 12px;",
                radioButtons(ns("landings_layer_selector"), NULL, inline = T,
                  choices = c("Main landed species" = "COMMON_NAME", "Guild" = "GUILD", "Country" = "COUNTRY")))),
            card_body(withSpinner(
              plotlyOutput(ns("landings_layer"), height = "65vh")))
          ))),
      tabPanel("Discards",
        layout_sidebar(bg = "white", fg = "black", 
          sidebar = sidebar(width = "33vw", bg = "white", fg = "black", 
                            open = FALSE,
                            uiOutput(ns("discards_text"))),
          card(height = "85vh",
            card_header(
              div(style = "margin-left: 12px;",
                radioButtons(ns("discards_layer_selector"), NULL, inline = TRUE,
                             choices = c("Discard rates by guild   " = "rates", "Landings and discards (Stocks with recorded discards only)    " = "recorded", "Landings and discards (All_stocks) " = "all")))),
            card_body(
              conditionalPanel(ns = NS("landings_1"),
                condition = "input.discards_layer_selector == 'rates'",
                withSpinner(plotlyOutput(ns("discard_trends")))
                ),
              conditionalPanel(ns = NS("landings_1"),
                condition = "input.discards_layer_selector == 'recorded'",
                withSpinner(plotlyOutput(ns("recorded_discards")))
                ),
              conditionalPanel(ns = NS("landings_1"),
                condition = "input.discards_layer_selector == 'all'",
                withSpinner(plotlyOutput(ns("all_discards")))
              )
            )
          )
        )
      )
    )
  )
}
    
#' landings Server Functions
#'
#' @noRd 
mod_landings_server <- function(id, cap_year, cap_month, selected_ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$landings_text <- renderUI({
      HTML(select_text(texts,"landings_discards","landings"))
    })
    
    output$discards_text <- renderUI({
      HTML(select_text(texts,"landings_discards","discards"))
    })

    output$landings_layer <- renderPlotly({
      req(!is.null(input$landings_layer_selector))

      plotting_params <- list()
      plotting_params$landings <- list(
        "COMMON_NAME" = list("n" = 10, type = "line"),
        "GUILD" = list("n" = 6, type = "line"),
        "COUNTRY" = list("n" = 9, type = "area")
      )
      AZ
      params <- plotting_params$landings[[input$landings_layer_selector]]
      
      ecoregion <- selected_ecoregion()
      
      fig <- ggplotly(plot_catch_trends_app_new(get(get_ecoregion_acronym(ecoregion)), type = input$landings_layer_selector, line_count = params$n, plot_type = params$type, official_catches_year = as.numeric(cap_year))) %>%
        plotly::layout(legend = list(orientation = "v", title = list(text = paste0("<b>", input$landings_layer_selector, "</b>"))))
      
      for (i in 1:length(fig$x$data)) {
        if (!is.null(fig$x$data[[i]]$name)) {
          fig$x$data[[i]]$name <- gsub("\\(", "", str_split(fig$x$data[[i]]$name, ",")[[1]][1])
        }
      }
      fig
    })
    
    year <- 2022
    
    output$discard_trends <- renderPlotly({
      fig2 <- ggplotly(plot_discard_trends_app(catch_trends, year, cap_year , cap_month, caption = F)) 
      for (i in 1:length(fig2$x$data)) {
        if (!is.null(fig2$x$data[[i]]$name)) {
          fig2$x$data[[i]]$name <- gsub("\\(", "", str_split(fig2$x$data[[i]]$name, ",")[[1]][1])
        }
      }
      fig2
    })
    output$recorded_discards <- renderPlotly({
      ggplotly(plot_discard_current_app(catch_trends, year, cap_year , cap_month, position_letter = ""))
    })
    output$all_discards <- renderPlotly({
      ggplotly(plot_discard_current_app(catch_trends, year-1, cap_year , cap_month, position_letter = "" ))
    })
    
  })
}
    
## To be copied in the UI
# mod_landings_ui("landings_1")
    
## To be copied in the server
# mod_landings_server("landings_1")
