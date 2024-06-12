#' stock_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card_image
#' @importFrom icesFO plot_CLD_bar plot_kobe plot_stock_trends plot_status_prop_pies plot_GES_pies
#' @importFrom dplyr filter slice_max
#' 
mod_stock_status_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Status Summary",
               radioButtons(ns("status_indicator_selector"), "Select status indicator",
                            choices = c("MSY / PA" = "ices", "GES" = "ges")),
               card(imageOutput(ns("status_summary")))),
      tabPanel("Trends-by-groups",
               radioButtons(ns("status_trend_selector"), "Select group",
                            choices = c("Elasmobranchs" = "elasmobranch",
                                        "Benthic" = "benthic",
                                        "Crustacean" = "crustacean",
                                        "Demersal" = "demersal",
                                        "Pelagic" = "pelagic")),
               card(plotOutput(ns("status_trends")))),
      tabPanel("Kobe-CLD",
               radioButtons(ns("status_kobe_cld_selector"), "Select group",
                            choices = c("All Stocks" = "All",
                                        "Benthic" = "benthic",
                                        "Demersal" = "demersal",
                                        "Crustacean" = "crustacean",
                                        "Pelagic" = "pelagic")),
               uiOutput(ns("slider")),
               card(plotOutput(ns("status_kobe")),
                    plotOutput(ns("status_cld")))),
      tabPanel("Stock status Lookup",
               selectInput(ns("stock_status_selector"), "Select stock to view status", choices = "A stock"), 
               radioButtons(ns("stock_status_indicator_selector"), "Select status indicator",
                            choices = c("MSY / PA" = "ices", "GES" = "ges")),
               plotOutput("stock_status"))
      
    )
 
  )
}
    
#' stock_status Server Functions
#'
#' @noRd 
mod_stock_status_server <- function(id, cap_year, cap_month){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$status_summary <- renderPlot({
      req(!is.null(input$status_indicator_selector))
      
      if(input$status_indicator_selector == "ices") {
        plot_status_prop_pies(clean_status, cap_month, cap_year)
      
        } else if((input$status_indicator_selector == "ges")) {
        
          plot_GES_pies(clean_status, current_catches, cap_month, cap_year)
        }
      })
   
    
    trends_data <- reactive({
      
      #stopifnot(sum(is.na(current_catches$FisheriesGuild)) ==0)
      
      if(input$status_trend_selector == "all_stocks") {
        guild <- c("demersal", "pelagic", "crustacean", "benthic", "elasmobranch")
      } else {
        guild <- input$status_trend_selector
      }
      tmp <- trends %>% filter(FisheriesGuild %in% guild)
      
    })
    
     output$status_trends <- renderPlot({
      req(!is.null(input$status_trend_selector))
      
       plot_stock_trends(trends_data(), guild = input$status_trend_selector, cap_year, cap_month , return_data = FALSE)
       })
     
     kobe_cld_data <- reactive({
       
       #stopifnot(sum(is.na(current_catches$FisheriesGuild)) ==0)
       
       if(input$status_kobe_cld_selector == "All") {
         guild <- c("demersal", "pelagic", "crustacean", "benthic", "elasmobranch")
         tmp <- current_catches %>% filter(FisheriesGuild %in% guild)
         tmp <- plot_CLD_bar(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = TRUE) 
         
       } else {
         guild <- input$status_kobe_cld_selector
        tmp <- current_catches %>% filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = TRUE) 
        
       }
       
     })
     
     output$slider <- renderUI({
       slider_max <- nrow(kobe_cld_data())
       sliderInput(ns("n_selector"), "Select number of stocks to display",  min = 1, max = slider_max, value = min(10, slider_max))
     })
     
     output$status_kobe <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
      req(!is.null(input$n_selector))
      
        plot_data <- kobe_cld_data() %>% 
          slice_max(order_by = total, n = input$n_selector)
       plot_kobe(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = FALSE)
        
    })
    
      output$status_cld <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
        req(!is.null(input$n_selector))
        plot_data <- kobe_cld_data() %>% 
          slice_max(order_by = total, n = input$n_selector)
        plot_CLD_bar(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = FALSE)
        
    })
  
     
  })
}
    
## To be copied in the UI
# mod_stock_status_ui("stock_status_1")
    
## To be copied in the server
# mod_stock_status_server("stock_status_1")
