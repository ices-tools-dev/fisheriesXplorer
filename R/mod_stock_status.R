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
#' @importFrom dplyr filter slice_max mutate select group_by if_else row_number ungroup summarize_all
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom reactable reactable reactableOutput renderReactable colDef
#' 
mod_stock_status_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Status Summary",
        radioButtons(ns("status_indicator_selector"), "Select status indicator",
          choices = c("MSY / PA" = "ices", "GES" = "ges")
        ),
        card(
          withSpinner(
            imageOutput(ns("status_summary"))))
      ),
      tabPanel(
        "Trends-by-groups",
        radioButtons(ns("status_trend_selector"), "Select group",
          choices = c(
            "Elasmobranchs" = "elasmobranch",
            "Benthic" = "benthic",
            "Crustacean" = "crustacean",
            "Demersal" = "demersal",
            "Pelagic" = "pelagic"
          )
        ),
        card(
          card_body(
            withSpinner(
              plotlyOutput(
                ns("status_trends")))
          )
        )
      ),
      tabPanel(
        "Kobe-CLD",
        radioButtons(ns("status_kobe_cld_selector"), "Select group",
          choices = c(
            "All Stocks" = "All",
            "Benthic" = "benthic",
            "Demersal" = "demersal",
            "Crustacean" = "crustacean",
            "Pelagic" = "pelagic"
          )
        ),
        uiOutput(ns("slider")),
        card(
          withSpinner(plotOutput(ns("status_kobe"))),
          withSpinner(plotOutput(ns("status_cld")))
        )
      ),
      tabPanel(
        "Stock status Lookup",
        withSpinner(reactableOutput(ns("stock_status_table_reactable")))
      )
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
    
     output$status_trends <- renderPlotly({
      req(!is.null(input$status_trend_selector))
      
       ggplotly(plot_stock_trends(trends_data(), guild = input$status_trend_selector, cap_year, cap_month , return_data = FALSE))
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
    
    
    processed_data_DT <- reactive({
      stock_status_data <- read.csv("data-raw/GNS/annex_table.csv")
      
      stock_status_data %>%
      group_by(StockKeyLabel) %>%
      mutate("Fishing Pressure" = sapply(FishingPressure, icon_mapping),
             "Stock Size" = sapply(StockSize, icon_mapping),
             D3C1 = sapply(D3C1, icon_mapping),
             D3C2 = sapply(D3C2, icon_mapping),
             GES = sapply(GES, icon_mapping),
             SBL = sapply(SBL, icon_mapping),
             StockKeyLabel = if_else(row_number() == 2, "", StockKeyLabel),
             StockKeyDescription = if_else(row_number() == 2, "", StockKeyDescription),
             SpeciesScientificName = if_else(row_number() == 2, "", SpeciesScientificName),
             SpeciesCommonName = if_else(row_number() == 2, "", SpeciesCommonName),
             FisheriesGuild.y = if_else(row_number() == 2, "", FisheriesGuild.y),
             DataCategory = if_else(row_number() == 2, "", as.character(DataCategory)),
             AssessmentYear = if_else(row_number() == 2, "", as.character(AssessmentYear)),
             AdviceCategory = if_else(row_number() == 2, "", AdviceCategory)
      ) %>%
        
      select("Stock code" = StockKeyLabel,
             "Stock Description" = StockKeyDescription,
             "Scientific Name" = SpeciesScientificName,
             "Common Name" = SpeciesCommonName,
             "Fisheries Guild" = FisheriesGuild.y,
             "Data Category" = DataCategory,
             "Assessment Year" = AssessmentYear,
             "Advice Category" = AdviceCategory,
             "Approach" = lineDescription,
             "Fishing Pressure",
             "Stock Size", D3C1, D3C2, GES, SBL)
    })
    
    
    processed_data_reactable <- reactive({
      stock_status_data <- read.csv("data-raw/GNS/annex_table.csv")
      
      stock_status_data %>%
        group_by(StockKeyLabel, StockKeyDescription, SpeciesScientificName, 
                 SpeciesCommonName, FisheriesGuild.y, DataCategory, 
                 AssessmentYear, AdviceCategory, lineDescription, GES, SBL) %>%
        summarize_all(~paste(unique(.), collapse = " ")) %>%
        ungroup() %>% 
      mutate("Fishing Pressure" = sapply(FishingPressure, icon_mapping),
             "Stock Size" = sapply(StockSize, icon_mapping),
             D3C1 = sapply(D3C1, icon_mapping),
             D3C2 = sapply(D3C2, icon_mapping),
             GES = sapply(GES, icon_mapping),
             SBL = sapply(SBL, icon_mapping)
      ) %>%
        
      select("Stock code" = StockKeyLabel,
             "Stock Description" = StockKeyDescription,
             "Scientific Name" = SpeciesScientificName,
             "Common Name" = SpeciesCommonName,
             "Fisheries Guild" = FisheriesGuild.y,
             "Data Category" = DataCategory,
             "Assessment Year" = AssessmentYear,
             "Advice Category" = AdviceCategory,
             "Approach" = lineDescription,
             "Fishing Pressure",
             "Stock Size", D3C1, D3C2, GES, SBL)
      #   select(-c(Ecoregion, StockSize, FishingPressure))
    })
      
    
    
    output$stock_status_table <- renderDT({
      req(nrow(processed_data_DT())!=0)
        datatable(processed_data_DT(), 
                  escape = FALSE, # allows HTML to be rendered
                  rownames = FALSE,
                  options = list(
                    rowCallback = JS(
                      "function(row, data, index) {",
                      "  if (index % 2 == 0) {",
                      "    $('td', row).eq(0).attr('rowspan', 2);", # merging row span for the first column
                      "    $('td', row).eq(1).attr('rowspan', 2);",
                      "    $('td', row).eq(2).attr('rowspan', 2);",
                      "    $('td', row).eq(3).attr('rowspan', 2);",
                      "    $('td', row).eq(4).attr('rowspan', 2);",
                      "    $('td', row).eq(5).attr('rowspan', 2);",
                      "    $('td', row).eq(6).attr('rowspan', 2);",
                      "    $('td', row).eq(7).attr('rowspan', 2);",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "    $('td', row.nextElementSibling).eq(0).remove();",
                      "  }",
                      "}")
                  )
        )
      })
    
    output$stock_status_table_reactable <- renderReactable({
      req(nrow(processed_data_reactable())!=0)
      reactable(processed_data_reactable(), filterable = TRUE,
                #groupBy = c("Stock code"),
                columns = list(
                               # "Stock code" = colDef(html = T, cell = merge_cells),
                               # "Stock Description" = colDef(html = T, cell = merge_cells),
                               # "Scientific Name" = colDef(html = T, cell = merge_cells),
                               # "Common Name" = colDef(html = T, cell = merge_cells),
                               # "Fisheries Guild" = colDef(html = T, cell = merge_cells),
                               "Fisheries Guild" = colDef(html = T, width = 108),
                               # "Data Category" = colDef(html = T, cell = merge_cells),
                               "Data Category" = colDef(html = T, width = 80),
                               # "Assessment Year" = colDef(html = T, cell = merge_cells),
                               # "Advice Category" = colDef(html = T, cell = merge_cells),
                              "Advice Category" = colDef(html = T, width = 80),
                              "Approach" = colDef(html = T, width = 108),
                               # SBL = colDef(html = T, filterable = F, cell = merge_cells),
                               # GES = colDef(html = T, filterable = F, cell = merge_cells),
                               "Fishing Pressure" = colDef(html = T, filterable = F, width = 74),
                               SBL = colDef(html = T, filterable = F, width = 74),
                               GES = colDef(html = T, filterable = F, width = 74),
                               D3C1 = colDef(html = T, filterable = F, width = 74),
                               D3C2 = colDef(html = T, filterable = F, width = 74),
                               "Stock Size" = colDef(html = T, filterable = F, width = 74)
                               )
      )
    })
     
  })
}
    
## To be copied in the UI
# mod_stock_status_ui("stock_status_1")
    
## To be copied in the server
# mod_stock_status_server("stock_status_1")
