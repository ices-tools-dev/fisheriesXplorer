#' stock_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card_image navset_tab nav_panel layout_sidebar sidebar
#' @importFrom icesFO plot_CLD_bar plot_kobe plot_stock_trends plot_status_prop_pies plot_GES_pies
#' @importFrom dplyr filter slice_max mutate select group_by if_else row_number ungroup summarize_all
#' @importFrom reactable reactable reactableOutput renderReactable colDef
#' 
mod_stock_status_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_tab(
       nav_panel("Status Summary",
          layout_sidebar(
            sidebar = sidebar(width = "33vw", bg = "white", fg = "black", 
                                       open = FALSE,
                                       uiOutput(ns("status_text1"))),
          fluidRow(column(6,
           card(height = "85vh", full_screen = T,
            card_header("MSY & Precautionary Approach"),
            card_body(fillable = T, 
              withSpinner(
                plotOutput(ns("status_summary_ices"), height = "72vh"))))
          ),
          column(6,          
           card(height = "85vh", full_screen = T,
            card_header("Good Environmental Status"),
            card_body(fillable = T,
              withSpinner(
                plotOutput(ns("status_summary_ges"), height = "72vh")))
                
            )
          ))
      )),
      
      nav_panel("Trends by group",
          layout_sidebar(
            sidebar = sidebar(width = "33vw", bg = "white", fg = "black", 
                                       open = FALSE,
                                       uiOutput(ns("status_text2"))),
          column(12,
           card(height = "85vh", full_screen = T,
             card_header(
              div(style = "margin-left: 12px;",
               radioButtons(ns("status_trend_selector"), "Select group", inline = T,
              choices = c(
                "Elasmobranchs" = "elasmobranch",
                "Benthic" = "benthic",
                "Crustacean" = "crustacean",
                "Demersal" = "demersal",
                "Pelagic" = "pelagic")))),
          card_body(
            withSpinner(
              plotOutput(ns("status_trends"), height = "68vh")))
            )
          )
      )),
      
      nav_panel("Kobe-CLD",
          layout_sidebar(
            sidebar = sidebar(width = "33vw", bg = "white", fg = "black", open = FALSE,
                              uiOutput(ns("status_text3"))),
        card(card_header(
          column(6,
            radioButtons(ns("status_kobe_cld_selector"), "Select group", inline = T,
              choices = c(
                "Benthic" = "benthic",
                "Demersal" = "demersal",
                "Crustacean" = "crustacean",
                "Pelagic" = "pelagic",
                "All Stocks" = "All")
            )
          ),
          column(6,
            uiOutput(ns("kobe_cld_slider"), inline = T)
                 ))
        ),
        fluidRow(
          column(6,
            card(fillable = T, height = "75vh", full_screen = T,
              withSpinner(plotOutput(ns("status_cld"), height = "67vh"))
                   )),
          column(6,
            card(fillable = T, height = "75vh", full_screen = T,
              withSpinner(plotOutput(ns("status_kobe"), height = "67vh"))
                   )
          )
        )
      )),
      
      nav_panel("Stock status Lookup",
          layout_sidebar(
            sidebar = sidebar(width = "33vw", bg = "white", fg = "black", 
                                       open = FALSE,
                                       uiOutput(ns("status_text4"))),
            
        withSpinner(reactableOutput(ns("stock_status_table_reactable")))
      )
      )
    )
  )
}
        
    
#' stock_status Server Functions
#'
#' @noRd 
mod_stock_status_server <- function(id, cap_year, cap_month, selected_ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$status_text1 <- output$status_text2 <- output$status_text3 <- output$status_text4 <- renderUI({
      HTML(select_text(texts,"status","sidebar"))
    })
    
    SID <- reactive({      
      # dat <- prepare_ices_stock_status(clean_status)
      getSID(year = 2024, EcoR = selected_ecoregion())
      # format_sag_status_new(status)
    })


    ices_prop_pies_data <- reactive({
      # dat <- prepare_ices_stock_status(clean_status)
      # status <- getStatus(year = 2024, EcoR = "Greater North Sea")     
      format_sag_status_new(getStatus(SID()))
    })
    
    catch_current <- reactive({
      # dat <- prepare_ges_stock_status(status_df = clean_status, catch_df = current_catches)
      sag <- getSAG_ecoregion(2024, selected_ecoregion(), SID())
      stockstatus_CLD_current(format_sag(sag, SID()))
    })
    
    output$status_summary <- renderPlot({
      req(!is.null(input$status_indicator_selector))
      
      if(input$status_indicator_selector == "ices") {
        plot_status_prop_pies_app(ices_prop_pies_data(), cap_month, cap_year)
      
        } else if((input$status_indicator_selector == "ges")) {
        
          plot_GES_pies_app(ges_prop_pies_data(), cap_month, cap_year)
        }
    })
    output$status_summary_ices <- renderPlot({
      # plot_status_prop_pies_app(ices_prop_pies_data(), cap_month, cap_year)
      
      plot_status_prop_pies(ices_prop_pies_data(), cap_month, cap_year)
        
    })
    output$status_summary_ges <- renderPlot({
        # plot_GES_pies_app(ges_prop_pies_data(), cap_month, cap_year)
        plot_GES_pies(ices_prop_pies_data(), catch_current(),  cap_month, cap_year)
    })
   
    
    trends_data <- reactive({
      
      #stopifnot(sum(is.na(current_catches$FisheriesGuild)) ==0)
      
      if(input$status_trend_selector == "all_stocks") {
        guild <- c("demersal", "pelagic", "crustacean", "benthic", "elasmobranch")
      } else {
        guild <- input$status_trend_selector
      }
      
      dat <- trends %>% filter(FisheriesGuild %in% guild) %>% 
        prepare_stock_trends()
      
    })
    
     output$status_trends <- renderPlot({
      req(!is.null(input$status_trend_selector))

       # ggplotly(plot_stock_trends_app(trends_data(), guild = input$status_trend_selector, caption_year = cap_year, caption_month = cap_month))
       plot_stock_trends_app(trends_data(), caption_year = cap_year, caption_month = cap_month)
       })
     
     
     output$kobe_cld_slider <- renderUI({
       slider_max <- nrow(kobe_cld_data())
       div(id="custom_slider",
           sliderInput(ns("n_selector"), HTML("Top <em>n</em> stocks"),  
                       min = 1, max = slider_max, value = min(10, slider_max), step = 1)
           )
    })
     
     kobe_cld_data <- reactive({
       
       if(input$status_kobe_cld_selector == "All") {
         guild <- c("demersal", "pelagic", "crustacean", "benthic", "elasmobranch")
         tmp <- catch_current() %>% filter(FisheriesGuild %in% guild)
         tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = TRUE) 
         
       } else {
         guild <- input$status_kobe_cld_selector
        tmp <- catch_current() %>% filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = TRUE) 
        
       }
       
     })
     
     
     output$status_kobe <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
        req(!is.null(input$n_selector))
      
        plot_data <- kobe_cld_data() %>% 
          slice_max(order_by = total, n = input$n_selector)
       plot_kobe_app(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = FALSE)
        
    })
    
      output$status_cld <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
        req(!is.null(input$n_selector))
        plot_data <- kobe_cld_data() %>% 
          slice_max(order_by = total, n = input$n_selector)
        plot_CLD_bar_app(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month , return_data = FALSE)
        
    })
    
    
    processed_data_reactable <- reactive({

      # annex_data <- all_data[[selected_ecoregion()]]$stock_annex_table
      annex_data <- format_annex_table(ices_prop_pies_data(), 2024, SID())
      annex_data %>%
        group_by(StockKeyLabel, StockKeyDescription, SpeciesScientificName, 
                 SpeciesCommonName, FisheriesGuild.y, DataCategory, 
                  AdviceCategory, lineDescription, GES, SBL) %>%
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
            #  "Assessment Year" = AssessmentYear,
            "Assessment Year" = YearOfLastAssessment,
             "Advice Category" = AdviceCategory,
             "Approach" = lineDescription,
             "Fishing Pressure",
             "Stock Size", D3C1, D3C2, GES, SBL)
    })
    
    
    output$stock_status_table_reactable <- renderReactable({
      req(nrow(processed_data_reactable())!=0)
      reactable(processed_data_reactable(), 
                filterable = TRUE,
                defaultPageSize = 100,
                columns = list(
                               # "Stock code" = colDef(html = T, cell = merge_cells),
                               # "Stock Description" = colDef(html = T, cell = merge_cells),
                               # "Scientific Name" = colDef(html = T, cell = merge_cells),
                               # "Common Name" = colDef(html = T, cell = merge_cells),
                               # "Fisheries Guild" = colDef(html = T, cell = merge_cells),
                               "Fisheries Guild" = colDef(html = T, width = 108),
                               # "Data Category" = colDef(html = T, cell = merge_cells),
                               "Data Category" = colDef(html = T, width = 80),
                               "Assessment Year" = colDef(html = T, width = 80),
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
