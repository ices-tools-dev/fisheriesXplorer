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
#' @importFrom dplyr filter slice_max mutate select group_by if_else row_number ungroup summarize_all rename
#' @importFrom reactable reactable reactableOutput renderReactable colDef
#' 
mod_stock_status_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # div(
    #   style = "display: flex; justify-content: space-between; align-items: center;
    #        padding: 10px; font-weight: bold; font-size: 1.2em; margin-bottom: 0px;",
    #   span(textOutput(ns("ecoregion_label"))),
    #   span(textOutput(ns("current_date")))
    # ),
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    navset_tab(
      nav_panel(
        "Status Summary",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text1"))
          ),
          fluidRow(
            column(
              6,
              card(
                height = "85vh", full_screen = T,
                card_header("MSY & Precautionary Approach"),
                card_body(
                  fillable = T,
                  withSpinner(
                    plotOutput(ns("status_summary_ices"), height = "72vh"),
                    caption = "Getting status data..."
                  ),
                  downloadLink(ns("download_clean_status_data"), HTML(paste0("<span class='hovertext' data-hover='Download stock status (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>")))
                )
              )
            ),
            column(
              6,
              card(
                height = "85vh", full_screen = T,
                card_header("Good Environmental Status"),
                card_body(
                  fillable = T,
                  withSpinner(
                    plotOutput(ns("status_summary_ges"), height = "72vh"),
                    caption = "Getting assessment data..."
                  ),
                  downloadLink(ns("download_status_catch_data"), HTML(paste0("<span class='hovertext' data-hover='Download stock status (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>")))
                )
              )
            )
          )
        )
      ),
      nav_panel(
        "Trends by group",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text2"))
          ),
          column(
            12,
            card(
              height = "85vh", full_screen = T,
              card_header(
                div(
                  style = "margin-left: 12px;",
                  radioButtons(ns("status_trend_selector"), "Select group",
                    inline = T,
                    choices = c(
                      "Elasmobranchs" = "elasmobranch",
                      "Benthic" = "benthic",
                      "Crustacean" = "crustacean",
                      "Demersal" = "demersal",
                      "Pelagic" = "pelagic"
                    )
                  )
                )
              ),
              card_body(
                withSpinner(
                  plotlyOutput(ns("status_trends"), height = "68vh")
                ),
                downloadLink(ns("download_trends_data"), HTML(paste0("<span class='hovertext' data-hover='Download stock status trends (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>")))
              )
            )
          )
        )
      ),
      nav_panel(
        "Kobe-CLD",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black", open = FALSE,
            uiOutput(ns("status_text3"))
          ),
          card(
            card_header(
              column(
                6,
                radioButtons(ns("status_kobe_cld_selector"), "Select group",
                  inline = T,
                  choices = c(
                    "Benthic" = "benthic",
                    "Demersal" = "demersal",
                    "Crustacean" = "crustacean",
                    "Pelagic" = "pelagic",
                    "All Stocks" = "All"
                  )
                )
              ),
              column(
                6,
                 div(style = "text-align: right;", uiOutput(ns("kobe_cld_slider")))
              )
            )
          ),
          fluidRow(
            column(
              6,
              card(
                fillable = T, height = "70vh", full_screen = T,
                withSpinner(plotOutput(ns("status_cld"), height = "67vh")),
                downloadLink(ns("download_CLD_data"), HTML(paste0("<span class='hovertext' data-hover='Download stock status relative to exploitation and stock size (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>")))
              )
            ),
            column(
              6,
              card(
                fillable = T, height = "75vh", full_screen = T,
                withSpinner(plotOutput(ns("status_kobe"), height = "67vh"))
              )
            )
          )
        )
      ),
      nav_panel(
        "Stock status Lookup",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text4"))
          ),
          downloadLink(ns("download_status_table"), HTML(paste0("<span class='hovertext' data-hover='Download stock status relative to exploitation and stock size (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))),
          withSpinner(reactableOutput(ns("stock_status_table_reactable")))
        )
      )
    )
  )
}
        
    
#' stock_status Server Functions
#'
#' @noRd 
mod_stock_status_server <- function(id, cap_year, cap_month, selected_ecoregion, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      paste0("Last update: ", format(Sys.Date(), "%B %d, %Y")) # e.g., "May 26, 2025"
    })

    output$status_text1 <- output$status_text2 <- output$status_text3 <- output$status_text4 <- renderUI({
      HTML(select_text(texts, "status", "sidebar"))
    })

    
    catch_current <- reactive({
      stockstatus_CLD_current(format_sag(shared$SAG, shared$SID))
    })

    output$status_summary_ices <- renderPlot({
      plot_status_prop_pies(shared$clean_status, cap_month, cap_year)
    })

    # Download handler
    output$download_clean_status_data <- downloadHandler(
      filename = function() {
        paste0("plot_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(shared$clean_status, file, row.names = FALSE)
      }
    )

    output$status_summary_ges <- renderPlot({      
      plot_GES_pies(shared$clean_status, catch_current(), cap_month, cap_year)
    })

    # Download handler
    output$download_status_catch_data <- downloadHandler(
      filename = function() {
        paste0("plot_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(plot_GES_pies(shared$clean_status, catch_current(), cap_month, cap_year, return_data = TRUE), file, row.names = FALSE)
      }
    )


    trends_data <- reactive({
      stock_trends(format_sag(shared$SAG, shared$SID))     
    })

    output$status_trends <- renderPlotly({
      req(!is.null(input$status_trend_selector))
      if (input$status_trend_selector == "all_stocks") {
        guild <- c("demersal", "pelagic", "crustacean", "benthic", "elasmobranch")
      } else {
        guild <- input$status_trend_selector
      }
      
      plot_stock_trends(trends_data(), guild, cap_year, cap_month)
    })

    # Download handler
    output$download_trends_data <- downloadHandler(
      filename = function() {
        paste0("plot_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(trends_data(), file, row.names = FALSE)
      }
    )


    output$kobe_cld_slider <- renderUI({
      slider_max <- nrow(kobe_cld_data())
      div(
        id = "custom_slider",
        sliderInput(ns("n_selector"), HTML("Top <em>n</em> stocks"),
          min = 1, max = slider_max, value = min(10, slider_max), step = 1
        )
      )
    })

    kobe_cld_data <- reactive({
      if (input$status_kobe_cld_selector == "All") {
        guild <- c("demersal", "pelagic", "crustacean", "benthic", "elasmobranch")
        tmp <- catch_current() %>% filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = TRUE)
      } else {
        guild <- input$status_kobe_cld_selector
        tmp <- catch_current() %>% filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = TRUE)
      }
    })


    output$status_kobe <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
      req(!is.null(input$n_selector))

      plot_data <- kobe_cld_data() %>%
        slice_max(order_by = total, n = input$n_selector)
      plot_kobe_app(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = FALSE)
    })

    output$status_cld <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
      req(!is.null(input$n_selector))
      plot_data <- kobe_cld_data() %>%
        slice_max(order_by = total, n = input$n_selector)
      plot_CLD_bar_app(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = FALSE)
    })

    # Download handler
    output$download_CLD_data <- downloadHandler(
      filename = function() {
        paste0("plot_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(kobe_cld_data(), file, row.names = FALSE)
      }
    )

    processed_data_reactable <- reactive({
      annex_data <- format_annex_table(shared$clean_status, as.integer(format(Sys.Date(), "%Y")), shared$SID, shared$SAG)

      annex_data_cleaned <- annex_data %>%
        mutate(
          icon = paste0("<img src='", paste0("www/fish/", match_stockcode_to_illustration(StockKeyLabel, .)), "' height=30>"),
          StockKeyLabel = paste0("<a href='https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=", AssessmentKey, "&assessmentcomponent=", AssessmentComponent,"' target='_blank'>", StockKeyLabel, ifelse(is.na(AssessmentComponent), "", paste0(" (", AssessmentComponent, ")")), "</a>")
        ) %>%
        select(
          "Stock code (component)" = StockKeyLabel,
          # "Stock component" = stockComponent,
          "Stock Description" = StockKeyDescription,
          " " = icon,
          "Scientific Name" = SpeciesScientificName,
          "Common Name" = SpeciesCommonName,
          "Fisheries Guild" = FisheriesGuild,
          "Data Category" = DataCategory,
          #  "Assessment Year" = AssessmentYear,
          "Assessment Year" = YearOfLastAssessment,
          "Advice Category" = AdviceCategory,
          "Approach" = lineDescription,
          "Fishing Pressure" = FishingPressure,
          "Stock Size" = StockSize
        ) %>%
        mutate(Approach = tolower(Approach)) %>% # Ensure consistent case
        tidyr::pivot_wider(
          names_from = Approach,
          values_from = c(`Fishing Pressure`, `Stock Size`),
          names_glue = "{Approach}_{.value}"
        ) %>%
        mutate(
          `MSY Fishing Pressure` = sapply(`maximum sustainable yield_Fishing Pressure`, icon_mapping),
          `MSY Stock Size` = sapply(`maximum sustainable yield_Stock Size`, icon_mapping),
          `PA Fishing Pressure` = sapply(`precautionary approach_Fishing Pressure`, icon_mapping),
          `PA Stock Size` = sapply(`precautionary approach_Stock Size`, icon_mapping)
        ) %>%
        select(
          -`maximum sustainable yield_Fishing Pressure`, -`maximum sustainable yield_Stock Size`,
          -`precautionary approach_Fishing Pressure`, -`precautionary approach_Stock Size`
        )
    })


    # Download handler
    output$download_status_table <- downloadHandler(
      filename = function() {
        paste0("plot_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(format_annex_table(shared$clean_status, as.integer(format(Sys.Date(), "%Y")), shared$SID, shared$SAG), file, row.names = FALSE)
      }
    )

    output$stock_status_table_reactable <- renderReactable({
      req(nrow(processed_data_reactable()) != 0)

      reactable(processed_data_reactable(),
        filterable = TRUE,
        defaultPageSize = 150,
        resizable = TRUE,
        wrap = TRUE,
        bordered = TRUE,
        columns = list(
          "Stock code (component)" = colDef(html = T, filterable = T),
          " " = colDef(html = T, filterable = F, style = list(textAlign = "center")),
          "MSY Fishing Pressure" = colDef(html = T, filterable = F, style = list(textAlign = "center")),
          "MSY Stock Size" = colDef(html = T, filterable = F, style = list(textAlign = "center")),
          "PA Fishing Pressure" = colDef(html = T, filterable = F, style = list(textAlign = "center")),
          "PA Stock Size" = colDef(html = T, filterable = F, style = list(textAlign = "center"))
        ),
        columnGroups = list(
          reactable::colGroup(name = "Maximum sustainable yield", columns = c("MSY Fishing Pressure", "MSY Stock Size")),
          reactable::colGroup(name = "Precautionary approach", columns = c("PA Fishing Pressure", "PA Stock Size"))
        )
      )
    })
  })
}
    
## To be copied in the UI
# mod_stock_status_ui("stock_status_1")
    
## To be copied in the server
# mod_stock_status_server("stock_status_1")
