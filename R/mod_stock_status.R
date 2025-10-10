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
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

    # Give the navset an id; give each nav_panel a stable value
    navset_tab(
      id = ns("main_tabset"),   # <-- NEW

      nav_panel(
        "Status Summary", value = "status_summary",   # <-- NEW value
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
                height = "85vh", full_screen = TRUE,
                card_header(
                  "MSY & Precautionary Approach",
                  downloadLink(ns("download_clean_status_data"),
                    HTML(paste0("<span class='hovertext' data-hover='Download stock status (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                  )
                ),
                card_body(
                  fillable = TRUE,
                  withSpinner(plotOutput(ns("status_summary_ices"), height = "75vh"),
                              caption = "Getting status data...")
                )
              )
            ),
            column(
              6,
              card(
                height = "85vh", full_screen = TRUE,
                card_header(
                  "Good Environmental Status",
                  downloadLink(ns("download_status_catch_data"),
                    HTML(paste0("<span class='hovertext' data-hover='Download stock status (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                  )
                ),
                card_body(
                  fillable = TRUE,
                  withSpinner(plotOutput(ns("status_summary_ges"), height = "75vh"),
                              caption = "Getting assessment data...")
                )
              )
            )
          )
        )
      ),

      nav_panel(
        "Trends by group", value = "trends_by_group",   # <-- NEW value
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text2"))
          ),
          column(
            12,
            card(
              height = "85vh", full_screen = TRUE,
              card_header(
                radioButtons(ns("status_trend_selector"), "Select group",
                  inline = TRUE,
                  choices = c(
                    "Elasmobranchs" = "elasmobranch",
                    "Benthic"       = "benthic",
                    "Crustacean"    = "crustacean",
                    "Demersal"      = "demersal",
                    "Pelagic"       = "pelagic"
                  )
                ),
                downloadLink(ns("download_trends_data"),
                  HTML(paste0("<span class='hovertext' data-hover='Download stock status trends (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                )
              ),
              card_body(withSpinner(plotlyOutput(ns("status_trends"), height = "68vh")))
            )
          )
        )
      ),

      nav_panel(
        "Kobe-CLD", value = "kobe_cld",   # <-- NEW value
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black", open = FALSE,
            uiOutput(ns("status_text3"))
          ),
          card(
            card_header(
              column(
                6,
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding: 0 16px;",
                  radioButtons(ns("status_kobe_cld_selector"), "Select group",
                    inline = TRUE,
                    choices = c(
                      "Benthic"   = "benthic",
                      "Demersal"  = "demersal",
                      "Crustacean"= "crustacean",
                      "Pelagic"   = "pelagic",
                      "All Stocks"= "All"
                    ),
                    selected = "All"
                  )
                )
              ),
              column(
                6,
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding: 0 16px;",
                  uiOutput(ns("kobe_cld_slider")),
                  downloadLink(ns("download_CLD_data"),
                    HTML(paste0("<span class='hovertext' data-hover='Download stock status relative to exploitation and stock size (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              card(fillable = TRUE, height = "70vh", full_screen = TRUE,
                   withSpinner(plotOutput(ns("status_cld"),  height = "67vh")))
            ),
            column(
              6,
              card(fillable = TRUE, height = "75vh", full_screen = TRUE,
                   withSpinner(plotOutput(ns("status_kobe"), height = "67vh")))
            )
          )
        )
      ),

      nav_panel(
        "Stock status Lookup", value = "status_lookup",   # <-- NEW value
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text4"))
          ),
          card(
            card_header(
              "Stock status table",
              downloadLink(ns("download_status_table"),
                HTML(paste0("<span class='hovertext' data-hover='Download stock status relative to exploitation and stock size (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
              )
            ),
            card_body(withSpinner(reactableOutput(ns("stock_status_table_reactable"))))
          )
        )
      )
    )
  )
}

        
    
#' stock_status Server Functions
#'
#' @noRd 
mod_stock_status_server <- function(
  id, cap_year, cap_month, selected_ecoregion, shared,
  bookmark_qs = reactive(NULL),
  set_subtab   = function(...) {}
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # RESTORE once, defer until after first flush, then push up
observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
  qs <- bookmark_qs()
  wanted <- qs$subtab
  valid  <- c("status_summary", "trends_by_group", "kobe_cld", "status_lookup")
  if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
    session$onFlushed(function() {
      if (utils::packageVersion("bslib") >= "0.5.0") {
        bslib::nav_select(id = "main_tabset", selected = wanted, session = session)
      } else {
        updateTabsetPanel(session, "main_tabset", selected = wanted)
      }
      isolate(set_subtab(wanted))
    }, once = TRUE)
  }
})

# REPORT on user changes, skip initial default
observeEvent(input$main_tabset, {
  set_subtab(input$main_tabset)
}, ignoreInit = TRUE)


    output$ecoregion_label <- renderText({
      req(selected_ecoregion()); paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      paste0("Last update: ", format(Sys.Date(), "%B %d, %Y"))
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

    output$download_clean_status_data <- downloadHandler(
      filename = function() { paste0("status_data_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(shared$clean_status, file, row.names = FALSE) }
    )

    output$status_summary_ges <- renderPlot({
      plot_GES_pies(shared$clean_status, catch_current(), cap_month, cap_year)
    })

    output$download_status_catch_data <- downloadHandler(
      filename = function() { paste0("status_catch_data_", Sys.Date(), ".csv") },
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
      plot_stock_trends(trends_data(), guild, cap_year, cap_month, return_data = FALSE, ecoregion = get_ecoregion_acronym(selected_ecoregion()))
    })

    output$download_trends_data <- downloadHandler(
      filename = function() { paste0("status_trends_data_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(trends_data(), file, row.names = FALSE) }
    )

    output$kobe_cld_slider <- renderUI({
      slider_max <- nrow(kobe_cld_data())
      div(
        id = "custom_slider",
        sliderInput(ns("n_selector"), HTML("Choose <em>n</em> of stocks"),
          min = 1, max = slider_max, value = min(10, slider_max), step = 1
        )
      )
    })

    kobe_cld_data <- reactive({
      if (input$status_kobe_cld_selector == "All") {
        guild <- c("demersal", "pelagic", "crustacean", "benthic", "elasmobranch")
        tmp <- catch_current() %>% dplyr::filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = TRUE)
      } else {
        guild <- input$status_kobe_cld_selector
        tmp <- catch_current() %>% dplyr::filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = TRUE)
      }
    })

    output$status_kobe <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
      req(!is.null(input$n_selector))
      plot_data <- kobe_cld_data() %>% dplyr::slice_max(order_by = total, n = input$n_selector)
      plot_kobe_app(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = FALSE)
    })

    output$status_cld <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
      req(!is.null(input$n_selector))
      plot_data <- kobe_cld_data() %>% dplyr::slice_max(order_by = total, n = input$n_selector)
      plot_CLD_bar_app(plot_data, guild = input$status_kobe_cld_selector, caption = TRUE, cap_year, cap_month, return_data = FALSE)
    })

    output$download_CLD_data <- downloadHandler(
      filename = function() { paste0("status_CLD_data_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(kobe_cld_data(), file, row.names = FALSE) }
    )

    processed_data_reactable <- reactive({
      annex_data <- format_annex_table(shared$clean_status, as.integer(format(Sys.Date(), "%Y")), shared$SID, shared$SAG)

      annex_data_cleaned <- annex_data %>%
        dplyr::mutate(
          icon = paste0("<img src='", paste0("www/fish/", match_stockcode_to_illustration(StockKeyLabel, .)), "' height=30>"),
          StockKeyLabel = paste0("<a href='https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=", AssessmentKey, "&assessmentcomponent=", AssessmentComponent,"' target='_blank'>", StockKeyLabel, ifelse(is.na(AssessmentComponent), "", paste0(" (", AssessmentComponent, ")")), "</a>")
        ) %>%
        dplyr::select(
          "Stock code (component)" = StockKeyLabel,
          " " = icon,
          "Stock Description" = StockKeyDescription,
          "Scientific Name" = SpeciesScientificName,
          "Common Name" = SpeciesCommonName,
          "Fisheries Guild" = FisheriesGuild,
          "Data Category" = DataCategory,
          "Assessment Year" = YearOfLastAssessment,
          "Advice Category" = AdviceCategory,
          "Approach" = lineDescription,
          "Fishing Pressure" = FishingPressure,
          "Stock Size" = StockSize
        ) %>%
        dplyr::mutate(Approach = tolower(Approach)) %>%
        tidyr::pivot_wider(
          names_from = Approach,
          values_from = c(`Fishing Pressure`, `Stock Size`),
          names_glue = "{Approach}_{.value}"
        ) %>%
        dplyr::mutate(
          `MSY Fishing Pressure` = sapply(`maximum sustainable yield_Fishing Pressure`, icon_mapping),
          `MSY Stock Size` = sapply(`maximum sustainable yield_Stock Size`, icon_mapping),
          `PA Fishing Pressure` = sapply(`precautionary approach_Fishing Pressure`, icon_mapping),
          `PA Stock Size` = sapply(`precautionary approach_Stock Size`, icon_mapping)
        ) %>%
        dplyr::select(
          -`maximum sustainable yield_Fishing Pressure`, -`maximum sustainable yield_Stock Size`,
          -`precautionary approach_Fishing Pressure`, -`precautionary approach_Stock Size`
        )
    })

    output$download_status_table <- downloadHandler(
      filename = function() { paste0("status_table_data_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(format_annex_table(shared$clean_status, as.integer(format(Sys.Date(), "%Y")), shared$SID, shared$SAG), file, row.names = FALSE)
      }
    )

    output$stock_status_table_reactable <- renderReactable({
      req(nrow(processed_data_reactable()) != 0)
      reactable::reactable(processed_data_reactable(),
        filterable = TRUE,
        defaultPageSize = 150,
        resizable = TRUE,
        wrap = TRUE,
        bordered = TRUE,
        columns = list(
          "Stock code (component)" = reactable::colDef(html = TRUE, filterable = TRUE),
          " " = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "MSY Fishing Pressure" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "MSY Stock Size" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "PA Fishing Pressure" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "PA Stock Size" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center"))
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
