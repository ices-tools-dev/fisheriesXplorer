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
    #  div(
    #     style = "display: flex; justify-content: space-between; align-items: center;
    #          padding: 10px; font-weight: bold; font-size: 1.2em; margin-bottom: 0px;",
    #     span(textOutput(ns("ecoregion_label"))),
    #     span(textOutput(ns("current_date")))
    #   ),
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    tabsetPanel(
      id = ns("main_tabset"), 
      tabPanel(
        "Landings",
        layout_sidebar(
          bg = "white", fg = "black",
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("landings_text"))
          ),
          card(
            height = "85vh",
            card_header(
              div(
                style = "margin-left: 12px;",
                radioButtons(ns("landings_layer_selector"), NULL,
                  inline = T,
                  choices = c("Main landed species" = "COMMON_NAME", "Guild" = "GUILD", "Country" = "COUNTRY")
                )
              )
            ),
            card_body(withSpinner(
              plotlyOutput(ns("landings_layer"), height = "65vh")
            ))
          )
        )
      ),
      tabPanel(
        "Discards",
        layout_sidebar(
          bg = "white", fg = "black",
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("discards_text"))
          ),
          card(
            height = "95vh",
            card_body(
              withSpinner(plotlyOutput(ns("combined_discards_plot"), width = "100%", height = "80vh"))
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
mod_landings_server <- function(id, cap_year, cap_month, selected_ecoregion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    # output$current_date <- renderText({
    #   "Last update: December 05, 2024" # e.g., "May 26, 2025"
    # })
    output$current_date <- renderText({
  tab <- input$main_tabset

  date_string <- switch(tab,
    "Landings" = "Last update: December 05, 2024",
    "Discards" = paste0("Last update: ", format(Sys.Date(), "%B %d, %Y"))
    # "Last update: December 05, 2024" # default
  )

  date_string
})

    SID <- reactive({
      # dat <- prepare_ices_stock_status(clean_status)
      getSID(year = 2024, EcoR = selected_ecoregion())
      # format_sag_status_new(status)
    })

    SAG <- reactive({
      # getSAG_ecoregion(2024, selected_ecoregion(), SID())
      getSAG_ecoregion_new(SID()$AssessmentKey)
      # getSAG_ecoregion_new(SID()$StockKeyLabel, SID()$YearOfLastAssessment)
    })
    output$landings_text <- renderUI({
      HTML(select_text(texts, "landings_discards", "landings"))
    })

    output$discards_text <- renderUI({
      HTML(select_text(texts, "landings_discards", "discards"))
    })

    output$landings_layer <- renderPlotly({
      req(!is.null(input$landings_layer_selector))

      plotting_params <- list()
      plotting_params$landings <- list(
        "COMMON_NAME" = list("n" = 10, type = "line"),
        "GUILD" = list("n" = 6, type = "line"),
        "COUNTRY" = list("n" = 9, type = "area")
      )

      params <- plotting_params$landings[[input$landings_layer_selector]]

      ecoregion <- selected_ecoregion()
      acronym <- get_ecoregion_acronym(ecoregion)

      # Load the corresponding .rda file
      rda_path <- paste0("./data/", acronym, ".rda")
      load(rda_path)
      fig <- ggplotly(plot_catch_trends_app_new(get(get_ecoregion_acronym(ecoregion)), type = input$landings_layer_selector, line_count = params$n, plot_type = params$type, official_catches_year = as.numeric(cap_year))) %>%
        plotly::layout(legend = list(orientation = "v", title = list(text = paste0("<b>", input$landings_layer_selector, "</b>"))))

      for (i in 1:length(fig$x$data)) {
        if (!is.null(fig$x$data[[i]]$name)) {
          fig$x$data[[i]]$name <- gsub("\\(", "", str_split(fig$x$data[[i]]$name, ",")[[1]][1])
        }
      }
      fig
    })

    year <- 2024

    # output$discard_trends <- renderPlotly({
    #   fig2 <- ggplotly(plot_discard_trends_app_plotly(CLD_trends(format_sag(SAG(), SID())), year, cap_year , cap_month, caption = F))
    #   for (i in 1:length(fig2$x$data)) {
    #     if (!is.null(fig2$x$data[[i]]$name)) {
    #       fig2$x$data[[i]]$name <- gsub("\\(", "", str_split(fig2$x$data[[i]]$name, ",")[[1]][1])
    #     }
    #   }
    #   fig2
    # })
    # output$recorded_discards <- renderPlotly({
    #   catch_trends2 <- CLD_trends(format_sag(SAG(), SID())) %>% filter(Discards > 0)
    #   # ggplotly(plot_discard_current(CLD_trends(format_sag(SAG(), SID())), year, cap_year , cap_month, position_letter = ""))
    #   plot_discard_current_plotly(catch_trends2, year = year, cap_year = cap_year, cap_month = cap_month)
    # })

    # output$all_discards <- renderPlotly({
    #   # dat <- plot_discard_current_plotly(CLD_trends(format_sag(SAG(), SID())), year, cap_year = cap_year , cap_month = cap_month, return_data = TRUE)
    #   # ggplotly(plot_discard_current_order(CLD_trends(format_sag(SAG(), SID())), year-1, dat, cap_year , cap_month, position_letter = ""))
    #   plot_discard_current_plotly(CLD_trends(format_sag(SAG(), SID())), year = year, cap_year = cap_year, cap_month = cap_month)
    # })
    output$combined_discards_plot <- renderPlotly({
      # Prepare top plot
      top_plot <- ggplotly(
        plot_discard_trends_app_plotly(
          CLD_trends(format_sag(SAG(), SID())),
          year, cap_year, cap_month,
          caption = FALSE
        )
      )
      

      # # Clean top plot trace names
      # for (i in seq_along(top_plot$x$data)) {
      #   if (!is.null(top_plot$x$data[[i]]$name)) {
      #     top_plot$x$data[[i]]$name <- gsub("\\(", "", str_split(top_plot$x$data[[i]]$name, ",")[[1]][1])
      #   }
      # }

      # Bottom left and right plots
      bottom_left <- plot_discard_current_plotly(
        CLD_trends(format_sag(SAG(), SID())) %>% filter(Discards > 0),
        year = year, cap_year = cap_year, cap_month = cap_month
      )

      bottom_right <- plot_discard_current_plotly(
        CLD_trends(format_sag(SAG(), SID())),
        year = year, caption = FALSE
      )

      # Combine all in subplot
      combined <- plotly::subplot(
        top_plot,
        plotly::subplot(bottom_left, bottom_right, nrows = 1, margin = 0.05, titleX = TRUE, titleY = TRUE),
        nrows = 2,
        heights = c(0.5, 0.5),
        shareX = FALSE,
        shareY = FALSE,
        margin = 0.1
      )

      # Add subplot titles via layout annotations
      combined <- combined %>% plotly::layout(
        # title = list(text = "Discard Overview", x = 0, xanchor = "left"),
        annotations = list(
          list(
            text = "Discard Trends",
            x = 0.5,
            y = 1.05,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "black")
          ),
          list(
            text = "Recorded Discards",
            x = 0.22,
            y = 0.45,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "black")
          ),
          list(
            text = "All Discards",
            x = 0.78,
            y = 0.45,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "black")
          ),
          list(
            text = "Discards and Landings (thousand tonnes)",
            x = 0.5,
            y = -0.08,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,  
            font = list(size = 12, color = "black")
          ),
          list(
            text = "Years",
            x = 0.5,
            y = 0.52,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,  
            font = list(size = 12, color = "black")
          )
        ),
        showlegend = TRUE        
      )
    })
  })
}
    
## To be copied in the UI
# mod_landings_ui("landings_1")
    
## To be copied in the server
# mod_landings_server("landings_1")
