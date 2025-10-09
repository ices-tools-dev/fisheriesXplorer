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
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    tabsetPanel(
      id = ns("main_tabset"),
      tabPanel(
        title = "Landings", value = "landings",   # <-- add value
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
              radioButtons(ns("landings_layer_selector"), NULL,
                inline = TRUE,
                choices = c("Main landed species" = "Common name",
                            "Fisheries Guild"    = "Fisheries guild",
                            "Country"            = "Country")
              ),
              downloadLink(ns("download_landings_data"),
                HTML(paste0("<span class='hovertext' data-hover='Download landings (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
              )
            ),
            card_body(
              withSpinner(
                uiOutput(ns("landings_layer"), height = "65vh")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Discards", value = "discards",   # <-- add value
        layout_sidebar(
          bg = "white", fg = "black",
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("discards_text"))
          ),
          card(
            card_header(
              "Discard trends",
              downloadLink(ns("download_discard_data"),
                HTML(paste0("<span class='hovertext' data-hover='Download discards (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
              )
            ),
            card_body(
              style = "overflow-y: hidden;",
              withSpinner(plotlyOutput(ns("discard_trends")))
            )
          ),
          card(
            card_body(
              layout_column_wrap(
                width = 1 / 2,
                withSpinner(plotlyOutput(ns("recorded_discards"))),
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
mod_landings_server <- function(
  id, cap_year, cap_month, selected_ecoregion, shared,
  bookmark_qs = reactive(NULL),
  set_subtab   = function(...) {}
) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # RESTORE once, defer until after first flush, then push up
observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
  qs <- bookmark_qs()
  wanted <- qs$subtab
  valid  <- c("landings", "discards")
  if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
    session$onFlushed(function() {
      updateTabsetPanel(session, "main_tabset", selected = wanted)
      isolate(set_subtab(wanted))
    }, once = TRUE)
  }
})

# REPORT on user changes, skip initial default
observeEvent(input$main_tabset, {
  set_subtab(input$main_tabset)
}, ignoreInit = TRUE)


    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      tab <- input$main_tabset
      date_string <- switch(tab,
        "Landings" = "Last data update: October, 2025",
        "Discards" = paste0("Last data update: ", format(Sys.Date(), "%B %d, %Y"))
        # "Last update: December 05, 2024" # default
      )
      date_string
    })

    output$landings_text  <- renderUI({ HTML(select_text(texts,"landings_discards","landings")) })
    output$discards_text  <- renderUI({ HTML(select_text(texts,"landings_discards","discards")) })

    output$landings_layer <- renderUI({
      req(!is.null(input$landings_layer_selector))

      plotting_params <- list()
      plotting_params$landings <- list(
        "Common name" = list("n" = 10, type = "line"),
        "Fisheries guild" = list("n" = 6, type = "line"),
        "Country" = list("n" = 8, type = "line")
      )
      params <- plotting_params$landings[[input$landings_layer_selector]]
      ecoregion <- selected_ecoregion()
      acronym   <- get_ecoregion_acronym(ecoregion)
      rda_path  <- paste0("./data/", acronym, ".rda")
      load(rda_path)
      fig <- plot_catch_trends_plotly(get(get_ecoregion_acronym(ecoregion)), type = input$landings_layer_selector, line_count = params$n,  dataUpdated = "October, 2025", session = session, ecoregion = acronym) #%>%
        #plotly::layout(legend = list(orientation = "v", title = list(text = paste0("<b>", input$landings_layer_selector, "</b>"))))
      
      for (i in 1:length(fig$x$data)) {
        if (!is.null(fig$x$data[[i]]$name)) {
          fig$x$data[[i]]$name <- gsub("\\(", "", strsplit(fig$x$data[[i]]$name, ",")[[1]][1])
        }
      }
      fig
    })

    output$download_landings_data <- downloadHandler(
      filename = function() paste0("landings_trends_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        ecoregion <- selected_ecoregion()
        acronym   <- get_ecoregion_acronym(ecoregion)
        rda_path  <- paste0("./data/", acronym, ".rda")
        load(rda_path)
        write.csv(get(get_ecoregion_acronym(ecoregion)), file, row.names = FALSE)
      }
    )
    year <- 2025
    
    output$discard_trends <- renderPlotly({
      fig2 <- ggplotly(plot_discard_trends_app_plotly(CLD_trends(format_sag(shared$SAG, shared$SID)),
                                                      year, cap_year, cap_month, caption = FALSE))
      for (i in seq_along(fig2$x$data)) {
        if (!is.null(fig2$x$data[[i]]$name)) {
          fig2$x$data[[i]]$name <- gsub("\\(", "", strsplit(fig2$x$data[[i]]$name, ",")[[1]][1])
        }
      }
      fig2
    })

    output$recorded_discards <- renderPlotly({
      catch_trends2 <- CLD_trends(format_sag(shared$SAG, shared$SID)) %>% filter(Discards > 0)      
      plot_discard_current_plotly(catch_trends2, year = year, position_letter = "Stocks with recorded discards (2025)", cap_year = cap_year, cap_month = cap_month)
    })

    output$all_discards <- renderPlotly({      
      plot_discard_current_plotly(CLD_trends(format_sag(shared$SAG, shared$SID)), year = year, position_letter = "All Stocks (2025)", cap_year = cap_year, cap_month = cap_month)
    })

    output$download_discard_data <- downloadHandler(
      filename = function() paste0("discard_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        write.csv(CLD_trends(format_sag(shared$SAG, shared$SID)), file, row.names = FALSE)
      }
    )
  })
}

    
## To be copied in the UI
# mod_landings_ui("landings_1")
    
## To be copied in the server
# mod_landings_server("landings_1")
