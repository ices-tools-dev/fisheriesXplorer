#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),   
    tabsetPanel(
      type = "hidden",
      id = ns("overview"),
      card(
        min_height = "80vh",
        layout_sidebar(
          fillable = TRUE, bg = "white", fg = "black",
          sidebar = sidebar(
            bg = "white", fg = "black",
            width = "50%",
            card(
              min_height = "50vh", height = "80vh", full_screen = TRUE,
              tags$style(
                type = "text/css",
                "#staticMap1 {margin-left:auto; margin-right:auto; margin-bottom:auto; max-width:97%; height:auto;}"
              ),
              withSpinner(uiOutput(ns("staticMap1"), width = "100%"))
            )
          ),
          # ------ make these bookmarkable ------
          tabsetPanel(
            id = ns("tabs_overview"), # <-- NEW
            tabPanel("Executive Summary",
              value = "exec_summary",
              card(uiOutput(ns("executive_summary")))
            ),
            tabPanel("Introduction",
              value = "introduction",
              card(uiOutput(ns("introduction")))
            ),
            tabPanel("Who is Fishing",
              value = "who_is_fishing",
              card(uiOutput(ns("who_is_fishing")))
            )
          )
          # ------------------------------------
        )
      )
    )
  )
}

       
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(
    id,
    selected_ecoregion,
    bookmark_qs = reactive(NULL),
    set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # RESTORE once, defer until after first flush, then push up
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("exec_summary", "introduction", "who_is_fishing")
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "tabs_overview", selected = wanted)
          isolate(set_subtab(wanted)) # one-arg setter
        }, once = TRUE)
      }
    })

    # REPORT on user changes, skip initial default
    observeEvent(input$tabs_overview,
      {
        set_subtab(input$tabs_overview) # one arg only
      },
      ignoreInit = TRUE
    )


    output$ecoregion_label <- renderUI({
      req(selected_ecoregion())
      tags$span(tags$b("Ecoregion:"), " ", selected_ecoregion())
    })

    output$current_date <- renderUI({
      tagList(
        tags$span(tags$b("Last text update:"), " December 05, 2024"),
        tags$span(" \u00B7 "),
        mod_glossary_modal_ui(ns("overview_glossary"), link_text = "Glossary")
      )
    })

    # --- Glossary module server (Overview terms)
    # assumes glossary_for("Overview") returns your ICES-sourced list
    mod_glossary_modal_server(
      "overview_glossary",
      terms = reactive(glossary_for("Overview")),
      title = "Overview \u2014 Glossary",
      size  = "l"
    )

    output$staticMap1 <- renderUI({
      ecoregion <- get_ecoregion_acronym(selected_ecoregion())
      file_name <- paste0(ecoregion, ".jpg")
      src_url <- file.path("www", file_name)
      tags$img(
        id = ns("staticMap1"),
        src = src_url,
        style = "width: 100%; cursor: pointer;",
        onclick = "toggleFullScreen(this)"
      )
    })

    output$executive_summary <- renderUI({
      HTML(select_text(texts, paste0("overview_", get_ecoregion_acronym(selected_ecoregion())), "executive_summary"))
    })
    output$introduction <- renderUI({
      HTML(select_text(texts, paste0("overview_", get_ecoregion_acronym(selected_ecoregion())), "introduction"))
    })
    output$who_is_fishing <- renderUI({
      HTML(select_text(texts, paste0("overview_", get_ecoregion_acronym(selected_ecoregion())), "who_is_fishing"))
    })
  })
}
