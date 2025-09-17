#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    tabsetPanel(
      type = "hidden",
      id = ns("overview"),
      card(min_height = "80vh",
        layout_sidebar(
          fillable = TRUE, bg = "white", fg = "black",
          sidebar = sidebar(bg = "white", fg = "black",
            width = "50%",
            card(min_height = "50vh", height = "80vh", full_screen = TRUE,
              tags$style(type = "text/css",
                "#staticMap1 {margin-left:auto; margin-right:auto; margin-bottom:auto; max-width:97%; height:auto;}"),
              withSpinner(uiOutput(ns("staticMap1"), width = "100%"))
            )
          ),
          # ------ make these bookmarkable ------
          tabsetPanel(
            id = ns("tabs_overview"),                       # <-- NEW
            tabPanel("Executive Summary", value = "exec_summary",
              card(uiOutput(ns("executive_summary")))
            ),
            tabPanel("Introduction", value = "introduction",
              card(uiOutput(ns("introduction")))
            ),
            tabPanel("Who is Fishing", value = "who_is_fishing",
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
  bookmark_qs = reactive(NULL),   # <-- NEW
  set_subtab   = function(...) {} # <-- NEW
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- Restore subtab from URL (once)
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      if (!is.null(qs$subtab) && nzchar(qs$subtab)) {
        updateTabsetPanel(session, "tabs_overview", selected = qs$subtab)
      }
    })

    # --- Report subtab upward (initial + on change)
    observeEvent(input$tabs_overview, {
      set_subtab(input$tabs_overview)   # "exec_summary" | "introduction" | "who_is_fishing"
    }, ignoreInit = FALSE)

    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      "Last update: December 05, 2024"
    })

    output$staticMap1 <- renderUI({
      ecoregion <- get_ecoregion_acronym(selected_ecoregion())
      file_name <- paste0(ecoregion, ".jpg")
      src_url   <- file.path("www", file_name)
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
