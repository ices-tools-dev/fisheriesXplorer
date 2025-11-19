#' Overview UI module
#'
#' This module UI creates the main \emph{Overview} section of the app, showing
#' a static ecoregion map alongside three text panels: Executive Summary,
#' Introduction, and Who is Fishing. It is typically the entry point for an
#' ecoregion, combining a visual overview with narrative context.
#'
#' @param id A character string used as the module namespace. Passed to
#'   \code{shiny::NS()} and used to namespace all UI elements within the
#'   module.
#'
#' @details
#' The layout consists of:
#' \itemize{
#'   \item A flexible header created by \code{mod_flex_header_ui()} showing
#'     the current ecoregion label and last update date.
#'   \item A hidden \code{tabsetPanel} wrapper (\code{id = "overview"}) that
#'     contains a \code{card} with a \code{layout_sidebar}:
#'     \itemize{
#'       \item Left-hand sidebar: a tall card containing a static map
#'         output (\code{staticMap1}) wrapped in
#'         \code{shinycssloaders::withSpinner()}.
#'       \item Right-hand pane: a \code{tabsetPanel} (\code{tabs_overview})
#'         with three tabs:
#'         \item \strong{Executive Summary} (\code{"exec_summary"}):
#'           \code{executive_summary} UI output.
#'         \item \strong{Introduction} (\code{"introduction"}):
#'           \code{introduction} UI output.
#'         \item \strong{Who is Fishing} (\code{"who_is_fishing"}):
#'           \code{who_is_fishing} UI output.
#'     }
#' }
#'
#' The corresponding server logic, including bookmarking of
#' \code{tabs_overview}, is implemented in \code{mod_overview_server()}.
#'
#' @return A \code{shiny.tag.list} representing the overview UI, suitable for
#'   inclusion in a Shiny application.
#'
#' @importFrom shiny NS tagList tabsetPanel tabPanel uiOutput
#' @importFrom bslib layout_sidebar sidebar card
#' @importFrom shinycssloaders withSpinner
#'
#' @export
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
              withSpinner(uiOutput(ns("staticMap1"), width = "100%"))
            )
          ),
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
        )
      )
    )
  )
}

       
#' Server logic for overview module
#'
#' This module server powers the \emph{Overview} section of the app. It
#' manages bookmarking of the overview subtabs, renders the ecoregion header
#' (with last text update and glossary link), displays a static ecoregion
#' map image, and fills the three main text sections (Executive Summary,
#' Introduction, Who is Fishing) from the internal \code{texts} table.
#'
#' @param id Module id, matching the id used in \code{mod_overview_ui()}.
#' @param selected_ecoregion A reactive expression returning the currently
#'   selected ecoregion name (character). Used for header labels, map file
#'   selection, and lookup of overview texts.
#' @param bookmark_qs A reactive returning a list of bookmark query-string
#'   parameters (including \code{subtab}) or \code{NULL}. Used to restore
#'   the selected overview subtab (\code{"exec_summary"}, \code{"introduction"},
#'   or \code{"who_is_fishing"}) on load.
#' @param set_subtab A callback function that takes a single character
#'   argument (the current subtab id). Used to inform the parent app of
#'   subtab changes for bookmarking or other side effects.
#'
#' @details
#' The module:
#' \itemize{
#'   \item Restores the active overview tab from \code{bookmark_qs()} on first
#'     load and reports it via \code{set_subtab()}.
#'   \item Observes changes to \code{input$tabs_overview} (ignoring the
#'     initial default) and calls \code{set_subtab()} whenever the user
#'     switches between "Executive Summary", "Introduction", and
#'     "Who is Fishing".
#'   \item Renders a header that includes:
#'     \itemize{
#'       \item The current ecoregion label (\code{ecoregion_label}).
#'       \item A fixed "Last text update" date string.
#'       \item A floating glossary trigger, wired up via
#'         \code{mod_glossary_float_server()} and populated from
#'         \code{texts} with type \code{"glossary"}.
#'     }
#'   \item Renders a static ecoregion map image (\code{staticMap1}) from the
#'     \code{www} directory, using the ecoregion acronym
#'     (\code{get_ecoregion_acronym()}) to build the filename, and an
#'     \code{onclick} handler (\code{toggleFullScreen}) for full-screen view.
#'   \item Fills the three overview text sections by calling
#'     \code{select_text()} on \code{texts} with type
#'     \code{paste0("overview_", acronym)} and the appropriate section key
#'     (\code{"executive_summary"}, \code{"introduction"},
#'     \code{"who_is_fishing"}).
#' }
#'
#' @return No direct return value; called for its side effects of registering
#'   outputs and observers in a Shiny server context.
#'
#' @importFrom shiny moduleServer observeEvent updateTabsetPanel renderUI
#'   reactive req tags tagList
#'
#' @export
mod_overview_server <- function(
    id,
    selected_ecoregion,
    bookmark_qs = reactive(NULL),
    set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

     ################################## bookmarking #########################################
    # This module participates in the global bookmarking via two hooks:
    # - `bookmark_qs`: a reactive list provided by the main server with the
    #   parsed query-string (including $subtab).
    # - `set_subtab()`: a callback into the main server to report *user-driven*
    #   changes of the internal tab state.
    #
    # Restore path:
    # - On first non-null bookmark_qs(), we read the desired subtab.
    # - If it is valid for this module, we wait for the UI to flush, then
    #   select the corresponding tabsetPanel value.
    # - We also call set_subtab() once so the main server can see that the
    #   module has accepted the requested subtab.
    #
    # Report path:
    # - Any later changes to input$tabs_overview (ignoring the initial) are
    #   forwarded upstream via set_subtab(), so the main server can update
    #   the URL hash / desired() state.
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("exec_summary", "introduction", "who_is_fishing")
      # Only act if a valid subtab was requested
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          # Drive the internal tabsetPanel to the requested subtab
          updateTabsetPanel(session, "tabs_overview", selected = wanted)
          # Drive the internal tabsetPanel to the requested subtab
          isolate(set_subtab(wanted)) 
        }, once = TRUE)
      }
    })

    # REPORT on user changes, skip initial default
    observeEvent(input$tabs_overview,
      {
        # Whenever the user clicks a different subtab, tell the main server.
        # The parent can then update the URL and/or its own `desired()` state.
        set_subtab(input$tabs_overview) # one arg only
      },
      ignoreInit = TRUE
    )

    ################################## header + glossary #########################################
    output$ecoregion_label <- renderUI({
      req(selected_ecoregion())
      tags$span(tags$b("Ecoregion:"), " ", selected_ecoregion())
    })

    output$current_date <- renderUI({
      tagList(
        tags$span(tags$b("Last text update:"), " December 05, 2024"),
        tags$span(" \u00B7 "),
        mod_glossary_float_ui(ns("app_glossary"), link_text = "Glossary", panel_title = "Glossary")
      )
    })
   mod_glossary_float_server(
     "app_glossary",
     terms = reactive({
       df <- select_text(texts, "glossary", NULL) # your texts.rda table
       df[, intersect(names(df), c("term", "definition", "source")), drop = FALSE]
     })
   )
    
    #################################### Ecoregion map #########################################
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
    ################################## Text sections #########################################
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
