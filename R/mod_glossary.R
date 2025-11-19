
#' Floating, non-blocking glossary panel (UI)
#'
#' Adds a right-side **floating panel** that stays hidden until opened via a
#' “Glossary” link. The panel overlays the app **without blocking** interaction
#' and contains a searchable/filterable **reactable** table of glossary terms.
#' The panel body scrolls vertically; the header remains fixed.
#'
#' This UI relies on CSS rules for the `.glossary-float` container
#' (recommended to place in `www/styles.css`) to control responsive sizing and
#' typography. By default, the panel is closed on load (`display: none`) and is
#' opened by the inline JS in this UI (set to `display: flex`).
#'
#' @param id Module ID (string). Passed to \code{\link[shiny]{NS}} for namespacing.
#' @param link_text Text shown in the trigger link (default: "Glossary").
#' @param panel_title Title shown in the panel header (default: "Glossary").
#'
#' @details
#' **Styling**
#'
#' Add responsive styles for the container to `www/styles.css`, e.g.:
#' \preformatted{
#' .glossary-float{ display:none; position:fixed; right:20px; top:80px; z-index:2000;
#'   background:#fff; border:1px solid #ddd; border-radius:8px; box-shadow:0 8px 24px rgba(0,0,0,.2);
#'   overflow:hidden; display:flex; flex-direction:column;
#'   width:clamp(360px, 38vw, 780px); max-height:42vh; font-size:clamp(12px,1.4vw,14px);
#' }
#' @media (max-width:576px){ .glossary-float{ right:8px; top:8px; width:min(96vw,560px); max-height:70vh; } }
#' }
#'
#' **Behavior**
#'
#' - Panel opens with a click on the “Glossary” link and closes with the “×” or the
#'   Escape key.
#' - The app remains usable, including scrolling, while the panel is open.
#' - The panel can be repositioned by dragging the header (basic mouse-drag logic).
#'
#' If you prefer zero inline JS, you can switch to a small external script that toggles
#' an `is-open` class via `data-g-open`/`data-g-close` attributes. (See notes in server docs.)
#'
#' @return A UI fragment (tagList) to include in your Shiny UI.
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   mod_glossary_float_ui("gloss", link_text = "Glossary", panel_title = "Glossary")
#' )
#' server <- function(input, output, session) {
#'   mod_glossary_float_server("gloss", terms = reactive({
#'     data.frame(
#'       term = c("Catch", "Landings"),
#'       definition = c("Total quantity taken...", "Portion of the catch brought ashore."),
#'       source = c("https://www.ices.dk/...", "https://www.ices.dk/...")
#'     )
#'   }))
#' }
#' shinyApp(ui, server)
#' }
#'
#' @export
mod_glossary_float_ui <- function(id, link_text = "Glossary", panel_title = "Glossary") {
  ns <- NS(id)

  tagList(
    actionLink(ns("open"), label = tagList(icon("book"), link_text),
               class = "glossary-link", style = "margin-left:.5rem;"),

    # Panel starts hidden; external CSS (.glossary-float) handles size/looks
    tags$div(
      id = ns("panel"),
      class = "glossary-float",
      style = "display:none;",
      div(
        class = "g-header",
        span(class = "g-title", panel_title),
        tags$button(type = "button", class = "g-close-btn",
                    onclick = paste0("document.getElementById('", ns("panel"), "').style.display='none';"),
                    HTML("&times;"))
      ),
      div(class = "g-body", reactable::reactableOutput(ns("tbl")))
    ),

    
    tags$script(HTML(paste0("
      (function(){
        var openEl  = document.getElementById('", ns("open"), "');
        var panelEl = document.getElementById('", ns("panel"), "');
        if(openEl && panelEl){
          openEl.addEventListener('click', function(e){ e.preventDefault(); panelEl.style.display = 'flex'; });
          var header = panelEl.querySelector('.g-header');
          var isDown=false, startX=0, startY=0, startRight=0, startTop=0;
          if(header){
            header.addEventListener('mousedown', function(ev){
              isDown = true; startX = ev.clientX; startY = ev.clientY;
              var cs = window.getComputedStyle(panelEl); startRight = parseInt(cs.right,10) || 0; startTop = parseInt(cs.top,10) || 0;
              ev.preventDefault();
            });
            document.addEventListener('mousemove', function(ev){
              if(!isDown) return; var dx = ev.clientX - startX; var dy = ev.clientY - startY;
              panelEl.style.right = (startRight - dx) + 'px'; panelEl.style.top = (startTop + dy) + 'px';
            });
            document.addEventListener('mouseup', function(){ isDown=false; });
          }
          document.addEventListener('keydown', function(ev){ if(ev.key === 'Escape') panelEl.style.display = 'none'; });
        }
      })();
    ")))
  )
}



#' Floating, non-blocking glossary panel (server)
#'
#' Server logic for the floating glossary panel. Renders a searchable/filterable
#' \pkg{reactable} with two columns: **Term** and **Definition**. If a `source`
#' URL is present, a small \code{[source]} hyperlink is appended after the definition.
#'
#' @param id Module ID (string). Must match the \code{id} used in
#'   \code{mod_glossary_float_ui()}.
#' @param terms A glossary source. One of:
#'   \itemize{
#'     \item a \code{data.frame} with columns \code{term}, \code{definition}, and optional \code{source};
#'     \item a list of items \code{list(term=..., def=..., source=...)};
#'     \item a \code{reactive()} or \code{function()} that returns one of the above.
#'   }
#'
#' @details
#' The table is rendered even while the panel is hidden by setting:
#' \code{outputOptions(output, "tbl", suspendWhenHidden = FALSE)}.
#'
#' **Expected columns**
#'
#' - \strong{term}: character — the glossary term (required)
#' - \strong{definition}: character/HTML — the definition (required)
#' - \strong{source}: character — URL to the source (optional)
#'
#' **External assets (recommended)**
#'
#' Add responsive CSS for `.glossary-float`, `.g-header`, `.g-body`, etc. to
#' `www/styles.css` and include it globally (e.g., via
#' `golem_add_external_resources()`). If you prefer **no inline JS**, supply
#' a small `www/glossary.js` that toggles an `.is-open` class using
#' `data-g-open` / `data-g-close` attributes; then remove the inline `tags$script`
#' block from the UI and add:
#' \preformatted{
#' tags$script(src = "www/glossary.js")
#' }
#'
#' @return None. This is called for its side effects (registers outputs).
#'
#' @examples
#' \dontrun{
#' # Minimal usage inside a Shiny app:
#' ui <- fluidPage(mod_glossary_float_ui("gloss"))
#' server <- function(input, output, session) {
#'   mod_glossary_float_server("gloss", terms = reactive({
#'     data.frame(
#'       term = c("Catch", "Landings"),
#'       definition = c("Total quantity taken...", "Portion of the catch brought ashore."),
#'       source = c("https://www.ices.dk/...", "https://www.ices.dk/...")
#'     )
#'   }))
#' }
#' shinyApp(ui, server)
#' }
#'
#' @importFrom htmltools htmlEscape
#' @importFrom shiny moduleServer NS reactive
#' @export
mod_glossary_float_server <- function(id, terms) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Normalize inputs -> data.frame(term, definition, source)
    resolve_df <- function(x) {
      if (is.function(x)) x <- x()
      if (shiny::is.reactive(x)) x <- x()
      if (is.data.frame(x)) {
        n <- tolower(gsub("[^a-z0-9]+", "_", names(x))); names(x) <- n
        if (!"term" %in% names(x) || !"definition" %in% names(x))
          stop("Glossary needs columns 'term' and 'definition'.")
        if (!"source" %in% names(x)) x$source <- ""
        return(x[, intersect(c("term","definition","source"), names(x)), drop = FALSE])
      }
      if (is.list(x) && length(x) && !is.null(x[[1]]$term)) {
        term <- vapply(x, function(it) if (!is.null(it$term)) it$term else "", character(1))
        def  <- vapply(x, function(it) if (!is.null(it$def)) it$def else if (!is.null(it$definition)) it$definition else "", character(1))
        src  <- vapply(x, function(it) if (!is.null(it$source)) it$source else "", character(1))
        return(data.frame(term = term, definition = def, source = src, stringsAsFactors = FALSE))
      }
      data.frame(term = character(), definition = character(), source = character(), stringsAsFactors = FALSE)
    }

    add_link <- function(txt, url) {
      if (is.na(url) || !nzchar(url)) return(txt)
      paste0(txt, " <span class='src-link'><a href='",
             htmltools::htmlEscape(url), "' target='_blank' rel='noopener'>[source]</a></span>")
    }

    # Always-available data for the table
    data_r <- reactive({
      df <- resolve_df(terms)
      # if (nrow(df)) df <- df[order(tolower(df$term)), , drop = FALSE]
      df
    })

    output$tbl <- reactable::renderReactable({
      df <- data_r()
      validate(need(nrow(df) > 0, "No glossary terms found."))
      reactable::reactable(
        data.frame(
          Term = df$term,
          Definition = mapply(add_link, df$definition, df$source, USE.NAMES = FALSE),
          stringsAsFactors = FALSE
        ),
        searchable = TRUE, 
        filterable = TRUE, 
        striped = TRUE, 
        compact = TRUE, 
        highlight = TRUE,
        # defaultSorted = list(Term = "asc"), 
        defaultPageSize = 20, 
        showPageSizeOptions = TRUE,
        minRows = 5, 
        resizable = TRUE,
        columns = list(
          Term = reactable::colDef(minWidth = 100),
          Definition = reactable::colDef(
            html = TRUE, minWidth = 350
            
          )
        )
      )
    })
    outputOptions(output, "tbl", suspendWhenHidden = FALSE)
  })
}
