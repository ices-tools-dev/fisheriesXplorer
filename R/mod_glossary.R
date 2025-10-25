# # mod_glossary_modal.R
# mod_glossary_modal_ui <- function(id, link_text = "Glossary") {
#   ns <- NS(id)
#   actionLink(
#     ns("open"),
#     label = tagList(icon("book"), link_text),
#     class = "glossary-link",
#     style = "margin-left:.5rem;"
#   )
# }

# mod_glossary_modal_server <- function(id, terms, title = "Glossary", size = "l") {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     # Accepts: data.frame(term, definition[, source]) OR list of {term, def[, source]} OR reactive()/function
#     resolve_df <- function(x) {
#       if (is.function(x)) x <- x()                 # works for reactive(...)
#       if (shiny::is.reactive(x)) x <- x()
#       if (is.data.frame(x)) {
#         n <- tolower(gsub("[^a-z0-9]+", "_", names(x)))
#         names(x) <- n
#         if (!"term" %in% names(x) || !"definition" %in% names(x)) {
#           stop("Glossary data needs columns 'term' and 'definition'.")
#         }
#         if (!"source" %in% names(x)) x$source <- ""
#         return(x[, intersect(c("term","definition","source"), names(x)), drop = FALSE])
#       }
#       if (is.list(x) && length(x) && !is.null(x[[1]]$term)) {
#         # list of {term, def[, source]}
#         term <- vapply(x, function(it) if (!is.null(it$term)) it$term else "", character(1))
#         def  <- vapply(x, function(it) if (!is.null(it$def)) it$def else if (!is.null(it$definition)) it$definition else "", character(1))
#         src  <- vapply(x, function(it) if (!is.null(it$source)) it$source else "", character(1))
#         return(data.frame(term = term, definition = def, source = src, stringsAsFactors = FALSE))
#       }
#       data.frame(term = character(), definition = character(), source = character(), stringsAsFactors = FALSE)
#     }

#     make_table_df <- function(df) {
#       # Append a small "[link]" after the definition when source is present
#       add_link <- function(txt, url) {
#         if (is.na(url) || identical(url, "") || !nzchar(url)) return(txt)
#         paste0(
#           txt,
#           " <span class='src-link'><a href='", htmltools::htmlEscape(url),
#           "' target='_blank' rel='noopener'>[source]</a></span>"
#         )
#       }
#       Definition <- mapply(add_link, df$definition, df$source, USE.NAMES = FALSE)
#       data.frame(
#         Term = df$term,
#         Definition = Definition,
#         stringsAsFactors = FALSE
#       )
#     }

#     observeEvent(input$open, {
#       if (!requireNamespace("reactable", quietly = TRUE)) {
#         showModal(modalDialog(
#           title = title, easyClose = TRUE, size = size,
#           footer = modalButton("Close"),
#           div("Please install the 'reactable' package to show the glossary as a table: ",
#               code("install.packages('reactable')"))
#         ))
#         return()
#       }

#       df_raw <- resolve_df(terms)
#       # sort A–Z
#       if (nrow(df_raw)) df_raw <- df_raw[order(tolower(df_raw$term)), , drop = FALSE]
#       df_tbl <- make_table_df(df_raw)

#       output$tbl <- reactable::renderReactable({
#         reactable::reactable(
#           df_tbl,
#           searchable = TRUE,
#           filterable = TRUE,
#           striped = TRUE,
#           compact = TRUE,
#           highlight = TRUE,
#           defaultSorted = list(Term = "asc"),
#           defaultPageSize = 10,
#           showPageSizeOptions = TRUE,
#           minRows = 5,
#           resizable = TRUE,
#           columns = list(
#             Term = reactable::colDef(minWidth = 180),
#             Definition = reactable::colDef(
#               html = TRUE,
#               minWidth = 480
#             )
#           )
#         )
#       })

#       showModal(
#         modalDialog(
#           title = title,
#           easyClose = TRUE,
#           size = size,
#           footer = modalButton("Close"),
#           tags$style(HTML(sprintf("
#             /* small, subtle link after definitions */
#             .glossary-reactable .src-link { font-size: 0.85em; opacity: 0.85; margin-left: .35rem; }
#             .glossary-reactable a { text-decoration: none; }
#             /* keep modal scrollable */
#             #%s_tbl { max-height: 60vh; overflow-y: auto; }
#           ", ns("tbl")))),
#           div(class = "glossary-reactable",
#               reactable::reactableOutput(ns("tbl"))
#           )
#         )
#       )
#     })
#   })
# }
# # mod_glossary_modal.R
# # Action link -> non-blocking right-side offcanvas with reactable table

# # mod_glossary_modal.R
# # Action link -> non-blocking right-side offcanvas with reactable table
# # Requires Bootstrap 5 (ui theme = bslib::bs_theme(version = 5))

# mod_glossary_modal_ui <- function(id, link_text = "Glossary", panel_title = "Glossary") {
#   ns <- NS(id)

#   tagList(
#     # One-time JS handler to open the offcanvas
#     singleton(tags$head(tags$script(HTML("
#       (function(){
#         if (window.__glossaryOffcanvasInit) return;
#         window.__glossaryOffcanvasInit = true;
#         Shiny.addCustomMessageHandler('open-offcanvas', function(x){
#           var el = document.getElementById(x.id);
#           if (!el) return;
#           if (window.bootstrap && bootstrap.Offcanvas) {
#             var inst = bootstrap.Offcanvas.getOrCreateInstance(el, { backdrop: false, scroll: true });
#             inst.show();
#           } else {
#             // Fallback if Bootstrap JS isn't present
#             el.classList.add('show');
#             el.style.visibility = 'visible';
#           }
#         });
#       })();
#     ")))),
#     # Trigger link (keeps your original API)
#     actionLink(
#       ns("open"),
#       label = tagList(icon("book"), link_text),
#       class = "glossary-link",
#       style = "margin-left:.5rem;"
#     ),

#     # Offcanvas (mounted once; opened via JS message)
#     tags$div(
#       class = "offcanvas offcanvas-end glossary-canvas",
#       tabindex = "-1",
#       id = ns("panel"),
#       `aria-labelledby` = ns("label"),
#       `data-bs-backdrop` = "false",  # non-blocking
#       `data-bs-scroll`   = "true",   # keep page scrollable
#       tags$div(
#         class = "offcanvas-header",
#         tags$h5(class = "offcanvas-title", id = ns("label"), panel_title),
#         tags$button(type = "button", class = "btn-close",
#                     `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")
#       ),
#       tags$div(
#         class = "offcanvas-body",
#         div(class = "glossary-body",
#             reactable::reactableOutput(ns("tbl")))
#       )
#     ),

#     # CSS (no sprintf; safe with %)
#     tags$style(HTML("
#       .offcanvas.glossary-canvas { width: min(720px, 92vw); z-index: 1045; }
#       .offcanvas.glossary-canvas .offcanvas-body { padding: 0; }
#       .glossary-body { padding: 12px 12px 16px 12px; max-height: calc(100vh - 58px); overflow-y: auto; }
#       .glossary-reactable .src-link { font-size: 0.85em; opacity: 0.85; margin-left: .35rem; }
#       .glossary-reactable a { text-decoration: none; }
#       /* Fallback slide-in if BS5 JS is missing */
#       .offcanvas.glossary-canvas {
#         position: fixed; top: 0; right: 0; bottom: 0; background: #fff;
#         visibility: hidden; transform: translateX(100%); transition: transform .25s ease, visibility .25s ease;
#       }
#       .offcanvas.glossary-canvas.show { visibility: visible; transform: translateX(0); }
#     "))
#   )
# }

# mod_glossary_modal_server <- function(id, terms, title = "Glossary", size = "l") {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     # Normalize inputs -> data.frame(term, definition, source)
#     resolve_df <- function(x) {
#       if (is.function(x)) x <- x()
#       if (shiny::is.reactive(x)) x <- x()
#       if (is.data.frame(x)) {
#         n <- tolower(gsub("[^a-z0-9]+", "_", names(x))); names(x) <- n
#         if (!"term" %in% names(x) || !"definition" %in% names(x))
#           stop("Glossary data needs columns 'term' and 'definition'.")
#         if (!"source" %in% names(x)) x$source <- ""
#         return(x[, intersect(c("term","definition","source"), names(x)), drop = FALSE])
#       }
#       if (is.list(x) && length(x) && !is.null(x[[1]]$term)) {
#         term <- vapply(x, function(it) it$term %||% "", character(1))
#         def  <- vapply(x, function(it) (it$def %||% it$definition) %||% "", character(1))
#         src  <- vapply(x, function(it) it$source %||% "", character(1))
#         return(data.frame(term = term, definition = def, source = src, stringsAsFactors = FALSE))
#       }
#       data.frame(term = character(), definition = character(), source = character(), stringsAsFactors = FALSE)
#     }

#     make_table_df <- function(df) {
#       add_link <- function(txt, url) {
#         if (is.na(url) || !nzchar(url)) return(txt)
#         paste0(txt, " <span class='src-link'><a href='",
#                htmltools::htmlEscape(url), "' target='_blank' rel='noopener'>[source]</a></span>")
#       }
#       Definition <- mapply(add_link, df$definition, df$source, USE.NAMES = FALSE)
#       data.frame(Term = df$term, Definition = Definition, stringsAsFactors = FALSE)
#     }

#     observeEvent(input$open, {
#       df_raw <- resolve_df(terms)
#       if (nrow(df_raw)) df_raw <- df_raw[order(tolower(df_raw$term)), , drop = FALSE]
#       df_tbl <- make_table_df(df_raw)

#       output$tbl <- reactable::renderReactable({
#         reactable::reactable(
#           df_tbl,
#           searchable = TRUE, filterable = TRUE, striped = TRUE, compact = TRUE, highlight = TRUE,
#           defaultSorted = list(Term = "asc"), defaultPageSize = 10, showPageSizeOptions = TRUE,
#           minRows = 5, resizable = TRUE,
#           columns = list(
#             Term = reactable::colDef(minWidth = 180),
#             Definition = reactable::colDef(
#               html = TRUE, minWidth = 520
#               # cell = function(value) htmltools::div(
#               #   class = "glossary-reactable",
#               #   style = "white-space: normal;",
#               #   htmltools::HTML(value)
#               # )
#             )
#           )
#         )
#       })

#       # Open the offcanvas (non-blocking overlay)
#       session$sendCustomMessage("open-offcanvas", list(id = ns("panel")))
#     })
#   })
# }

# R/mod_glossary_overlay.R
# Non-blocking glossary overlay without Bootstrap 5.
# Variant "float": draggable absolutePanel
# Variant "bottom": bottom sheet (fixed at bottom)
# Requires: reactable

# R/mod_glossary_float.R
# Non-blocking floating popup (fixed position) with reactable glossary.
# Works with Bootstrap 3. No offcanvas/BS5 needed.

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
      if (nrow(df)) df <- df[order(tolower(df$term)), , drop = FALSE]
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
        searchable = TRUE, filterable = TRUE, striped = TRUE, compact = TRUE, highlight = TRUE,
        defaultSorted = list(Term = "asc"), defaultPageSize = 10, showPageSizeOptions = TRUE,
        minRows = 5, resizable = TRUE,
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
