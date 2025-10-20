# mod_glossary_modal.R
mod_glossary_modal_ui <- function(id, link_text = "Glossary") {
  ns <- NS(id)
  # Action link renders as a hyperlink (not a button)
  actionLink(ns("open"), label = tagList(icon("book"), link_text),
             class = "glossary-link", style = "margin-left:.5rem;")
}

mod_glossary_modal_server <- function(id, terms, title = "Glossary", size = "l") {
  moduleServer(id, function(input, output, session) {

    # Helper to normalize inputs: accepts data.frame(term, definition) OR list of {term, def}
    resolve_items <- function(x) {
      if (is.function(x)) x <- x()  # works for reactive(...)
      if (is.data.frame(x)) {
        stopifnot(all(c("term","definition") %in% names(x)))
        lapply(seq_len(nrow(x)), function(i) list(term = x$term[i], def = x$definition[i]))
      } else if (is.list(x) && length(x) && !is.null(x[[1]]$term)) {
        x
      } else if (is.list(x) && length(x) && !is.null(x$term)) {
        list(x)
      } else {
        list()
      }
    }

    observeEvent(input$open, {
      items <- resolve_items(terms)

      showModal(
        modalDialog(
          title = title,
          easyClose = TRUE,
          size = size,
          footer = modalButton("Close"),
          tags$div(
            class = "glossary-modal",
            style = "max-height:60vh; overflow-y:auto;",
            tags$dl(
              class = "glossary",
              lapply(items, function(it) {
                list(
                  tags$dt(tags$b(it$term)),
                  tags$dd(HTML(it$def))
                )
              })
            )
          )
        )
      )
    })
  })
}
