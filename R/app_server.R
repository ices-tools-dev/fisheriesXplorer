#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom stringr str_split
#' @noRd
# app_server.R

# =========================
# SERVER (ECO-PIN + FORCE-SELECT with once=TRUE + SAFE STOP)
# =========================
app_server <- function(input, output, session) {

  # ------------------------
  # Helpers
  # ------------------------
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a else b

  .base_url <- function(session) {
    proto <- session$clientData$url_protocol %||% "https:"
    host  <- session$clientData$url_hostname %||% ""
    port  <- session$clientData$url_port
    path  <- session$clientData$url_pathname %||% "/"
    port_part <- if (!is.null(port) && nzchar(port) &&
                     !(proto == "https:" && port == "443") &&
                     !(proto == "http:"  && port == "80")) paste0(":", port) else ""
    paste0(proto, "//", host, port_part, path)
  }

  parse_nav <- function(hash, search) {
    if (nzchar(hash)) shiny::parseQueryString(sub("^#", "", hash))
    else if (nzchar(search)) shiny::parseQueryString(search)
    else list()
  }

  .current_subtab_for <- function(tab, input) {
    id <- switch(tab,
      "overview"     = "overview_1-tabs_overview",
      "landings"     = "landings_1-main_tabset",
      "stock_status" = "stock_status_1-main_tabset",
      NULL
    )
    if (is.null(id)) return("")
    val <- input[[id]]
    if (is.null(val) || !nzchar(val)) "" else as.character(val)
  }

  # ------------------------
  # Welcome modal (unchanged)
  # ------------------------
  showModal(modalDialog(
    title = "Important message",
    HTML("Welcome to the development version of the fisheriesXplorer application. <u>The contents are indicative and should not be quoted or used elsewhere</u>.")
  ))

  # ------------------------
  # Date bits
  # ------------------------
  app_date  <- strsplit(date(), " ")[[1]]
  cap_year  <- app_date[5]
  cap_month <- app_date[2]

  # ------------------------
  # Shared state
  # ------------------------
  selected_ecoregion <- reactiveVal(NULL)
  share_url          <- reactiveVal(NULL)

  subtab_for <- reactiveValues(overview = NULL, landings = NULL, stock_status = NULL)
  set_subtab_for <- function(top_tab_key, value) subtab_for[[top_tab_key]] <- value

  # ------------------------
  # Parse URL and stage desired location
  # ------------------------
  nav_params_rv  <- reactiveVal(list())
  is_restoring   <- reactiveVal(TRUE)
  want_tab_rv    <- reactiveVal(NULL)
  want_subtab_rv <- reactiveVal(NULL)

  observeEvent(session$clientData$url_hash, {
    hash   <- isolate(session$clientData$url_hash)   %||% ""
    search <- isolate(session$clientData$url_search) %||% ""
    p <- parse_nav(hash, search)
    nav_params_rv(p)

    if (!is.null(p$eco) && nzchar(p$eco)) selected_ecoregion(p$eco)
    if (!is.null(p$tab) && nzchar(p$tab)) updateNavbarPage(session, "nav-page", selected = p$tab)

    want_tab_rv(p$tab %||% NULL)
    want_subtab_rv(p$subtab %||% NULL)

    if (!is.null(p$tab) && nzchar(p$tab) && !is.null(p$subtab) && nzchar(p$subtab)) {
      if (p$tab %in% c("overview", "landings", "stock_status")) {
        subtab_for[[p$tab]] <- p$subtab
      }
    }
  }, once = TRUE, ignoreInit = FALSE)

  # Enforce eco during restore (prevents fallback)
  observe({
    if (!isTRUE(is_restoring())) return()
    want_eco <- (nav_params_rv()$eco %||% "")
    if (!nzchar(want_eco)) return()
    if (!identical(selected_ecoregion() %||% "", want_eco)) {
      selected_ecoregion(want_eco)
    }
  })

  # One-time parent selection (don’t rely on module observers)
  did_parent_restore <- reactiveVal(FALSE)
  observe({
    if (isTRUE(did_parent_restore())) return()
    want_tab    <- want_tab_rv()
    want_subtab <- want_subtab_rv()
    if (is.null(want_tab) || !nzchar(want_tab) ||
        is.null(want_subtab) || !nzchar(want_subtab)) return()
    if ((input$`nav-page` %||% "") != want_tab) return()

    try({
      if (identical(want_tab, "overview")) {
        updateTabsetPanel(session, "overview_1-tabs_overview", selected = want_subtab)
      } else if (identical(want_tab, "landings")) {
        updateTabsetPanel(session, "landings_1-main_tabset", selected = want_subtab)
      } else if (identical(want_tab, "stock_status")) {
        if (requireNamespace("bslib", quietly = TRUE) &&
            utils::packageVersion("bslib") >= "0.5.0") {
          bslib::nav_select(id = "stock_status_1-main_tabset", selected = want_subtab, session = session)
        } else {
          updateTabsetPanel(session, "stock_status_1-main_tabset", selected = want_subtab)
        }
      }
    }, silent = TRUE)

    did_parent_restore(TRUE)
  })

  # FORCE-SELECT UNTIL MATCH, but with once=TRUE (no permanent hook)
  observe({
    if (!isTRUE(is_restoring())) return()

    want_tab    <- want_tab_rv()    %||% ""
    want_subtab <- want_subtab_rv() %||% ""
    if (!nzchar(want_tab) || !nzchar(want_subtab)) return()
    if ((input$`nav-page` %||% "") != want_tab) return()

    if (identical(.current_subtab_for(want_tab, input), want_subtab)) return()

    session$onFlushed(function() {
      if (identical(want_tab, "overview")) {
        updateTabsetPanel(session, "overview_1-tabs_overview",  selected = want_subtab)
      } else if (identical(want_tab, "landings")) {
        updateTabsetPanel(session, "landings_1-main_tabset",    selected = want_subtab)
      } else if (identical(want_tab, "stock_status")) {
        if (requireNamespace("bslib", quietly = TRUE) &&
            utils::packageVersion("bslib") >= "0.5.0") {
          bslib::nav_select(id = "stock_status_1-main_tabset", selected = want_subtab, session = session)
        } else {
          updateTabsetPanel(session, "stock_status_1-main_tabset", selected = want_subtab)
        }
      }
    }, once = TRUE)  # <-- key change
  })

  # Don’t finish restoring until eco + tab + subtab match
  observe({
    if (!isTRUE(is_restoring())) return()

    want_tab    <- want_tab_rv()
    want_subtab <- want_subtab_rv()
    want_eco    <- (nav_params_rv()$eco %||% "")

    if (nzchar(want_eco) && !identical(selected_ecoregion() %||% "", want_eco)) return()

    if (is.null(want_tab) || !nzchar(want_tab)) {
      cur_tab <- input$`nav-page` %||% ""
      if (!nzchar(cur_tab)) return()
      cur_sub <- .current_subtab_for(cur_tab, input)
      if (cur_sub == "" && cur_tab %in% c("overview","landings","stock_status")) return()
      is_restoring(FALSE)
      return()
    }

    if ((input$`nav-page` %||% "") != want_tab) return()

    if (!is.null(want_subtab) && nzchar(want_subtab) && want_tab %in% c("overview","landings","stock_status")) {
      if (!identical(.current_subtab_for(want_tab, input), want_subtab)) return()
    }

    is_restoring(FALSE)
  })

  # ------------------------
  # Modules
  # ------------------------
  mod_navigation_page_server("navigation_page_1", parent_session = session, selected_ecoregion = selected_ecoregion)

  shared <- reactiveValues(SID = NULL, SAG = NULL, clean_status = NULL)
  fetchData <- reactive({
    withProgress(message = paste0("Fetching data for ", selected_ecoregion(), "..."), value = 0, {
      incProgress(0.2, detail = "Getting SID...")
      sid <- tryCatch(getSID(year = as.integer(format(Sys.Date(), "%Y")), EcoR = selected_ecoregion()),
                      error = function(e) paste("Error fetching SID:", e$message))
      incProgress(0.5, detail = "Getting SAG...")
      sag <- tryCatch(getSAG_ecoregion_new(selected_ecoregion()),
                      error = function(e) paste("Error fetching SAG:", e$message))
      incProgress(0.9, detail = "Getting SAG status...")
      status <- tryCatch(format_sag_status_new(getStatusWebService(selected_ecoregion(), sid)),
                         error = function(e) paste("Error fetching SAG status:", e$message))
      list(SID = sid, SAG = sag, clean_status = status)
    })
  }) %>% bindCache(selected_ecoregion())

  observe({
    data <- fetchData()
    shared$SID          <- data$SID
    shared$SAG          <- data$SAG
    shared$clean_status <- data$clean_status
  })

  mod_overview_server(
    "overview_1",
    selected_ecoregion = selected_ecoregion,
    bookmark_qs        = reactive(nav_params_rv()),
    set_subtab         = function(val) set_subtab_for("overview", val)
  )

  mod_landings_server(
    "landings_1",
    cap_year, cap_month,
    selected_ecoregion = selected_ecoregion,
    shared             = shared,
    bookmark_qs        = reactive(nav_params_rv()),
    set_subtab         = function(val) set_subtab_for("landings", val)
  )

  mod_stock_status_server(
    "stock_status_1",
    cap_year, cap_month,
    selected_ecoregion = selected_ecoregion,
    shared             = shared,
    bookmark_qs        = reactive(nav_params_rv()),
    set_subtab         = function(val) set_subtab_for("stock_status", val)
  )

  sel_mixfish <- mod_mixfish_plot_selection_server("mixfish_selection_1", selected_ecoregion = selected_ecoregion)
  mod_mixfish_plot_display_server("mixfish_viz_1",
    plot_name = sel_mixfish$plot_choice,
    selected_ecoregion = selected_ecoregion,
    selected_subRegion = sel_mixfish$sub_region
  )
  mod_vms_server("vms_1", selected_ecoregion = selected_ecoregion)
  mod_bycatch_server("bycatch_1", selected_ecoregion = selected_ecoregion)

  # ------------------------
  # Share link
  # ------------------------
  observeEvent(input$share_btn, {
    eco <- tryCatch(as.character(selected_ecoregion()), error = function(e) "") %||% ""
    tab <- input$`nav-page` %||% ""
    sub <- .current_subtab_for(tab, input)

    hash <- paste0(
      "#eco=", utils::URLencode(eco, reserved = TRUE),
      "&tab=", utils::URLencode(tab, reserved = TRUE),
      if (nzchar(sub)) paste0("&subtab=", utils::URLencode(sub, reserved = TRUE)) else ""
    )

    final <- paste0(.base_url(session), hash)
    share_url(final)

    showModal(modalDialog(
      title = "Share this view",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("copy_share_link", "Copy link", icon = icon("copy"), class = "btn btn-primary btn-sm")
      ),
      tags$p("This link reproduces this exact view:"),
      tags$div(
        id = "share_url_box",
        style = paste(
          "font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Courier New', monospace;",
          "font-size: 0.9em; background:#f6f8fa; border:1px solid #e1e4e8;",
          "border-radius:6px; padding:8px 10px; white-space:normal;",
          "overflow-wrap:anywhere; word-break:break-word;"
        ),
        final
      ),
      tags$div(style = "margin-top: 8px;", tags$a(href = final, target = "_blank", rel = "noopener", "Open in new tab")),
      size = "m"
    ))
  })

  observeEvent(input$copy_share_link, {
    js_text <- jsonlite::toJSON(share_url() %||% "", auto_unbox = TRUE)
    shinyjs::runjs(sprintf("
      (function(txt){
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(txt)
            .then(function(){ Shiny.setInputValue('share_copy_success', Math.random()); })
            .catch(function(err){ Shiny.setInputValue('share_copy_error', String(err)); });
        } else {
          var ta = document.createElement('textarea');
          ta.value = txt; document.body.appendChild(ta);
          ta.select(); try { document.execCommand('copy');
            Shiny.setInputValue('share_copy_success', Math.random());
          } catch(e){ Shiny.setInputValue('share_copy_error', String(e)); }
          document.body.removeChild(ta);
        }
      })(%s);
    ", js_text))
  })
  observeEvent(input$share_copy_success, { showNotification("Link copied to clipboard", type = "message") })
  observeEvent(input$share_copy_error,   { showNotification(paste("Copy failed:", input$share_copy_error), type = "error") })

  # ------------------------
  # Keep address bar hash in sync (not during restore)
  # ------------------------
  write_hash_now <- function() {
    eco <- selected_ecoregion() %||% ""
    tab <- input$`nav-page`    %||% ""
    sub <- .current_subtab_for(tab, input)
    paste0(
      "#eco=", utils::URLencode(eco, reserved = TRUE),
      "&tab=", utils::URLencode(tab, reserved = TRUE),
      if (nzchar(sub)) paste0("&subtab=", utils::URLencode(sub, reserved = TRUE)) else ""
    )
  }

  observeEvent(input$`nav-page`, {
    if (isTRUE(is_restoring())) return(invisible(NULL))
    shinyjs::runjs(sprintf("location.hash = %s;", jsonlite::toJSON(write_hash_now(), auto_unbox = TRUE)))
  }, ignoreInit = TRUE)

  observeEvent(
    list(
      input$`overview_1-tabs_overview`,
      input$`landings_1-main_tabset`,
      input$`stock_status_1-main_tabset`
    ),
    {
      if (isTRUE(is_restoring())) return(invisible(NULL))
      shinyjs::runjs(sprintf("location.hash = %s;", jsonlite::toJSON(write_hash_now(), auto_unbox = TRUE)))
    },
    ignoreInit = FALSE
  )

  # When restoring finishes, write once and clear desired targets
  observeEvent(is_restoring(), {
    if (isTRUE(is_restoring())) return()
    shinyjs::runjs(sprintf("location.hash = %s;", jsonlite::toJSON(write_hash_now(), auto_unbox = TRUE)))
    want_tab_rv(NULL); want_subtab_rv(NULL)   # <-- prevent any stale enforcement
  }, ignoreInit = TRUE)

  # Optional: still react to module-reported signals
  observe({
    if (isTRUE(is_restoring())) return(invisible(NULL))
    dummy <- list(subtab_for$overview, subtab_for$landings, subtab_for$stock_status)
    shinyjs::runjs(sprintf("location.hash = %s;", jsonlite::toJSON(write_hash_now(), auto_unbox = TRUE)))
  })
}




