#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom stringr str_split
#' @noRd
# app_server.R

# =========================
# SERVER (simplified: single restore + single writer)
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

  # Map subtab input ids (read) & selectors (write)
  SUBTAB_INPUTS <- list(
    overview     = "overview_1-tabs_overview",
    landings     = "landings_1-main_tabset",
    stock_status = "stock_status_1-main_tabset"
  )

  get_current_subtab <- function(tab, input) {
    id <- SUBTAB_INPUTS[[tab]]
    if (is.null(id)) "" else as.character(input[[id]] %||% "")
  }

  select_subtab <- function(tab, value) {
    if (!nzchar(value)) return(invisible(NULL))
    if (tab == "stock_status" && requireNamespace("bslib", quietly = TRUE) &&
        utils::packageVersion("bslib") >= "0.5.0") {
      bslib::nav_select(id = SUBTAB_INPUTS[[tab]], selected = value, session = session)
    } else {
      updateTabsetPanel(session, SUBTAB_INPUTS[[tab]], selected = value)
    }
  }

  write_hash <- function(eco, tab, sub) {
    paste0(
      "#eco=", utils::URLencode(eco %||% "", reserved = TRUE),
      "&tab=", utils::URLencode(tab %||% "", reserved = TRUE),
      if (nzchar(sub %||% "")) paste0("&subtab=", utils::URLencode(sub, reserved = TRUE)) else ""
    )
  }

  # ------------------------
  # Welcome (unchanged)
  # ------------------------
  showModal(modalDialog(
    title = "Important message",
    HTML("Welcome to the development version of the fisheriesXplorer application. <u>The contents are indicative and should not be quoted or used elsewhere</u>.")
  ))

  # ------------------------
  # Date bits (unchanged)
  # ------------------------
  app_date  <- strsplit(date(), " ")[[1]]
  cap_year  <- app_date[5]
  cap_month <- app_date[2]

  # ------------------------
  # Shared app state
  # ------------------------
  selected_ecoregion <- reactiveVal(NULL)

  # Track whether we're restoring from URL (suspends writer)
  is_restoring <- reactiveVal(TRUE)

  # One-time desired state from URL
  desired <- reactiveVal(NULL)

  # Read URL once
  observeEvent(session$clientData$url_hash, {
    hash   <- isolate(session$clientData$url_hash)   %||% ""
    search <- isolate(session$clientData$url_search) %||% ""
    p <- parse_nav(hash, search)
    # Stage desired state
    desired(list(
      eco    = p$eco    %||% "",
      tab    = p$tab    %||% "",
      subtab = p$subtab %||% ""
    ))
    is_restoring(TRUE)
  }, once = TRUE, ignoreInit = FALSE)

  # Single restore path (bounded retry while inputs bind)
  observe({
    d <- desired()
    if (is.null(d)) return()
    if (!isTRUE(is_restoring())) return()

    # 1) eco immediately
    if (nzchar(d$eco)) selected_ecoregion(d$eco)

    # 2) top tab immediately
    if (nzchar(d$tab)) updateNavbarPage(session, "nav-page", selected = d$tab)

    # 3) subtab after controls bind; retry briefly until it sticks or no subtab requested
    if (nzchar(d$subtab) && d$tab %in% names(SUBTAB_INPUTS)) {
      # if mismatch, try selecting; we run this observe block repeatedly while restoring
      cur <- get_current_subtab(d$tab, input)
      if (!identical(cur, d$subtab)) select_subtab(d$tab, d$subtab)
    }

    # Stop restoring only when current live state matches desired (or no subtab requested)
    cur_tab <- input$`nav-page` %||% ""
    cur_sub <- get_current_subtab(d$tab, input)
    if (identical(selected_ecoregion() %||% "", d$eco %||% "") &&
        (!nzchar(d$tab) || identical(cur_tab, d$tab)) &&
        (!nzchar(d$subtab) || identical(cur_sub, d$subtab))) {
      is_restoring(FALSE)
      return()
    }

    # Re-check soon while restoring (short, bounded loop without onFlushed churn)
    if (isTRUE(is_restoring())) invalidateLater(80, session)
  })

  # ------------------------
  # Data fetch (unchanged)
  # ------------------------
  shared <- reactiveValues(SID = NULL, SAG = NULL, clean_status = NULL)

  fetchData <- reactive({
    withProgress(message = paste0("Fetching data for ", selected_ecoregion(), "..."), value = 0, {
      incProgress(0.2, detail = "Getting SID...")
      sid <- tryCatch(
        getSID(year = as.integer(format(Sys.Date(), "%Y")), EcoR = selected_ecoregion()),
        error = function(e) paste("Error fetching SID:", e$message)
      )
      incProgress(0.5, detail = "Getting SAG...")
      sag <- tryCatch(
        getSAG_ecoregion_new(selected_ecoregion()),
        error = function(e) paste("Error fetching SAG:", e$message)
      )
      incProgress(0.9, detail = "Getting SAG status...")
      status <- tryCatch(
        format_sag_status_new(getStatusWebService(selected_ecoregion(), sid)),
        error = function(e) paste("Error fetching SAG status:", e$message)
      )
      list(SID = sid, SAG = sag, clean_status = status)
    })
  }) %>% bindCache(selected_ecoregion())

  observe({
    data <- fetchData()
    shared$SID          <- data$SID
    shared$SAG          <- data$SAG
    shared$clean_status <- data$clean_status
  })

  # ------------------------
  # Feature modules (unchanged; parent handles bookmarking)
  # ------------------------
  mod_navigation_page_server("navigation_page_1", parent_session = session, selected_ecoregion = selected_ecoregion)

  mod_overview_server(
    "overview_1",
    selected_ecoregion = selected_ecoregion,
    bookmark_qs        = reactive(list()),     # parent restores
    set_subtab         = function(...) {}      # no-op
  )
  mod_landings_server(
    "landings_1", cap_year, cap_month,
    selected_ecoregion = selected_ecoregion, shared = shared,
    bookmark_qs        = reactive(list()),     # parent restores
    set_subtab         = function(...) {}
  )
  mod_stock_status_server(
    "stock_status_1", cap_year, cap_month,
    selected_ecoregion = selected_ecoregion, shared = shared,
    bookmark_qs        = reactive(list()),     # parent restores
    set_subtab         = function(...) {}
  )
  # sel_mixfish <- mod_mixfish_plot_selection_server("mixfish_selection_1", selected_ecoregion = selected_ecoregion)
  # mod_mixfish_plot_display_server("mixfish_viz_1",
  #   plot_name          = sel_mixfish$plot_choice,
  #   selected_ecoregion = selected_ecoregion,
  #   selected_subRegion = sel_mixfish$sub_region
  # )
  # mod_vms_server("vms_1", selected_ecoregion = selected_ecoregion)
  # mod_bycatch_server("bycatch_1", selected_ecoregion = selected_ecoregion)
  mod_resources_server("resources_1")
  # ------------------------
  # Single writer: keep URL hash in sync (debounced), except during restore
  # ------------------------
  current_state <- reactive({
    list(
      eco    = selected_ecoregion() %||% "",
      tab    = input$`nav-page`     %||% "",
      subtab = {
        t <- input$`nav-page` %||% ""
        get_current_subtab(t, input)
      }
    )
  })
  current_state_deb <- debounce(current_state, millis = 150)

  observeEvent(current_state_deb(), {
    if (isTRUE(is_restoring())) return()
    st <- current_state_deb()
    shinyjs::runjs(sprintf("location.hash = %s;",
      jsonlite::toJSON(write_hash(st$eco, st$tab, st$subtab), auto_unbox = TRUE)
    ))
  }, ignoreInit = TRUE)

  # When restore completes, write once immediately
  observeEvent(is_restoring(), {
    if (isTRUE(is_restoring())) return()
    st <- current_state()
    shinyjs::runjs(sprintf("location.hash = %s;",
      jsonlite::toJSON(write_hash(st$eco, st$tab, st$subtab), auto_unbox = TRUE)
    ))
  }, ignoreInit = TRUE)

  # ------------------------
  # Share button (uses the same writer)
  # ------------------------
  share_url <- reactiveVal(NULL)
  observeEvent(input$share_btn, {
    st <- current_state()
    final <- paste0(.base_url(session), write_hash(st$eco, st$tab, st$subtab))
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

  # Clipboard handler (unchanged)
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
}
