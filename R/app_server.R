#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom stringr str_split
#' @noRd
# app_server.R

app_server <- function(input, output, session) {
  # ------------------------------------------------------------
  # Welcome modal (unchanged)
  # ------------------------------------------------------------
  showModal(modalDialog(
    title = "Important message",
    HTML("Welcome to the development version of the fisheriesXplorer application. <u>The contents are indicative and should not be quoted or used elsewhere</u>.")
  ))

  # ------------------------------------------------------------
  # Basic date bits used by some modules (unchanged, but base R)
  # ------------------------------------------------------------
  app_date <- strsplit(date(), " ")[[1]]
  cap_year <- app_date[5]
  cap_month <- app_date[2]

  # ------------------------------------------------------------
  # Bookmarking: parse incoming query once on load
  #   URL shape we target: ?eco=<ECORGN_CODE>&tab=<top_tab>&subtab=<module_subtab>
  # ------------------------------------------------------------
  initial_qs <- reactive(shiny::parseQueryString(session$clientData$url_search))

  # Will hold the *current* module subtab value (reported by active module)
  current_subtab <- reactiveVal(NULL)


  # Helper a module can call to publish its active subtab to the parent
  set_subtab <- function(val) current_subtab(val)

  # Tabs that do NOT have module-level subtabs
  no_subtab_tabs <- c("home", "overview", "vms", "bycatch") # adjust to your values

  # If the user switches to a no-subtab tab, clear any previously set subtab
  observeEvent(input$`nav-page`,
    {
      if (isTRUE(input$`nav-page` %in% no_subtab_tabs)) {
        current_subtab(NULL)
      }
    },
    ignoreInit = FALSE
  )

  # ------------------------------------------------------------
  # Shared state
  # ------------------------------------------------------------
  selected_ecoregion <- reactiveVal(NULL)
  share_url <- reactiveVal(NULL)

  # Restore ecoregion + top tab from URL once (modules will restore their own subtabs)
  observeEvent(initial_qs(),
    {
      qs <- initial_qs()


      # Restore ecoregion from ?eco=
      if (!is.null(qs$eco) && nzchar(qs$eco)) {
        selected_ecoregion(qs$eco)
        # NOTE: if you also want the ecoregion dropdown in your navigation module to
        # reflect this on first load, add a small restore handler inside that module.
      }

      # Restore top-level navbar tab from ?tab=
      if (!is.null(qs$tab) && nzchar(qs$tab)) {
        updateNavbarPage(session, "nav-page", selected = qs$tab)
      }
      # Subtabs are restored by the modules themselves (see comments below).
    },
    once = TRUE,
    ignoreInit = TRUE
  )

  # ------------------------------------------------------------
  # Navigation module (sets selected_ecoregion during normal use)
  # ------------------------------------------------------------
  mod_navigation_page_server(
    "navigation_page_1",
    parent_session     = session,
    selected_ecoregion = selected_ecoregion
  )

  # ------------------------------------------------------------
  # Shared container / data fetch (unchanged)
  # ------------------------------------------------------------
  shared <- reactiveValues(
    SID = NULL,
    SAG = NULL,
    clean_status = NULL
  )

  fetchData <- reactive({
    withProgress(message = paste0("Fetching data for ", selected_ecoregion(), "..."), value = 0, {
      incProgress(0.2, detail = "Getting SID...")
      sid <- tryCatch(
        getSID(
          year = as.integer(format(Sys.Date(), "%Y")),
          EcoR = selected_ecoregion()
        ),
        error = function(e) {
          paste("Error fetching SID:", e$message)
        }
      )

      incProgress(0.5, detail = "Getting SAG...")
      sag <- tryCatch(
        getSAG_ecoregion_new(selected_ecoregion()),
        error = function(e) {
          paste("Error fetching SAG:", e$message)
        }
      )

      incProgress(0.9, detail = "Getting SAG status...")
      status <- tryCatch(
        format_sag_status_new(
          getStatusWebService(selected_ecoregion(), sid)
        ),
        error = function(e) {
          paste("Error fetching SAG status:", e$message)
        }
      )

      list(SID = sid, SAG = sag, clean_status = status)
    })
  }) %>% bindCache(selected_ecoregion()) # cache by ecoregion

  observe({
    data <- fetchData()
    shared$SID <- data$SID
    shared$SAG <- data$SAG
    shared$clean_status <- data$clean_status
  })

  # ------------------------------------------------------------
  # Feature modules
  #   - For modules that have internal tabsets (subtabs), add *two tiny hooks*:
  #       1) restore their subtab from initial_qs()$subtab on first load
  #       2) call set_subtab(input$<their_tabset_id>) when the subtab changes
  #   See examples in the comments below.
  # ------------------------------------------------------------

  mod_overview_server(
    "overview_1",
    selected_ecoregion = selected_ecoregion
  )

  # Landings module
  mod_landings_server(
    "landings_1",
    cap_year, cap_month,
    selected_ecoregion = selected_ecoregion,
    shared = shared,
    bookmark_qs = initial_qs,
    set_subtab = set_subtab
  )

  # Stock status module
  mod_stock_status_server(
    "stock_status_1",
    cap_year, cap_month,
    selected_ecoregion = selected_ecoregion,
    shared = shared,
    bookmark_qs = initial_qs,
    set_subtab = set_subtab
  )

  # Mixed fisheries (selection + display)
  sel_mixfish <- mod_mixfish_plot_selection_server(
    "mixfish_selection_1",
    selected_ecoregion = selected_ecoregion
  )
  mod_mixfish_plot_display_server(
    "mixfish_viz_1",
    plot_name          = sel_mixfish$plot_choice,
    selected_ecoregion = selected_ecoregion,
    selected_subRegion = sel_mixfish$sub_region
    # If this module has its own subtabs, pass bookmark_qs + set_subtab similarly
  )

  mod_vms_server("vms_1", selected_ecoregion = selected_ecoregion)
  mod_bycatch_server("bycatch_1", selected_ecoregion = selected_ecoregion)

  # ------------------------------------------------------------
  # Share button → write ?eco= & ?tab= & (optional) &subtab= into URL and bookmark
  #   (Requires bookmarkButton('share_btn', ...) in your navbar UI)
  # ------------------------------------------------------------
  observeEvent(input$share_btn, {
    eco <- tryCatch(as.character(selected_ecoregion()), error = function(e) "")
    if (is.null(eco) || length(eco) == 0) eco <- ""

    qs <- paste0(
      "?eco=", utils::URLencode(eco, reserved = TRUE),
      "&tab=", utils::URLencode(input$`nav-page`, reserved = TRUE),
      if (!is.null(current_subtab()) && nzchar(current_subtab())) {
        paste0("&subtab=", utils::URLencode(current_subtab(), reserved = TRUE))
      } else {
        ""
      }
    )

    updateQueryString(qs, mode = "replace", session = session)
    session$doBookmark()
  })

  # --- refined modal after bookmarking completes ---
  onBookmarked(function(url) {
    share_url(url)

    showModal(modalDialog(
      title = "Share this view",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("copy_share_link", "Copy link", icon = icon("copy"), class = "btn btn-primary btn-sm")
      ),
      # Body
      tags$p("This link reproduces this exact view:"),
      # Pretty monospace box with wrapping
      tags$div(
        id = "share_url_box",
        style = paste(
          "font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;",
          "font-size: 0.9em;",
          "background: #f6f8fa;",
          "border: 1px solid #e1e4e8;",
          "border-radius: 6px;",
          "padding: 8px 10px;",
          "white-space: normal;", # allow wrapping
          "overflow-wrap: anywhere;", # wrap long words/URLs
          "word-break: break-word;" # extra safety
        ),
        url
      ),
      tags$div(
        style = "margin-top: 8px;",
        tags$a(href = url, target = "_blank", rel = "noopener", "Open in new tab")
      ),
      size = "m"
    ))
  })

  # --- Copy button handler (uses Clipboard API) ---
  observeEvent(input$copy_share_link, {
    # Safely JSON-encode the URL for JS
    js_text <- jsonlite::toJSON(share_url() %||% "", auto_unbox = TRUE)
    shinyjs::runjs(sprintf("
    (function(txt){
      if (navigator.clipboard && window.isSecureContext) {
        navigator.clipboard.writeText(txt)
          .then(function(){ Shiny.setInputValue('share_copy_success', Math.random()); })
          .catch(function(err){ Shiny.setInputValue('share_copy_error', String(err)); });
      } else {
        // Fallback: temporary textarea
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

  # Optional toast notifications
  observeEvent(input$share_copy_success, {
    showNotification("Link copied to clipboard", type = "message")
  })
  observeEvent(input$share_copy_error, {
    showNotification(paste("Copy failed:", input$share_copy_error), type = "error")
  })
}

