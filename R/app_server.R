#' The application server-side
#'
#' Main server function for the \code{fisheriesXplorer} Shiny application.
#' It initialises shared reactive state, handles URL-based bookmarking and
#' share links, coordinates data fetching for the selected ecoregion, and
#' wires together the feature modules (navigation, overview, landings,
#' stock status, resources, etc.).
#'
#' @param input Internal list of reactive inputs provided by Shiny.
#' @param output Internal list of reactive outputs provided by Shiny.
#' @param session Shiny session object, used to access client data,
#'   send custom messages, and update UI elements.
#'
#' @return
#' This function is called for its side effects and does not return a
#' meaningful value.
#'
#' @details
#' The server logic includes:
#' \itemize{
#'   \item Displaying a welcome modal with an important notice.
#'   \item Deriving capture year and month from the current date.
#'   \item Managing a shared \code{selected_ecoregion} reactive and
#'     associated data (SID, SAG, and formatted status).
#'   \item Parsing and restoring navigation state from the URL hash or
#'     query string via \code{parse_nav()}, \code{get_current_subtab()},
#'     and \code{select_subtab()}.
#'   \item Keeping the URL hash in sync with the current app state
#'     using debounced observers and \code{write_hash()}.
#'   \item Initialising and calling the feature module server functions
#'     (navigation, overview, landings, stock status, resources).
#'   \item Implementing a share-link modal and clipboard copy logic
#'     using \code{.base_url()} and a custom \code{copyText} message.
#' }
#'
#' This function is intended to be registered as the main server
#' function in \code{shinyApp()} or a similar entry point.
#'
#' @import shiny
#' @importFrom stringr str_split
#' @noRd
app_server <- function(input, output, session) {

  
  ######################## Welcome modal (maybe we could add useful information here?) ########################
  
  showModal(modalDialog(
    title = "Important message",
    HTML("Welcome to the development version of the fisheriesXplorer application. <u>The contents are indicative and should not be quoted or used elsewhere</u>.")
  ))

  # ------------------------
  # Date bits (refactor all the dates here?)
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
      if (!identical(cur, d$subtab)) select_subtab(d$tab, d$subtab, session)
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

  
  ##################################### Data fetching #####################################
  
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
        format_sag_status_new(getStatusWebService(selected_ecoregion(), sid), sag),
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

  ##################################### Feature modules (parent handles bookmarking) #####################################
  mod_navigation_page_server(
    "navigation_page_1", 
    parent_session = session, 
    selected_ecoregion = selected_ecoregion,
    bookmark_qs        = reactive(list())
  )

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
  mod_resources_server(
    "resources_1",
    bookmark_qs        = reactive(list()),     # parent restores
    set_subtab         = function(...) {} 
  )
  
  ##################################### Single writer: keep URL hash in sync (debounced), except during restore #####################################
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

  ##################################### Share link modal and clipboard #####################################    
   
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
      tags$div(id = "share_url_box", final),
      tags$div(style = "margin-top: 8px;", tags$a(href = final, target = "_blank", rel = "noopener", "Open in new tab")),
      size = "m"
    ))
  })

  ########################################## Clipboard handler ##########################################
  observeEvent(input$copy_share_link, {
    session$sendCustomMessage("copyText", list(text = share_url() %||% ""))
  })
  observeEvent(input$share_copy_success, { showNotification("Link copied to clipboard", type = "message") })
  observeEvent(input$share_copy_error,   { showNotification(paste("Copy failed:", input$share_copy_error), type = "error") })
}
