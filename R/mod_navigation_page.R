#' Navigation / landing page UI module
#'
#' Build the landing page for the fisheriesXplorer app, combining:
#' \itemize{
#'   \item an interactive ICES ecoregion map with a synced selector, and
#'   \item three large topic buttons (Overview, Landings, Stock status)
#'         that the server module can use to navigate to the relevant
#'         sections of the app.
#' }
#'
#' The module exposes a single UI function to be used in \code{app_ui()}
#' and a corresponding server module (e.g. \code{mod_navigation_page_server()})
#' that wires the inputs into the rest of the application.
#'
#' @param id Character string used to create a namespace for the module
#'   via \code{shiny::NS()}. Typically passed as a literal in
#'   \code{mod_navigation_page_ui("navigation_page_1")}.
#'
#' @return A \link[shiny]{tagList} containing the landing-page UI:
#'   a hidden \code{tabsetPanel} with the map card (leaflet output and
#'   virtual select input) and a card of image-based navigation buttons.
#'   Intended to be included inside a higher-level UI definition such
#'   as \code{navbarPage()}.
#'
#' @seealso \code{\link{mod_navigation_page_server}} for the server-side
#'   logic associated with this UI.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tabsetPanel tabPanel fluidRow column br actionLink
#' @importFrom bslib card card_header card_body layout_column_wrap
#' @importFrom leaflet leafletOutput leafletProxy hideGroup showGroup
#' @importFrom shinyWidgets virtualSelectInput updateVirtualSelect
#' @importFrom shinyjs onclick
#' @importFrom stringr str_replace_all
mod_navigation_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$img(id = "logo", class = "center-block", src = "www/fisheriesxplorer_blue.png"),
    br(), br(),
    tabsetPanel(
      type = "hidden",
      id = ns("landing_page"),
      tabPanel("Map Tab",
        value = ns("tab_map"),
        layout_column_wrap(
          width = 1 / 2,
          card(
            full_screen = FALSE,
            card_header("Select an ICES ecoregion:"),
            tags$style(type = "text/css", "#map {margin-left: auto; margin-right: auto; margin-bottom: auto;  max-width: 97%; height: auto;}"),
            withSpinner(leafletOutput(ns("map"), width = "90%")),
            tags$style(type = "text/css", "#selected_locations {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
            card_body(
              min_height = 400,
              virtualSelectInput(
                inputId = ns("selected_locations"),
                label = "Selected ICES Ecoregion:",
                choices = sort(eco_shape$Ecoregion),
                selected = "Greater North Sea",
                multiple = FALSE,
                width = "100%",
                search = TRUE,
                optionsCount = 11
              )
            )
          ),
          card(
            card_header("Select a topic:"),
            # Let tooltips overflow outside the card body
            card_body(
              style = "overflow: visible;",
              fluidRow(
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button-wrap",
                    actionLink(
                      ns("overviewBtn"),
                      label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/overview.svg');",
                      title = "Overview", `aria-label` = "Overview"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Overview</strong><br><br>Key signals and detailed <br>catch-by-country information"))
                  )
                ),
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button-wrap",
                    actionLink(
                      ns("landingsBtn"),
                      label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/landings.svg');",
                      title = "Landings", `aria-label` = "Landings"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Landings</strong><br><br>Landings over time:<br>by country, species, fish guild, and gear type"))
                  )
                )),
              fluidRow(
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button-wrap",
                    actionLink(
                      ns("stockStatusBtn"),
                      label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/Stock Status.svg');",
                      title = "Stock status", `aria-label` = "Stock status"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Stock status</strong><br><br>Relative to MSY &amp; PA reference points, and MSFD descriptors"))
                  )
                ),
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button-wrap",
                    actionLink(
                      ns("vmsBtn"),
                      label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/vms.svg');",
                      title = "VMS", `aria-label` = "VMS"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>VMS</strong><br><br>Fishing Effort &amp; Benthic Impact"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}



#' Navigation / landing page server module
#'
#' Server-side logic for the fisheriesXplorer landing page / navigation
#' screen. This module:
#' \itemize{
#'   \item renders the ICES ecoregion leaflet map,
#'   \item keeps the map selection and the virtual select input
#'         \code{selected_locations} in sync, and
#'   \item updates the parent navbar when the user clicks one of the
#'         topic image-buttons (Overview, Landings, Stock status).
#' }
#'
#' It can optionally consume bookmarked query-string state via
#' \code{bookmark_qs} to restore an overview subtab, although this
#' currently assumes that a tabset with id \code{"tabs_overview"} and
#' values \code{"exec_summary"}, \code{"introduction"}, or
#' \code{"who_is_fishing"} exists elsewhere in the app.
#'
#' @param id Module id string, used to create a namespace with
#'   \code{shiny::NS()}. Typically passed as a literal in
#'   \code{mod_navigation_page_server("navigation_page_1", ...)}.
#'
#' @param parent_session The parent (top-level) \link[shiny]{session}
#'   object. This is used to switch navbar tabs via
#'   \code{\link[shiny]{updateNavbarPage}()} when the user clicks one
#'   of the topic buttons.
#'
#' @param selected_ecoregion A reactive value object (e.g. created
#'   with \code{shiny::reactiveVal()}) that will be updated by the
#'   module whenever the user selects an ICES ecoregion, either by
#'   clicking on the map or by using the \code{selected_locations}
#'   virtual select input. The current ecoregion name is written as a
#'   single character string.
#'
#' @param bookmark_qs A reactive expression returning either
#'   \code{NULL} (no bookmarked state) or a list of query-string
#'   values, typically including a \code{subtab} element. This is used
#'   once at initialisation time to restore the overview subtab, if a
#'   valid subtab value is provided. Defaults to
#'   \code{reactive(NULL)}.
#'
#' @return No return value; this function is called for its side
#'   effects (rendering the map, updating the selected ecoregion,
#'   and navigating the parent navbar).
#'
#' @details
#' This module assumes that the following objects / functions are
#' available in the package namespace:
#' \itemize{
#'   \item \code{eco_shape}, \code{map_shape}: spatial data used by
#'         \code{map_ecoregion()} to draw the leaflet map.
#'   \item \code{map_ecoregion()}: a helper that returns a configured
#'         \code{leaflet} map for the ICES ecoregions.
#' }
#' When the user selects an area on the map, the corresponding
#' ecoregion is:
#' \enumerate{
#'   \item set as the current choice in \code{selected_locations}, and
#'   \item propagated to \code{selected_ecoregion()} for use by other
#'         modules.
#' }
#'
#' @importFrom shiny moduleServer reactive observeEvent req updateNavbarPage
#'   updateTabsetPanel
#' @importFrom leaflet renderLeaflet leafletProxy hideGroup showGroup
#' @importFrom shinyWidgets updateVirtualSelect
#' @noRd
mod_navigation_page_server <- function(
  id, 
  parent_session, 
  selected_ecoregion,
  bookmark_qs = reactive(NULL)) {
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
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "tabs_overview", selected = wanted)
          isolate(set_subtab(wanted)) # one-arg setter
        }, once = TRUE)
      }
    })

    output$map <- leaflet::renderLeaflet({
      map_ecoregion(eco_shape, map_shape)
    })

    
    proxy_map <- leaflet::leafletProxy("map", session = session)

    selected_map <- reactiveValues(groups = character())

    observeEvent(input$map_shape_click, {
      req(!is.null(input$map_shape_click$id))

      if (identical(input$map_shape_click$group, "Eco_regions")) {
        selected_map$groups <- c(selected_map$groups, input$map_shape_click$id)
      }

      
      updateVirtualSelect(
        inputId = "selected_locations",
        choices = eco_shape$Ecoregion,
        selected = input$map_shape_click$id,
        session = session
      )
    })

    observeEvent(input$selected_locations, {
      removed <- setdiff(selected_map$groups, input$selected_locations)
      selected_map$groups <- input$selected_locations

      proxy_map %>%
        leaflet::hideGroup(removed) %>%
        leaflet::showGroup(input$selected_locations)
    }, ignoreNULL = FALSE)

    observeEvent(input$selected_locations, {
      selected_ecoregion(input$selected_locations)
    })

    # Top-tab switches via actionLinks (by tab values)
    observeEvent(input$overviewBtn,   { updateNavbarPage(parent_session, "nav-page", selected = "overview") })
    observeEvent(input$landingsBtn,   { updateNavbarPage(parent_session, "nav-page", selected = "landings") })
    observeEvent(input$stockStatusBtn,{ updateNavbarPage(parent_session, "nav-page", selected = "stock_status") })
    observeEvent(input$vmsBtn,{ updateNavbarPage(parent_session, "nav-page", selected = "vms") })
  })
}

