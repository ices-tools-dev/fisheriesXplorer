#' The application user-interface
#'
#' Main UI definition for the \code{fisheriesXplorer} Shiny application.
#' Sets up external resources, global options (e.g. spinners), a
#' fullscreen helper script, and the top-level navigation bar with
#' module UIs for the different sections of the app.
#'
#' @param request Internal parameter for Shiny, typically a
#'   \code{shiny::Request} object. Required to support bookmarking and
#'   other advanced features, even if not used directly.
#'
#' @return
#' A \code{shiny.tag.list} containing the complete UI for the
#' application, including external resources and a \code{navbarPage}
#' with the main tabs.
#'
#' @details
#' The UI includes:
#' \itemize{
#'   \item A call to \code{golem_add_external_resources()} to register
#'     static resources (CSS, JS, images, etc.).
#'   \item A logo/title link pointing to the deployed app.
#'   \item Global options for \pkg{shinycssloaders} spinners.
#'   \item A JavaScript helper function \code{toggleFullScreen()} for
#'     entering/exiting fullscreen mode.
#'   \item A \code{navbarPage} with tabs for Home, Overview, Landings,
#'     Stock status, and Resources, each rendering the corresponding
#'     module UI.
#'   \item A right-aligned "Share" button (using \code{bslib::nav_spacer()}
#'     and \code{bslib::nav_item()}) that triggers share-link logic in
#'     the server.
#' }
#'
#' This function is typically passed to \code{shinyApp()} (or the golem
#' entry point) as the UI argument.
#'
#' @import shiny
#' @importFrom bslib nav_spacer nav_item
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # External resources
    golem_add_external_resources(),
    title_html <- tags$a(
      href = "https://ices-taf.shinyapps.io/fisheriesXplorer/",
      tags$img(
        src = "www/negative_ices_logo.png",
        style = "margin-top: -15px; margin-bottom: 0px; padding-right:10px;",
        height = "50px"
      )
    ),

    # {shinycssloaders} options (if you use them)
    options(
      spinner.type = 5,
      spinner.color = "#00B6F1",
      spinner.size = 0.7
    ),

    # Fullscreen helper
    tags$script(HTML("
      function toggleFullScreen(elem) {
        if (!document.fullscreenElement) {
          elem.requestFullscreen().catch(err => {
            alert('Error attempting to enable fullscreen: ' + err.message);
          });
        } else {
          document.exitFullscreen();
        }
      }
    ")),
    navbarPage(
      title = title_html,
      position = "static-top",
      collapsible = TRUE,
      fluid = TRUE,
      windowTitle = "fisheriesXplorer",
      id = "nav-page",
      # theme = bslib::bs_theme(version = 5),
      tabPanel("Home", value = "home", mod_navigation_page_ui("navigation_page_1")),
      tabPanel("Overview", value = "overview", mod_overview_ui("overview_1")),
      tabPanel("Landings", value = "landings", mod_landings_ui("landings_1")),
      tabPanel("Stock status", value = "stock_status", mod_stock_status_ui("stock_status_1")),
      
      # push right
      bslib::nav_spacer(),

      # Share button: use actionButton (NOT bookmarkButton)
      bslib::nav_item(
        actionButton("share_btn",
          label = "Share",
          icon = icon("link"),
          class = "btn btn-default",
          style = "margin-right: 8px;"
        )
      ),
      tabPanel(
        tagList("Resources"),
        value = "resources",
        mod_resources_ui("resources_1")
      )
    )
  )
}

#' Add external resources to the app
#'
#' Registers the \code{www} resource path and attaches common external
#' dependencies (favicon, Google Analytics snippet, bundled CSS/JS,
#' custom styles, Font Awesome, and clipboard helper) to the document
#' \code{<head>} for the \code{fisheriesXplorer} app.
#'
#' @return
#' A \code{shiny.tag} (a \code{<head>} element) to be included in the UI,
#' typically via \code{tagList(golem_add_external_resources(), ...)}.
#'
#' @details
#' This helper:
#' \itemize{
#'   \item Registers the \code{"www"} static resource path using
#'     \code{app_sys("app/www")}.
#'   \item Sets the favicon.
#'   \item Includes the Google Analytics HTML snippet.
#'   \item Bundles app-specific resources from \code{app/www}.
#'   \item Applies small CSS tweaks (e.g. for \code{#custom_slider}).
#'   \item Adds a JS snippet to set \code{title} attributes on elements
#'     with class \code{"collapse-toggle"}.
#'   \item Loads custom fonts, Font Awesome, the main stylesheet, and a
#'     clipboard helper script (\code{copy.js}).
#'   \item Enables \pkg{shinyjs}.
#' }
#'
#' Intended to be called once from \code{app_ui()}.
#'
#' @importFrom golem add_resource_path app_sys bundle_resources
#' @importFrom shiny tags includeHTML
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    tags$link(rel = "shortcut icon", href = "www/fishriesXplorer_PNG.png"),
    includeHTML(("R/google-analytics.html")),
    bundle_resources(path = app_sys("app/www"), app_title = "fisheriesXplorer"),
    tags$style(HTML("#custom_slider .shiny-input-container { margin-top: 0px !important; }")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('.collapse-toggle').forEach(btn => {
          btn.setAttribute('title', 'Open/close sidebar for figure captions and results description');
        });
      });
    ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
    tags$style("body {font-family: 'Gothic A1', sans-serif;}"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$script(src = "www/copy.js"),
    shinyjs::useShinyjs()
  )
}


