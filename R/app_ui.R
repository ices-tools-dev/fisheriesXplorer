#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom desc desc_get_version
#' @noRd
# =========================
# UI
# =========================
app_ui <- function(request) {
  
  tagList(
    # External resources
    golem_add_external_resources(),
    title_html <- tags$a(
      href = "https://ices-tools-dev.shinyapps.io/fisheriesXplorer/",
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

# Resources (same as your version)
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
          btn.setAttribute('title', 'Open/close sidebar for additional information');
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


