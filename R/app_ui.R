#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom desc desc_get_version
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    title_html <- tags$a(
      href = "https://ices-tools-dev.shinyapps.io/fisheriesXplorer/",
      tags$img(
        src = "www/negative_ices_logo.png",
        style = "margin-top: -15px; margin-bottom: 0px; padding-right:10px;",
        height = "50px"
      )
    ),
    options(
      spinner.type = 5,
      spinner.color = "#00B6F1",
      spinner.size = 0.7
    ),

    # Add js to support fullscreen display of images
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
      tabPanel("Home",
        value = "home",
        mod_navigation_page_ui("navigation_page_1")
      ),
      tabPanel(
        "Overview",
        mod_overview_ui("overview_1")
      ),
      tabPanel(
        "Landings",
        mod_landings_ui("landings_1")
      ),
      tabPanel(
        "Stock Status",
        mod_stock_status_ui("stock_status_1")
      ),
      tabPanel(
        "Mixed Fisheries",
        layout_sidebar(
          sidebar = sidebar(
            mod_mixfish_plot_selection_ui("mixfish_selection_1"),
            width = "20vw"
          ),
          mod_mixfish_plot_display_ui("mixfish_viz_1"),
        )
      ),
      tabPanel(
        "VMS",
        mod_vms_ui("vms_1")
      ),
      tabPanel(
        "Bycatch",
        mod_bycatch_ui("bycatch_1")
      ),

      ## ------------------ NEW: Resources dropdown (right-aligned) ------------------
      bslib::nav_spacer(),  # pushes everything that follows to the right (BS5)
      navbarMenu("Resources", align = "right", #id = "resources_menu"
        tabPanel(
          tagList(icon("envelope"), "Contact & Feedback"),
          fluidPage(
            h3("Contact & Feedback"),
            p("We’d love to hear from you. For questions, bug reports, or suggestions:"),
            tags$ul(
              tags$li(HTML("Email: <a href='mailto:your-team@example.org'>your-team@example.org</a>")),
              tags$li(HTML("Issue tracker (optional): <a href='#'>link to GitHub/issue tracker</a>"))
            ),
            p(em("Please include the ecoregion and a screenshot/permalink if reporting an issue."))
          )
        ),
        tabPanel(
          tagList(icon("database"), "Data Sources"),
          fluidPage(
            h3("Data Sources"),
            p("This application integrates multiple ICES data services and related sources."),
            tags$ul(
              tags$li(HTML("<b>SID</b> — Stock Information Database (used to resolve stock metadata).")),
              tags$li(HTML("<b>SAG</b> — Stock Assessment Graphs service (assessment outputs and indicators).")),
              tags$li(HTML("<b>Status service</b> — current stock status summaries.")),
              tags$li(HTML("<b>Landings</b> — compiled landings by stock/ecoregion.")),
              tags$li(HTML("<b>VMS</b> and <b>Bycatch</b> — specialised components drawing on internal/external services."))
            ),
            p(em("Exact endpoints and refresh cadences are documented in the code repository / deployment manifest."))
          )
        ),
        tabPanel(
          tagList(icon("exclamation-triangle"), "Data disclaimer & policy"),
          fluidPage(
            h3("Data disclaimer & policy"),
            p(HTML("This is a <b>development</b> deployment. Contents are indicative and should not be quoted or used elsewhere.")),
            p("Data are subject to change as assessments and monthly updates are released."),
            tags$ul(
              tags$li("Stock status updates: daily cadence (when upstream services publish)."),
              tags$li("Landings updates: annual cadence."),
              tags$li("Historical views can be reproduced via the app’s “Share this view” permalink once bookmarking is enabled.")
            ),
            p("Please consult ICES data policies for redistribution and citation guidance.")
          )
        ),
        tabPanel(
          tagList(icon("quote-right"), "Citation"),
          fluidPage(
            h3("Citation"),
            p("Suggested wording (adapt to your needs):"),
            tags$pre(
              "ICES (YEAR).
              fisheriesXplorer [Shiny application].
              URL: https://ices-tools-dev.shinyapps.io/fisheriesXplorer/
              Accessed: YYYY-MM-DD."
            ),
            p("When you share a specific view (with bookmarking enabled), include the permalink and, where available, the code/data DOIs and snapshot information.")
          )
        )
      )
      ## ---------------- end Resources dropdown ------------------------------------
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    tags$link(rel = "shortcut icon", href = "www/fishriesXplorer_PNG.png"),
    includeHTML(("R/google-analytics.html")),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fisheriesXplorer"
    ),
    tags$style(HTML("
    #custom_slider .shiny-input-container {
      margin-top: 0px !important;  /* Remove top margin */
    }")),
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
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

    # Add here other external resources
    useShinyjs()
  )
}
