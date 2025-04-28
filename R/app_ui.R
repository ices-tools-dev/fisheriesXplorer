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
    options(spinner.type = 5, 
        spinner.color = "#00B6F1",
        spinner.size = 0.7),
    
    navbarPage(
      title = title_html,
      position = "static-top",
      collapsible = TRUE,
      fluid = TRUE,
      windowTitle = "fisheriesXplorer",
      id = "nav-page",
      tabPanel("Home",
        id = "home",
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
      tabPanel("Mixed Fisheries",
              #  mod_mixfish_ui("mixfish_1")
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
      )
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
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fisheriesXplorer"
    ),
    # tags$head(includeHTML(("google-analytics.html"))),
    tags$style(HTML("
    #custom_slider .shiny-input-container {
      margin-top: 0px !important;  /* Remove top margin */
    }"
    )),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    
    tags$style("body {font-family: 'Gothic A1', sans-serif;}"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    # Add here other external resources
    useShinyjs()
    
  )
}
