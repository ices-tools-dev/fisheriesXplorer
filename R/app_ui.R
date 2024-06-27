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
      href = "https://ices-taf.shinyapps.io/fisheriesxplorer/",
      tags$img(
        src = "www/NEGATIVE ICES-logo.png",
        style = "margin-top: -15px; margin-bottom: 0px; padding-right:10px;",
        height = "50px"
      )
    ),
    navbarPage(
      title = title_html,
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
      tabPanel("Mixed Fisheries"),
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
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fisheriesXplorer"
    ),
    # tags$head(includeHTML(("google-analytics.html"))),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/gothic-a1.css"),
    tags$style("body {font-family: 'Gothic A1', sans-serif;}"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
