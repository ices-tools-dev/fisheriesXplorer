#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    navbarPage(title = "FisheriesXplorer", id = "nav-page",
               tabPanel("Home", id = "home",
                        mod_landing_page_ui("landing_page_1")),
               tabPanel("Home2", id = "home2", 
                        mod_landing_page_ui("landing_page_2")),
               tabPanel("Mixed Fisheres"),
               tabPanel("Bycatch"),
               tabPanel("Stock Status",
                        mod_stock_status_ui("stock_status_1")),
               tabPanel("VMS"),
               tabPanel("Other")
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
