#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header card_body layout_column_wrap 
#' @importFrom leaflet leafletOutput leafletProxy hideGroup showGroup 
#' @importFrom shinyWidgets virtualSelectInput updateVirtualSelect
#' @importFrom shinyjs onclick
mod_navigation_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$img(id = "logo", class = "center-block", src = "www/fisheriesxplorer_blue.png"),
    br(),br(),
    tabsetPanel(
      type = "hidden",
      id = ns("landing_page"),
      tabPanel("Map Tab",
        value = ns("tab_map"),
        layout_column_wrap(
          width = 1 / 2,
          card(
            full_screen = TRUE,
            card_header("Select an ecoregion"),
            tags$style(type = "text/css", "#map {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
            withSpinner(
              leafletOutput(ns("map"), width = "90%")
            ),
            tags$style(type = "text/css", "#selected_locations {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
            card_body(
              min_height = 400,
              virtualSelectInput(
                inputId = ns("selected_locations"),
                label = "Selected Ecoregion:",
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
            card_body(
              fluidRow(
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button", id = ns("overviewBtn"),
                    style = "background-image: url('www/overview.svg');",
                    div(class = "tooltip", "Overview")
                  )
                ),
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button", id = ns("landingsBtn"),
                    style = "background-image: url('www/landings.svg');",
                    div(class = "tooltip", "Landings")
                  )
                ),
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button", id = ns("stockStatusBtn"),
                    style = "background-image: url('www/stock_status.svg');",
                    div(class = "tooltip", "Stock status")
                  )
                )
              ),
              fluidRow(
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button", id = ns("mixfishBtn"),
                    style = "background-image: url('www/mix_fishieries.svg');",
                    div(class = "tooltip", "Mixed Fisheries")
                  )
                ),
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button", id = ns("VMS"),
                    style = "background-image: url('www/vms.svg');",
                    div(class = "tooltip", "VMS")
                  )
                ),
                column(
                  4,
                  align = "center",
                  div(
                    class = "image-button", id = ns("bycatchBtn"),
                    style = "background-image: url('www/bycatch.svg');",
                    div(class = "tooltip", "Bycatch")
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
    
#' landing_page Server Functions
#'
#' @noRd 
mod_navigation_page_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({
      map_ecoregion(eco_shape, map_shape)
    })
    proxy_map <- leafletProxy("map")

    # create empty character vector to hold map selected locations
    selected_map <- reactiveValues(groups = character())

    observeEvent(input$map_shape_click, {
      req(!is.null(input$map_shape_click$id))

      if (input$map_shape_click$group == "Eco_regions") {
        selected_map$groups <- c(selected_map$groups, input$map_shape_click$id)
      }

      updateVirtualSelect(
        inputId = "selected_locations",
        choices = eco_shape$Ecoregion,
        selected = input$map_shape_click$id
      )
    })

    observeEvent(input$selected_locations,
      {
        removed <- setdiff(selected_map$groups, input$selected_locations)
        selected_map$groups <- input$selected_locations

        proxy_map %>%
          hideGroup(removed) %>%
          showGroup(input$selected_locations)
      },
      ignoreNULL = FALSE
    )

    onclick("overviewBtn", expr = {
      updateNavbarPage(session = parent_session, "nav-page", selected = "Overview")
    })
    onclick("landingsBtn", expr = {
      updateNavbarPage(session = parent_session, "nav-page", selected = "Landings")
    })
    onclick("stockStatusBtn", expr = {
      updateNavbarPage(session = parent_session, "nav-page", selected = "Stock Status")
    })
    onclick("mixfishBtn", expr = {
      updateNavbarPage(session = parent_session, "nav-page", selected = "Mixed Fisheries")
    })
    onclick("VMS", expr = {
      updateNavbarPage(session = parent_session, "nav-page", selected = "VMS")
    })
    onclick("bycatchBtn", expr = {
      updateNavbarPage(session = parent_session, "nav-page", selected = "Bycatch")
    })
  })
}
    
## To be copied in the UI
# mod_navigation_page_ui("navigation_page_1")
    
## To be copied in the server
# mod_navigation_page_server("navigation_page_1")
