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
mod_navigation_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$img(id = "logo", class = "center-block", src = "www/fisheriesXplorer blue.png"),
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
                  6,
                  align = "center",
                  actionButton(
                    inputId = ns("overview-btn"),
                    label = NULL,
                    style = "background: url('www/research.png') no-repeat center center; background-size: cover; height: 150px; width: 150px;"
                  )
                ),
                column(
                  6,
                  align = "center",
                  actionButton(
                    inputId = ns("landings-btn"),
                    label = NULL,
                    style = "background: url('www/trend.png') no-repeat center center; background-size: cover; height: 150px; width: 150px;"
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  align = "center",
                  actionButton(
                    inputId = ns("stock_status-btn"),
                    label = NULL,
                    style = "background: url('www/check-list.png') no-repeat center center; background-size: cover; height: 150px; width: 150px;"
                  )
                ),
                column(
                  6,
                  align = "center",
                  actionButton(
                    inputId = ns("mixfish-btn"),
                    label = NULL,
                    style = "background: url('www/fishing-net.png') no-repeat center center; background-size: cover; height: 150px; width: 150px;"
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  align = "center",
                  actionButton(
                    inputId = ns("vms-btn"),
                    label = NULL,
                    style = "background: url('www/architecture.png') no-repeat center center; background-size: cover; height: 150px; width: 150px;"
                  )
                ),
                column(
                  6,
                  align = "center",
                  actionButton(
                    inputId = ns("bycatch-btn"),
                    label = NULL,
                    style = "background: url('www/dolphin.png') no-repeat center center; background-size: cover; height: 150px; width: 150px;"
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

    observeEvent(input$selected_locations,{      
        
      removed <- setdiff(selected_map$groups, input$selected_locations)
      selected_map$groups <- input$selected_locations
        
      proxy_map %>% 
        hideGroup(removed) %>% 
        showGroup(input$selected_locations)
        
      }, ignoreNULL = FALSE
    )


    observeEvent(input[["overview-btn"]], {
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Overview")
    })
    observeEvent(input[["landings-btn"]], {
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Landings")
    })
    observeEvent(input[["stock_status-btn"]], {
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Stock Status")
    })
    observeEvent(input[["mixfish-btn"]], {
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Mixed Fisheries")
    })
    observeEvent(input[["vms-btn"]], {
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "VMS")
    })
    observeEvent(input[["bycatch-btn"]], {
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Bycatch")
    })
  })
}
    
## To be copied in the UI
# mod_navigation_page_ui("navigation_page_1")
    
## To be copied in the server
# mod_navigation_page_server("navigation_page_1")
