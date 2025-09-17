#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header card_body layout_column_wrap nav_panel layout_sidebar sidebar
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
            card_header("Select an ICES ecoregion"),
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
            card_body(
              fluidRow(
                column(
                  4, align = "center",
                  # OVERVIEW
                  actionLink(
                    ns("overviewBtn"), label = NULL, class = "image-button",
                    style = "background-image: url('www/icons/overview.svg'); display:block; width:140px; height:140px; background-size:contain; background-repeat:no-repeat; cursor:pointer;",
                    title = "Overview"
                  ),
                  div(class = "tooltip", HTML("<strong>Overview</strong><br><br>Key signals and detailed <br> catch-by-country <br> information<br><br>"))
                ),
                column(
                  4, align = "center",
                  # LANDINGS
                  actionLink(
                    ns("landingsBtn"), label = NULL, class = "image-button",
                    style = "background-image: url('www/icons/landings.svg'); display:block; width:140px; height:140px; background-size:contain; background-repeat:no-repeat; cursor:pointer;",
                    title = "Landings"
                  ),
                  div(class = "tooltip", HTML("<strong>Landings</strong><br><br>Landings over time:<br> by country, species, fish guild, and gear type<br><br>"))
                ),
                column(
                  4, align = "center",
                  # STOCK STATUS
                  actionLink(
                    ns("stockStatusBtn"), label = NULL, class = "image-button",
                    style = "background-image: url('www/icons/stock_status.svg'); display:block; width:140px; height:140px; background-size:contain; background-repeat:no-repeat; cursor:pointer;",
                    title = "Stock status"
                  ),
                  div(class = "tooltip", HTML("<strong>Stock status</strong><br><br>Stock status relative to MSY and PA reference points, and MSFD descriptors<br><br>"))
                )
              ),
              fluidRow(
                column(
                  4, align = "center",
                  actionLink(
                    ns("mixfishBtn"), label = NULL, class = "image-button",
                    style = "background-image: url('www/icons/mix_fishieries.svg'); display:block; width:140px; height:140px; background-size:contain; background-repeat:no-repeat; cursor:pointer;",
                    title = "Mixed Fisheries"
                  ),
                  div(class = "tooltip", HTML("<strong>Mixed Fisheries</strong><br><br>Overview of the Technical Interactions across the main fisheries<br><br>"))
                ),
                column(
                  4, align = "center",
                  actionLink(
                    ns("VMS"), label = NULL, class = "image-button",
                    style = "background-image: url('www/icons/vms.svg'); display:block; width:140px; height:140px; background-size:contain; background-repeat:no-repeat; cursor:pointer;",
                    title = "VMS"
                  ),
                  div(class = "tooltip", HTML("<strong>VMS</strong><br><br>Spatial distribution of fishing effort, and maps of the physical disturbance of benthic habitats by fishing gear<br><br>"))
                ),
                column(
                  4, align = "center",
                  actionLink(
                    ns("bycatchBtn"), label = NULL, class = "image-button",
                    style = "background-image: url('www/icons/bycatch.svg'); display:block; width:140px; height:140px; background-size:contain; background-repeat:no-repeat; cursor:pointer;",
                    title = "Bycatch"
                  ),
                  div(class = "tooltip", HTML("<strong>Bycatch</strong><br><br>Bycatch of protected, endangered, and threatened species<br><br>"))
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
mod_navigation_page_server <- function(id, parent_session, selected_ecoregion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
    observeEvent(input$mixfishBtn,    { updateNavbarPage(parent_session, "nav-page", selected = "mixed_fisheries") })
    observeEvent(input$VMS,           { updateNavbarPage(parent_session, "nav-page", selected = "vms") })
    observeEvent(input$bycatchBtn,    { updateNavbarPage(parent_session, "nav-page", selected = "bycatch") })
  })
}

