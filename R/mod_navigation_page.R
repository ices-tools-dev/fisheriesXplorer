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
    # --- Safe tooltip CSS (no BS5 clash) ---
    # tags$style(HTML("
    #   .image-button-wrap { position: relative; display: inline-block; }
    #   .image-button-link { display:block; width:140px; height:140px; background-size:contain; background-repeat:no-repeat; cursor:pointer; outline:0; text-decoration:none; }
    #   .image-button-wrap .fx-tooltip {
    #     position: absolute;
    #     left: 50%;
    #     transform: translateX(-50%) translateY(6px);
    #     bottom: -6px;
    #     background: rgba(0,0,0,0.85);
    #     color: #fff;
    #     padding: 8px 10px;
    #     border-radius: 6px;
    #     white-space: normal;
    #     width: 240px;
    #     max-width: 260px;
    #     font-size: 0.9rem;
    #     line-height: 1.25;
    #     opacity: 0;
    #     pointer-events: none;
    #     transition: opacity .15s ease, transform .15s ease;
    #     z-index: 9999;
    #     box-shadow: 0 4px 16px rgba(0,0,0,.2);
    #   }
    #   .image-button-wrap:hover .fx-tooltip,
    #   .image-button-wrap:focus-within .fx-tooltip {
    #     opacity: 1;
    #     transform: translateX(-50%) translateY(0);
    #   }
    # ")),

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
            # Let tooltips overflow outside the card body
            card_body(
              style = "overflow: visible;",
              fluidRow(
                column(
                  4, align = "center",
                  div(class = "image-button-wrap",
                    actionLink(
                      ns("overviewBtn"), label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/overview.svg');",
                      title = "Overview", `aria-label` = "Overview"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Overview</strong><br><br>Key signals and detailed <br>catch-by-country information"))
                  )
                ),
                column(
                  4, align = "center",
                  div(class = "image-button-wrap",
                    actionLink(
                      ns("landingsBtn"), label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/landings.svg');",
                      title = "Landings", `aria-label` = "Landings"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Landings</strong><br><br>Landings over time:<br>by country, species, fish guild, and gear type"))
                  )
                ),
                column(
                  4, align = "center",
                  div(class = "image-button-wrap",
                    actionLink(
                      ns("stockStatusBtn"), label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/stock_status.svg');",
                      title = "Stock status", `aria-label` = "Stock status"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Stock status</strong><br><br>Relative to MSY &amp; PA reference points, and MSFD descriptors"))
                  )
                )
              ),
              fluidRow(
                column(
                  4, align = "center",
                  div(class = "image-button-wrap",
                    actionLink(
                      ns("mixfishBtn"), label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/mix_fishieries.svg');",
                      title = "Mixed Fisheries", `aria-label` = "Mixed Fisheries"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Mixed Fisheries</strong><br><br>Technical interactions across the main fisheries"))
                  )
                ),
                column(
                  4, align = "center",
                  div(class = "image-button-wrap",
                    actionLink(
                      ns("VMS"), label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/vms.svg');",
                      title = "VMS", `aria-label` = "VMS"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>VMS</strong><br><br>Effort distribution and physical disturbance of benthic habitats"))
                  )
                ),
                column(
                  4, align = "center",
                  div(class = "image-button-wrap",
                    actionLink(
                      ns("bycatchBtn"), label = NULL, class = "image-button-link",
                      style = "background-image: url('www/icons/bycatch.svg');",
                      title = "Bycatch", `aria-label` = "Bycatch"
                    ),
                    div(class = "fx-tooltip", HTML("<strong>Bycatch</strong><br><br>Protected, endangered, and threatened species"))
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

