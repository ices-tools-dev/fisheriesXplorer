#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header card_body 
#' @importFrom leaflet leafletOutput leafletProxy hideGroup showGroup 
#' @import leaflet 
mod_navigation_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      tags$style(type = "text/css", "#logo {height: auto; width: 30% !important; margin-top: 0px;  padding-bottom: 20px; }"),
      tags$img(id = "logo", class = "center-block", src = "www/fisheriesXplorer blue.png")
      # card_header("Check out the FisheriesXplorer app! Select your area of interest!")
    ),
    tabsetPanel(
      type = "hidden",
      id = ns("landing_page"),
      tabPanel("Map Tab",
        value = ns("tab_map"),
        layout_column_wrap(
          width = 1 / 3,
          card(
            full_screen = TRUE,
            card_header("Select an ecoregion"),
            tags$style(type = "text/css", "#map {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
            leafletOutput(ns("map"), width = "90%")
          ),
          card(
            full_screen = TRUE,
            card_header("Filtering options"),
            virtualSelectInput(
              inputId = ns("selected_locations"),
              label = "Case study regions:",
              choices = c("", sort(eco_shape$Ecoregion)),
              selected = NULL,
              multiple = FALSE,
              width = "100%",
              search = TRUE
            ),
            virtualSelectInput(
              inputId = ns("assessmentYear"),
              label = "Assessment year:",
              choices = c(2024, 2022),
              selected = NULL,
              multiple = FALSE,
              width = "100%",
              search = TRUE
            )
          ),
          card(
            card_header("Choose a topic:"),
            card_body(
              fluidRow(
            column(
              6,
              actionButton(
                inputId = ns("overview-btn"),
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image1.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            ),
            column(
              6,
              actionButton(
                inputId = ns("landings-btn"),
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image2.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            )
          ),
          fluidRow(
            column(
              6,
              actionButton(
                inputId = ns("stock_status-btn"),
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image3.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            ),
            column(
              6,
              actionButton(
                inputId = ns("mixfish-btn"),
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image4.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            )
          ),
          fluidRow(
            column(
              6,
              actionButton(
                inputId = ns("vms-btn"),
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image5.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            ),
            column(
              6,
              actionButton(
                inputId = ns("bycatch-btn"),
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image6.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            )
          )
            )
            # card("Overview", id = ns("overview-btn"), class = "btn action-button"),
            # card("Landings", id = ns("landings-btn"), class = "btn action-button"),
            # card("Stock status", id = ns("stock_status-btn"), class = "btn action-button"),
            # card("Mixed Fisheries", id = ns("mixfish-btn"), class = "btn action-button"),
            # card("VMS", id = ns("vms-btn"), class = "btn action-button"),
            # card("Bycatch", id = ns("bycatch-btn"), class = "btn action-button")
          )
        )
      ) # , tabPanel("Next Topic",
      #   value = ns("tab_topic"),
      #   card("Overview", id = ns("overview-btn"), class = "btn action-button"),
      #   card("Landings", id = ns("landings-btn"), class = "btn action-button"),
      #   card("Stock status", id = ns("stock_status-btn"), class = "btn action-button"),
      #   card("Mixed Fisheries", id = ns("mixfish-btn"), class = "btn action-button"),
      #   card("VMS", id = ns("vms-btn"), class = "btn action-button"),
      #   card("Bycatch", id = ns("bycatch-btn"), class = "btn action-button")
      # )
    )
  )
}
    
#' landing_page Server Functions
#'
#' @noRd 
mod_navigation_page_server <- function(id, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    print(getwd())
    output$map <- leaflet::renderLeaflet({
      
      map_ecoregion(eco_shape, map_shape)
    })
    
    proxy_map <- leafletProxy("map", parent_session)

    # create empty vector to hold all click ids
    selected_1 <- reactiveValues(groups = vector())
    
    # find index
    observeEvent(input$map_shape_click, {
      req(!is.null(input$map_shape_click$id))
      if (input$map_shape_click$group == "Eco_regions") {
        selected_1$groups <- c(selected_1$groups, input$map_shape_click$id)
        proxy_map %>%
          showGroup(group = input$map_shape_click$id)
      }
      
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           choices = eco_shape$Ecoregion,
                           selected = selected_1$groups)
    })
    
    
    observeEvent(input$selected_locations,{
      
      req(input$selected_locations!= "")
                   removed_via_selectInput <- setdiff(selected_1$groups, input$selected_locations)
                   added_via_selectInput <- setdiff(input$selected_locations, selected_1$groups)
                   
                   if (length(removed_via_selectInput) > 0) {
                     selected_1$groups <- input$selected_locations
                     
                     proxy_map %>% hideGroup(group = removed_via_selectInput)
                   }
                   
                   if (length(added_via_selectInput) > 0) {
                     selected_1$groups <- input$selected_locations
                     
                     proxy_map %>% showGroup(group = added_via_selectInput)
                     
                   }
                   
                   updateTabsetPanel(session = session, "landing_page", selected = ns("tab_topic"))
                 },
                 ignoreNULL = FALSE
    )
    
    
    observeEvent(input[["overview-btn"]],{
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Overview")
    })
    observeEvent(input[["landings-btn"]],{
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Landings")
    })
    observeEvent(input[["stock_status-btn"]],{
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Stock Status")
    })
    observeEvent(input[["mixfish-btn"]],{
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Mixed Fisheries")
    })
    observeEvent(input[["vms-btn"]],{
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "VMS")
    })
    observeEvent(input[["bycatch-btn"]],{
      updateTabsetPanel(session, "landing_page", selected = ns("tab_map"))
      updateNavbarPage(session = parent_session, "nav-page", selected = "Bycatch")
    })
    
  })
}
    
## To be copied in the UI
# mod_navigation_page_ui("navigation_page_1")
    
## To be copied in the server
# mod_navigation_page_server("navigation_page_1")
