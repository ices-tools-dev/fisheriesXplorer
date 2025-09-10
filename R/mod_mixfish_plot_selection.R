# mod_mixfish_plot_selection_ui <- function(id) {
#   ns <- NS(id)  

#   tagList(
#     uiOutput(ns("subregion_ui")), # Placeholder for conditional UI

#     shinyWidgets::radioGroupButtons(
#       inputId = ns("tab_selected"),
#       label = "Select a plot:",
#       selected = "plot1",
#       direction = "vertical",
#       choiceNames = list(
#         make_tooltip_choice("Scenarios", "<strong>Scenarios</strong><br>Main highlights and signals"),
#         make_tooltip_choice("Effort By Fleet & Stock", "<strong>Effort</strong><br>Effort by fleet and stock"),
#         make_tooltip_choice("Landings By Métier & Stock", "<strong>Landings</strong><br>by fleet and stock"),
#         make_tooltip_choice("Landings By Stock", "<strong>Landings</strong><br>Aggregated by stock"),
#         make_tooltip_choice("Landings compositions by fleet", "<strong>Composition</strong><br>Species-level breakdown")
#       ),
#       choiceValues = c("plot1", "plot2", "plot3", "plot4", "plot5")
#     )
#   )
# }


# mod_mixfish_plot_selection_ui <- function(id) {
#   ns <- NS(id)  
  
#   tagList(
#     uiOutput(ns("subregion_ui")), # Placeholder for conditional UI
    
#     tags$h5("Data"),
#     shinyWidgets::radioGroupButtons(
#       inputId = ns("tab_selected_data"),
#       label = NULL,
#       selected = "plot1",
#       direction = "vertical",
#       choiceNames = list(
#         make_tooltip_choice("Scenarios", "<strong>Scenarios</strong><br>Main highlights and signals"),
#         make_tooltip_choice("Effort By Fleet & Stock", "<strong>Effort</strong><br>Effort by fleet and stock"),
#         make_tooltip_choice("Landings By Métier & Stock", "<strong>Landings</strong><br>by fleet and stock")
#       ),
#       choiceValues = c("plot1", "plot2", "plot3")
#     ),
    
#     tags$h5("Analysis"),
#     shinyWidgets::radioGroupButtons(
#       inputId = ns("tab_selected_analysis"),
#       label = NULL,
#       direction = "vertical",
#       choiceNames = list(
#         make_tooltip_choice("Landings By Stock", "<strong>Landings</strong><br>Aggregated by stock"),
#         make_tooltip_choice("Landings compositions by fleet", "<strong>Composition</strong><br>Species-level breakdown")
#       ),
#       choiceValues = c("plot4", "plot5")
#     )
#   )
# }
mod_mixfish_plot_selection_ui <- function(id) {
  ns <- NS(id)  

  tagList(
    uiOutput(ns("subregion_ui")),

    tags$h5(HTML("<b>Data</b>")),
    shinyWidgets::radioGroupButtons(
      inputId = ns("tab_selected_data"),
      label = NULL,
      direction = "vertical",
      choiceNames = list(
        
        
        make_tooltip_choice("Landings By Métier & Stock", "<strong>Landings</strong><br>by fleet and stock"),
        make_tooltip_choice("Landings By Stock", "<strong>Landings</strong><br>Aggregated by stock"),
        make_tooltip_choice("Landings compositions by fleet", "<strong>Composition</strong><br>Species-level breakdown")
      ),
      choiceValues = c("plot3", "plot4", "plot5"),
    # ),
      selected = character(0)   # ✅ start with none selected
    ),

    tags$h5(HTML("<b>Analysis</b>")),
    shinyWidgets::radioGroupButtons(
      inputId = ns("tab_selected_analysis"),
      label = NULL,
      direction = "vertical",
      choiceNames = list(
        make_tooltip_choice("Scenarios", "<strong>Scenarios</strong><br>Main highlights and signals"),
        make_tooltip_choice("Effort By Fleet & Stock", "<strong>Effort</strong><br>Effort by fleet and stock")
        
      ),
      choiceValues = c("plot1", "plot2"),
      selected = "plot1"   # ✅ default
    )
  )
}


mod_mixfish_plot_selection_server <- function(id, selected_ecoregion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_subRegion <- reactiveVal(NULL)
    selected_plot <- reactiveVal("plot1")

    # Conditionally render subregion selectInput based on ecoregion
    output$subregion_ui <- renderUI({
      req(selected_ecoregion())
      acr <- get_ecoregion_acronym(selected_ecoregion())

      if (acr %in% c("CS", "BI")) {
        new_choices <- switch(acr,
          "CS" = c("Celtic Sea", "Irish Sea"),
          "BI" = c("Bay of Biscay", "Iberian Waters")
        )

        # Update reactive value to the first sub-region when UI is drawn
        selected_subRegion(new_choices[1])

        selectInput(ns("subRegion"),
          "Select case study",
          choices = new_choices,
          selected = new_choices[1]
        )
      } else {
        selected_subRegion(NULL) # Reset if no subregion relevant
        NULL # No UI shown
      }
    })

    observeEvent(input$subRegion, {
      selected_subRegion(input$subRegion)
    })


    # Data group
    observeEvent(input$tab_selected_data, {
      if (!is.null(input$tab_selected_data)) {
        selected_plot(input$tab_selected_data)
        shinyWidgets::updateRadioGroupButtons(session, "tab_selected_analysis", selected = character(0))
      }
    })

    # Analysis group
    observeEvent(input$tab_selected_analysis, {
      if (!is.null(input$tab_selected_analysis)) {
        selected_plot(input$tab_selected_analysis)
        shinyWidgets::updateRadioGroupButtons(session, "tab_selected_data", selected = character(0))
      }
    })

    # # Reset selected_plot when ecoregion changes
    # observeEvent(selected_ecoregion(), {
    #   # reset plot
    #   selected_plot("plot1")
    # })

    list(
      plot_choice = selected_plot,
      sub_region = selected_subRegion
    )
  })
}
