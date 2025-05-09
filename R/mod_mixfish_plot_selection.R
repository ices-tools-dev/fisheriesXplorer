
mod_mixfish_plot_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("subregion_ui")),  # Placeholder for conditional UI
    actionButton(ns("plot1"), "Headline"),
    actionButton(ns("plot2"), "Effort By Fleet/Stock"),
    actionButton(ns("plot3"), "Catch Composition")
    
  )
}


mod_mixfish_plot_selection_server <- function(id, selected_ecoregion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_subRegion <- reactiveVal(NULL)
    selected_plot <- reactiveVal(NULL)

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
          "Select sub-region",
          choices = new_choices,
          selected = new_choices[1]
        )
      } else {
        selected_subRegion(NULL)  # Reset if no subregion relevant
        NULL  # No UI shown
      }
    })

    observeEvent(input$subRegion, {
      selected_subRegion(input$subRegion)
    })

    observeEvent(input$plot1, {
      selected_plot("plot1")
    })

    observeEvent(input$plot2, {
      selected_plot("plot2")
    })

    observeEvent(input$plot3, {
      selected_plot("plot3")
    })
    
    observeEvent(selected_ecoregion(), {
      # reset plot
      selected_plot(NULL)
    })

    list(
      plot_choice = selected_plot,
      sub_region = selected_subRegion
    )
  })
}
