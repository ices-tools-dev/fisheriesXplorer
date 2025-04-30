mod_mixfish_plot_selection_ui <- function(id) {
    # ns <- NS(id)
    # tagList(
    #     selectInput(ns("plotType"), "Choose a plot:",
    #         choices = c(
    #             "Histogram" = "hist",
    #             "Boxplot" = "boxplot",
    #             "Scatterplot" = "scatter"
    #         )
    #     )                
    # )
    ns <- NS(id)
  tagList(
    selectInput(ns("subRegion"), 
        "Select sub-region", 
        choices = c("Bay of Biscay", "Iberian waters", "Celtic Sea", "Irish Sea"), 
        selected = "Bay of Biscay"),
    actionButton(ns("plot1"), "Headline"),
    actionButton(ns("plot2"), "Composition")
    # actionButton(ns("plot3"), "Show FLStock"),
    # actionButton(ns("plot4"), "Show FLSR"),
  )
}


mod_mixfish_plot_selection_server <- function(id, selected_ecoregion) {
    moduleServer(id, function(input, output, session) {
    
    
    selected_subRegion <- reactiveVal(NULL)
    selected_plot <- reactiveVal(NULL)

    observeEvent(input$subRegion, {
        selected_subRegion(input$subRegion)
    })

    observeEvent(input$plot1, {
      selected_plot("plot1")
    })

    observeEvent(input$plot2, {
      selected_plot("plot2")
    })
    ## ← new: watch ecoregion and update subRegion choices
    shiny::observeEvent(selected_ecoregion(), {
      eco <- selected_ecoregion()
      acr <- get_ecoregion_acronym(eco)
      
      new_choices <- switch(acr,
        "CS" = c("Celtic Sea","Irish Sea"),
        "BI" = c("Bay of Biscay","Iberian Waters"),
        ## otherwise, just offer the full ecoregion name itself
        eco
      )
      
      ## update the namespaced selectInput
      shiny::updateSelectInput(
        session, 
        "subRegion", 
        choices = new_choices, 
        selected = new_choices[1]
      )
    }, ignoreNULL = TRUE)
    # observeEvent(input$plot3, {
    #   selected_plot("plot3")
    # })

    # observeEvent(input$plot4, {
    #   selected_plot("plot4")
    # })
    list(
      plot_choice = selected_plot,
      sub_region  = selected_subRegion
    )
    # return(selected_plot)
})
}