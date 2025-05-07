mod_mixfish_plot_display_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "80vh", full_screen = TRUE, fill = FALSE,
    card_header("Filter by scenario and stock"),
    card_body(
      fillable = TRUE, fill = TRUE, class = "p-1",
      uiOutput(ns("filter_ui")),  
      withSpinner(
        plotlyOutput(ns("plot"), height = "65vh"),
        caption = "Getting mix-fish results..."
      )
    )
  )
}



mod_mixfish_plot_display_server <- function(id, plot_name, selected_ecoregion, selected_subRegion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # data_reactive_all <- reactive({
    #   req(selected_ecoregion())
    #   get_active_region_acronym <- function(subregion, ecoregion) {
    #     region <- if (!is.null(subregion)) subregion else ecoregion
    #     get_ecoregion_acronym(region)
    #   }
    #   # sReg <- selected_subRegion()
    #   #   ecoR <- selected_ecoregion()
    #   eco_acronym <- get_active_region_acronym(selected_subRegion(), selected_ecoregion())
    #   print("Running data_reactive_all")
    #   browser()
    #   list(
    #     catchScenarioStk_filtered = catchScenarioStk %>% filter(ecoregion == eco_acronym),
    #     catchRange_filtered = catchRange %>% filter(ecoregion == eco_acronym),
    #     refTable_filtered = refTable %>% filter(ecoregion == eco_acronym)
    #   )
    # })
    data_reactive_all <- reactive({
      req(selected_ecoregion())
      eco_acronym <- get_active_region_acronym(selected_subRegion(), selected_ecoregion())

      validate(
        need(eco_acronym %in% catchScenarioStk$ecoregion, "Invalid ecoregion filter.")
      )

      print(paste("Filtering using acronym:", eco_acronym))
      
      # catchScenarioStk_filtered <- catchScenarioStk %>% filter(ecoregion == eco_acronym)
      # catchRange_filtered <- catchRange %>% filter(ecoregion == eco_acronym)
      # refTable_filtered <- refTable %>% filter(ecoregion == eco_acronym)
      
      # list(
      #   catchScenarioStk_filtered = catchScenarioStk_filtered,
      #   catchRange_filtered = catchRange_filtered,
      #   refTable_filtered = refTable_filtered
      # )
      list(
        catchScenarioStk_filtered = catchScenarioStk %>% filter(ecoregion == eco_acronym),
        catchRange_filtered       = catchRange %>% filter(ecoregion == eco_acronym),
        refTable_filtered         = refTable %>% filter(ecoregion == eco_acronym)
      )
    })

    dataComp <- reactive({
      req(plot_name())
      library(mixfishtools)
      data(stfMtStkSum)
      print("Running dataComp")
      list(stfMtStkSum = stfMtStkSum)
    })

    # Dynamically render the correct filter UI
    output$filter_ui <- renderUI({
      req(plot_name())
      print("Rendering filter_ui")
      if (plot_name() == "plot1") {
        select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            scenario = list(inputId = "scenario", label = "Management Scenario:", placeholder = "Select scenario"),
            stock = list(inputId = "stock", label = "Fish Stock", placeholder = "Select stock")
          )
        )
      } else if (plot_name() == "plot2") {
        select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            year = list(inputId = "year", label = "Year", placeholder = "Select year"),
            fleet = list(inputId = "fleet", label = "Fleet", placeholder = "Select fleet")
          )
        )
      }
    })

    data_filter_module <- select_group_server(
      id = "my-filters-mixfish",
      data_r = reactive({
        print("Running data_filter_module")
        req(plot_name())
        if (plot_name() == "plot1") {
          data_reactive_all()$catchScenarioStk_filtered
        } else if (plot_name() == "plot2") {
          dataComp()$stfMtStkSum
        }
      }),
      vars_r = reactive({
        req(plot_name())
        print("Running vars_r")
        if (plot_name() == "plot1") {
          c("scenario", "stock")
        } else if (plot_name() == "plot2") {
          c("year", "fleet")
        }
      })
    )



    output$plot <- renderPlotly({
      req(plot_name())
      # df <- data_filter_module()
      # req(df)
      print("Rendering plot")
      # data_reactive_all <- data_reactive_all()
      switch(plot_name(),
        "plot1" = plot_catchScenStk_plotly(
          data = data_filter_module(),
          adv = data_reactive_all()$catchRange_filtered,
          refTable = data_reactive_all()$refTable_filtered
        ),
        "plot2" = plot_catchComp_plotly(
          dataComposition = data_filter_module(),
          refTable = data_reactive_all()$refTable_filtered,
          filters = NULL,
          selectors = "year",
          divider = "fleet",
          yvar = "catch"
        )
      )
    })
    # output$plot <- renderPlotly({
    #   req(plot_name())
    #   df <- data_filter_module()
    #   req(df)

    #   if (plot_name() == "plot1") {
    #     req(input$scenario, input$stock)
    #   } else if (plot_name() == "plot2") {
    #     req(input$year, input$fleet)
    #   }

    #   print("Rendering plot")
    #   switch(plot_name(),
    #     "plot1" = plot_catchScenStk_plotly(
    #       data = df,
    #       adv = data_reactive_all()$catchRange_filtered,
    #       refTable = data_reactive_all()$refTable_filtered
    #     ),
    #     "plot2" = plot_catchComp_plotly(
    #       dataComposition = df,
    #       refTable = dataComp()$refTable,
    #       filters = NULL,
    #       selectors = "year",
    #       divider = "fleet",
    #       yvar = "catch"
    #     )
    #   )
    # })

  })
}
