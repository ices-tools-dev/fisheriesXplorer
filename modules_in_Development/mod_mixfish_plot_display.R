mod_mixfish_plot_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    card(
      height = "80vh", full_screen = TRUE, fill = FALSE,
      card_header("Filter by scenario and stock"),
      card_body(
        fillable = TRUE, fill = TRUE, class = "p-1",
        uiOutput(ns("filter_ui")),
        withSpinner(
          plotlyOutput(ns("plot"), height = "73vh"),
          caption = "Getting mix-fish results..."
        )
      )      
    )
  )
}




mod_mixfish_plot_display_server <- function(id, plot_name, selected_ecoregion, selected_subRegion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      "Last update: December 05, 2024" # e.g., "May 26, 2025"
    })



    ui_rendered <- reactiveVal(FALSE)         # Track if filter UI has rendered
    data_filter_module <- reactiveVal(NULL)   # Store the server module output

    # Reactive: Filter full data based on selected ecoregion/subregion
    data_reactive_all <- reactive({
      req(selected_ecoregion())
      eco_acronym <- get_active_region_acronym(selected_subRegion(), selected_ecoregion())

      validate(
        need(eco_acronym %in% catchScenarioStk$ecoregion, "Invalid ecoregion filter.")
      )
      
      print(paste("Filtering using acronym:", eco_acronym))
      list(
        catchScenarioStk_filtered = catchScenarioStk %>% filter(ecoregion == eco_acronym),
        catchRange_filtered       = catchRange %>% filter(ecoregion == eco_acronym),
        EffortByFleetStock_filtered = EffortByFleetStock %>% filter(ecoregion == eco_acronym),
        MetierStockLandings_filtered = MetierStockLandings %>% filter(ecoregion == eco_acronym),
        StockLandings_filtered = StockLandings %>% filter(ecoregion == eco_acronym),
        refTable_filtered         = refTable %>% filter(ecoregion == eco_acronym)
      )
    })

    # Reactive: Additional data for composition plots
    dataComp <- reactive({
      req(plot_name())
      library(mixfishtools)
      data(stfMtStkSum)
      print("Running dataComp")
      list(stfMtStkSum = stfMtStkSum)
    })

    # When plot_name changes, reset filter UI state
    observeEvent(plot_name(), {
      ui_rendered(FALSE)
      data_filter_module(NULL)  # Clear previous server module
    })

    # Render filter UI depending on the plot type
    output$filter_ui <- renderUI({
      req(plot_name())
      print("Rendering filter_ui")

      ui <- if (plot_name() == "plot1") {
        select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            scenario = list(inputId = "scenario", label = "Management Scenario:", placeholder = "Select scenario"),
            stock = list(inputId = "stock", label = "Fish Stock", placeholder = "Select stock")
          )
        )
      } else if (plot_name() == "plot2") {
        # For plot2, use a different set of filters
        select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            # stock = list(inputId = "stock", label = "Fish Stock", placeholder = "Select stock"),
            fleet = list(inputId = "fleet", label = "Fleet", placeholder = "Select fleet")
          )
        )
      } else if (plot_name() == "plot3") {
        # For plot3, use a different set of filters
        select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            stock = list(inputId = "stock", label = "Fish Stock", placeholder = "Select stock"),
            fleet = list(inputId = "metier", label = "Metier", placeholder = "Select metier")
          )
        )
      } else if (plot_name() == "plot5") {
        # For plot5, use a different set of filters
        select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            year = list(inputId = "year", label = "Year", placeholder = "Select year"),
            fleet = list(inputId = "fleet", label = "Fleet", placeholder = "Select fleet")
          )
        )
      }

      shinyjs::delay(100, ui_rendered(TRUE))  # Signal that UI is ready
      ui
    })

    # When UI is rendered, initialize the corresponding server module
    observeEvent(ui_rendered(), {
      req(ui_rendered())

      print("Registering filter server")
      data_filter_module(select_group_server(
        id = "my-filters-mixfish",
        data_r = reactive({
          req(plot_name())
          print("Providing filtered data")
          if (plot_name() == "plot1") {
            data_reactive_all()$catchScenarioStk_filtered
          } else if (plot_name() == "plot2") {
            data_reactive_all()$EffortByFleetStock_filtered
          } else if (plot_name() == "plot3") {
            data_reactive_all()$MetierStockLandings_filtered
          } else if (plot_name() == "plot5") {
            dataComp()$stfMtStkSum
          }
        }),
        vars_r = reactive({
          req(plot_name())
          print("Providing filter vars")
          if (plot_name() == "plot1") {
            c("scenario", "stock")
          } else if (plot_name() == "plot2") {
            c("stock","fleet")
          } else if (plot_name() == "plot3") {
            c("stock", "metier")
          } else if (plot_name() == "plot5") {
            c("year", "fleet")
          }
        })
      ))
    })

    # Render the appropriate plot
    output$plot <- renderPlotly({
      req(plot_name(), data_filter_module())
      print("Rendering plot")

      output$ecoregion_label <- renderText({
      req(selected_ecoregion())
      paste("Ecoregion:", selected_ecoregion())
    })

    output$current_date <- renderText({
      "Last update: December 05, 2024" # e.g., "May 26, 2025"
    })

      switch(plot_name(),
        "plot1" = plot_catchScenStk_plotly(
          data = data_filter_module()(),
          adv = data_reactive_all()$catchRange_filtered,
          refTable = data_reactive_all()$refTable_filtered
        ),
        "plot2" = plot_effortFltStk_plotly(
          data = data_filter_module()(),
          refTable = data_reactive_all()$refTable_filtered
          # ncol = 2,          # how many facet columns
          # rowHeight = 200
        ),
        "plot3" = plot_landByMetStock_plotly(
          data = data_filter_module()(),
          refTable = data_reactive_all()$refTable_filtered
        ),
        "plot4" = plot_landByStock_plotly(
          data = data_reactive_all()$StockLandings_filtered,
          refTable = data_reactive_all()$refTable_filtered
        ),
        "plot5" = plot_catchComp_plotly(
          dataComposition = data_filter_module()(),
          refTable = data_reactive_all()$refTable_filtered,
          filters = NULL,
          selectors = "year",
          divider = "fleet",
          yvar = "catch"
        )
      )
    })
  })
}
