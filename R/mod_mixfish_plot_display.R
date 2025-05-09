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



# mod_mixfish_plot_display_server <- function(id, plot_name, selected_ecoregion, selected_subRegion) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

    
#     data_reactive_all <- reactive({
#       req(selected_ecoregion())
#       eco_acronym <- get_active_region_acronym(selected_subRegion(), selected_ecoregion())

#       validate(
#         need(eco_acronym %in% catchScenarioStk$ecoregion, "Invalid ecoregion filter.")
#       )

#       print(paste("Filtering using acronym:", eco_acronym))
#       list(
#         catchScenarioStk_filtered = catchScenarioStk %>% filter(ecoregion == eco_acronym),
#         catchRange_filtered       = catchRange %>% filter(ecoregion == eco_acronym),
#         refTable_filtered         = refTable %>% filter(ecoregion == eco_acronym)
#       )
#     })

#     dataComp <- reactive({
#       req(plot_name())
#       library(mixfishtools)
#       data(stfMtStkSum)
#       print("Running dataComp")
#       list(stfMtStkSum = stfMtStkSum)
#     })

#     # Dynamically render the correct filter UI
#     output$filter_ui <- renderUI({
#       req(plot_name())
#       print("Rendering filter_ui")
#       if (plot_name() == "plot1") {
#         select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             scenario = list(inputId = "scenario", label = "Management Scenario:", placeholder = "Select scenario"),
#             stock = list(inputId = "stock", label = "Fish Stock", placeholder = "Select stock")
#           )
#         )
#       } else if (plot_name() == "plot2") {
#         select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             year = list(inputId = "year", label = "Year", placeholder = "Select year"),
#             fleet = list(inputId = "fleet", label = "Fleet", placeholder = "Select fleet")
#           )
#         )
#       }
#     })

#     data_filter_module <- select_group_server(
#       id = "my-filters-mixfish",
#       data_r = reactive({
#         print("Running data_filter_module")
#         req(plot_name())
#         if (plot_name() == "plot1") {
#           data_reactive_all()$catchScenarioStk_filtered
#         } else if (plot_name() == "plot2") {
#           dataComp()$stfMtStkSum
#         }
#       }),
#       vars_r = reactive({
#         req(plot_name())
#         print("Running vars_r")
#         if (plot_name() == "plot1") {
#           c("scenario", "stock")
#         } else if (plot_name() == "plot2") {
#           c("year", "fleet")
#         }
#       })
#     )



#     output$plot <- renderPlotly({
#       req(plot_name())      
#       print("Rendering plot")
      
#       switch(plot_name(),
#         "plot1" = plot_catchScenStk_plotly(
#           data = data_filter_module(),
#           adv = data_reactive_all()$catchRange_filtered,
#           refTable = data_reactive_all()$refTable_filtered
#         ),
#         "plot2" = plot_catchComp_plotly(
#           dataComposition = data_filter_module(),
#           refTable = data_reactive_all()$refTable_filtered,
#           filters = NULL,
#           selectors = "year",
#           divider = "fleet",
#           yvar = "catch"
#         )
#       )
#     })
    

#   })
# }
mod_mixfish_plot_display_server <- function(id, plot_name, selected_ecoregion, selected_subRegion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
      } else {
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
          } else {
            dataComp()$stfMtStkSum
          }
        }),
        vars_r = reactive({
          req(plot_name())
          print("Providing filter vars")
          if (plot_name() == "plot1") {
            c("scenario", "stock")
          } else {
            c("year", "fleet")
          }
        })
      ))
    })

    # Render the appropriate plot
    output$plot <- renderPlotly({
      req(plot_name(), data_filter_module())
      print("Rendering plot")

      switch(plot_name(),
        "plot1" = plot_catchScenStk_plotly(
          data = data_filter_module()(),
          adv = data_reactive_all()$catchRange_filtered,
          refTable = data_reactive_all()$refTable_filtered
        ),
        "plot2" = plot_catchComp_plotly(
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
