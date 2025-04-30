# mod_mixfish_plot_display_ui <- function(id) {
#   ns <- NS(id)
# #   plotlyOutput(ns("plot"))

#   card(
#             height = "80vh", full_screen = T, fill = F,
#             card_header("Filter by scenario and stock"),
#             card_body(
#               fillable = T, fill = T, class = "p-1",
#               select_group_ui(
#                 label = NULL,
#                 id = ns("my-filters"),
#                 params = list(
#                   scenario = list(inputId = "scenario", label = "Management Scenario:", placeholder = "Select scenario"),
#                   stock = list(inputId = "stock", label = "Fish Stock", placeholder = "Select stock")
#                 )
#               ),
#               withSpinner(
#                 plotlyOutput(ns("plot"), height = "65vh"),
#                 caption = "Getting mix-fish results..."
#               )
#             )
#           )
# }
mod_mixfish_plot_display_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "80vh", full_screen = TRUE, fill = FALSE,
    card_header("Filter by scenario and stock"),
    card_body(
      fillable = TRUE, fill = TRUE, class = "p-1",
      uiOutput(ns("filter_ui")),  # <-- Dynamic UI output
      withSpinner(
        plotlyOutput(ns("plot"), height = "65vh"),
        caption = "Getting mix-fish results..."
      )
    )
  )
}

# mod_mixfish_plot_display_server <- function(id, plot_name, selected_ecoregion, selected_subRegion) {
#   moduleServer(id, function(input, output, session) {

#     # output$ecoregion_label <- renderText({
#     #   req(selected_ecoregion())
#     #   paste("Ecoregion:", selected_ecoregion())
#     # })

#     # output$mf_projections_text <- renderUI({
#     #   HTML(select_text(texts,"mixfish","projection_sidebar"))
#     # })  
    
#     data_reactive_all <- reactive({
#         # req(selected_ecoregion())
#     #   req(c(selected_subRegion(),selected_ecoregion()))
#       sReg <- selected_subRegion()
#       ecoR <- selected_ecoregion()
#       eco_acronym <- get_ecoregion_acronym(sReg)
      
#       list(
#         catchScenarioStk_filtered = catchScenarioStk %>% filter(ecoregion == eco_acronym),
#         catchRange_filtered = catchRange %>% filter(ecoregion == eco_acronym),
#         refTable_filtered = refTable %>% filter(ecoregion == eco_acronym)
#       )
#     })
    
#     data_filter_module <- select_group_server(
#       id = "my-filters",
#       data_r = reactive(data_reactive_all()$catchScenarioStk_filtered),
#       vars_r = reactive(c("scenario", "stock"))
#     )

#     dataComp <- reactive({
#       library(mixfishtools)
#       data(refTable)
#       data(stfMtStkSum)
#       list(refTable = refTable, stfMtStkSum = stfMtStkSum)
#     #   data_comp <- subset(stfMtStkSum, scenario == "min")
#     })
    

    
      
#       # Plot catch composition for each fleet over time
#       selectors <- c("year")
#       divider <- c("fleet")

#     output$plot <- renderPlotly({
#       req(plot_name())
#       switch(plot_name(),
#              "plot1" = plot_catchScenStk_plotly(data = data_filter_module(),
#                                                 adv = data_reactive_all()$catchRange_filtered,
#                                                 refTable = data_reactive_all()$refTable_filtered
#                                             ),
#              "plot2" = plot_catchComp(dataComp()$stfMtStkSum, dataComp()$refTable, filters = NULL, selectors, divider, yvar = "catch")
#             #  "plot3" = ggplot(data=ple4, aes(year, data)) + geom_line(aes(group=age, colour=factor(age))) + 
#             #             facet_wrap(~slot, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "none"),
#             #   "plot4" = plot(nsher)
#     )
# })
# })
# }
mod_mixfish_plot_display_server <- function(id, plot_name, selected_ecoregion, selected_subRegion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_reactive_all <- reactive({
        req(selected_subRegion())
    #   req(selected_ecoregion())
      sReg <- selected_subRegion()
    #   ecoR <- selected_ecoregion()
      eco_acronym <- get_ecoregion_acronym(sReg)

      list(
        catchScenarioStk_filtered = catchScenarioStk %>% filter(ecoregion == eco_acronym),
        catchRange_filtered = catchRange %>% filter(ecoregion == eco_acronym),
        refTable_filtered = refTable %>% filter(ecoregion == eco_acronym)
      )
    })

    dataComp <- reactive({
      library(mixfishtools)
      data(refTable)
      data(stfMtStkSum)
      list(refTable = refTable, stfMtStkSum = stfMtStkSum)
    })

    # Dynamically render the correct filter UI
    output$filter_ui <- renderUI({
        req(plot_name())
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
            req(plot_name())
            if (plot_name() == "plot1") {
            data_reactive_all()$catchScenarioStk_filtered
            } else if (plot_name() == "plot2") {
            dataComp()$stfMtStkSum
            }
        }),
        vars_r = reactive({
            req(plot_name())
            if (plot_name() == "plot1") {
            c("scenario", "stock")
            } else if (plot_name() == "plot2") {
            c("year", "fleet")
            }
        })
    )

    output$plot <- renderPlotly({
      req(plot_name())
      switch(plot_name(),
             "plot1" = plot_catchScenStk_plotly(
               data = data_filter_module(),
               adv = data_reactive_all()$catchRange_filtered,
               refTable = data_reactive_all()$refTable_filtered
             ),
             "plot2" = plot_catchComp_plotly(
               dataComposition = data_filter_module(),
               refTable = dataComp()$refTable,
               filters = NULL,
               selectors = "year",
               divider = "fleet",
               yvar = "catch"
             )
      )
    })
  })
}