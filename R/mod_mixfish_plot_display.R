mod_mixfish_plot_display_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot"))
}


mod_mixfish_plot_display_server <- function(id, plot_name, selected_ecoregion, selected_subRegion) {
  moduleServer(id, function(input, output, session) {

    # output$ecoregion_label <- renderText({
    #   req(selected_ecoregion())
    #   paste("Ecoregion:", selected_ecoregion())
    # })

    # output$mf_projections_text <- renderUI({
    #   HTML(select_text(texts,"mixfish","projection_sidebar"))
    # })  
    
    data_reactive_all <- reactive({
      req(selected_ecoregion())
      sReg <- selected_subRegion()
      ecoR <- selected_ecoregion()
      eco_acronym <- get_ecoregion_acronym(sReg)
      
      list(
        catchScenarioStk_filtered = catchScenarioStk %>% filter(ecoregion == eco_acronym),
        catchRange_filtered = catchRange %>% filter(ecoregion == eco_acronym),
        refTable_filtered = refTable %>% filter(ecoregion == eco_acronym)
      )
    })
    # data_reactive_all <- reactive({
    #   # instead of req(selected_ecoregion())
    #   req(selected_subRegion())
    #   subR <- selected_subRegion()
    #     browser()
    #   list(
    #     catchScenarioStk_filtered = catchScenarioStk   %>% 
    #                                 dplyr::filter(subregion_col == subR),
    #     catchRange_filtered       = catchRange         %>% 
    #                                 dplyr::filter(subregion_col == subR),
    #     refTable_filtered         = refTable           %>% 
    #                                 dplyr::filter(subregion_col == subR)
    #   )
    # })

    data_filter_module <- select_group_server(
      id = "my-filters",
      data_r = reactive(data_reactive_all()$catchScenarioStk_filtered),
      vars_r = reactive(c("scenario", "stock"))
    )


    library(mixfishtools)
      data(refTable)
      data(stfMtStkSum)
      data <- subset(stfMtStkSum, scenario == "min")

      # add country and area identifiers (if desired)
      tmp <- strsplit(data$metier, ".", fixed = TRUE)
      data$area <- unlist(lapply(tmp, FUN = function(x) {
        ifelse(length(x) == 2, x[2], NA)
      }))
      tmp <- strsplit(data$fleet, "_", fixed = TRUE)
      data$country <- unlist(lapply(tmp, FUN = function(x) {
        ifelse(length(x) == 2, x[1], NA)
      }))
      # replace stock with ICES stock code
      data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]
      # Plot catch composition for each fleet over time
      selectors <- c("year")
      divider <- c("fleet")

    
    output$plot <- renderPlotly({
      req(plot_name())
      switch(plot_name(),
             "plot1" = plot_catchScenStk_plotly(data = data_filter_module(),
                                                adv = data_reactive_all()$catchRange_filtered,
                                                refTable = data_reactive_all()$refTable_filtered
                                            ),
             "plot2" = plot_catchComp(data, refTable, filters = NULL, selectors, divider, yvar = "catch")
            #  "plot3" = ggplot(data=ple4, aes(year, data)) + geom_line(aes(group=age, colour=factor(age))) + 
            #             facet_wrap(~slot, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "none"),
            #   "plot4" = plot(nsher)
    )
})
})
}