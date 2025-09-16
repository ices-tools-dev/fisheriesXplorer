


# plot_catchScenStk_interactive <- function(data, adv, ofwhich = FALSE,
#                                           xlab = "Scenario", ylab = "Catch [t]") {
#   # Load required libraries
#   library(ggplot2)
#   library(plotly)
#   library(dplyr)

#   # Ensure 'scenario' exists
#   if (!"scenario" %in% names(data)) {
#     stop("Error: 'scenario' column not found in data.")
#   }
#   data <- mutate(data, scenario = as.factor(scenario))

#   # Ensure adv has correct columns
#   adv <- adv %>%
#     mutate(upper = ifelse("upper" %in% names(.), upper, advice),
#            lower = ifelse("lower" %in% names(.), lower, advice))

#   # Base ggplot
#   p <- ggplot(data) +
#     aes(x = scenario, y = catch, text = paste("Stock:", stock, "<br>Catch:", catch)) +
#     facet_wrap(~ stock, scales = 'free_y') +

#     # Background color (Advice Range)
#     geom_rect(data = adv, aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = adv$advice), 
#               fill = 'green', alpha = 0.25, inherit.aes = FALSE) +
#     geom_rect(data = adv, aes(xmin = -Inf, xmax = Inf, ymin = advice, ymax = upper), 
#               fill = 'yellow', alpha = 0.25, inherit.aes = FALSE) +
#     geom_rect(data = adv, aes(xmin = -Inf, xmax = Inf, ymin = upper, ymax = Inf), 
#               fill = 'red', alpha = 0.25, inherit.aes = FALSE) +

#     # Catch bars
#     geom_col(aes(fill = stock), width = 0.5, color = "grey35") +

#     # Advice lines
#     geom_hline(data = adv, aes(yintercept = advice, group = stock), linetype = "solid", color = "black") +
#     geom_hline(data = adv, aes(yintercept = upper, group = stock), linetype = "dashed") +
#     geom_hline(data = adv, aes(yintercept = lower, group = stock), linetype = "dashed") +

#     # Labels & Theme
#     xlab(xlab) + ylab(ylab) +
#     theme_minimal() +
#     theme(text = element_text(size = 10),
#           legend.position = "none",
#           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#   # Convert to plotly and fix missing background
#   plot <- ggplotly(p, tooltip = "text") %>%
#     layout(shapes = list(
#       list(type = "rect", x0 = -Inf, x1 = Inf, y0 = 0, y1 = min(adv$advice, na.rm = TRUE), fillcolor = "green", opacity = 0.25, layer = "below"),
#       list(type = "rect", x0 = -Inf, x1 = Inf, y0 = min(adv$advice, na.rm = TRUE), y1 = max(adv$upper, na.rm = TRUE), fillcolor = "yellow", opacity = 0.25, layer = "below"),
#       list(type = "rect", x0 = -Inf, x1 = Inf, y0 = max(adv$upper, na.rm = TRUE), y1 = Inf, fillcolor = "red", opacity = 0.25, layer = "below")
#     ))
  
#   return(plot)
# }

# plot_catchScenStk_interactive <- function(data, adv, ofwhich = FALSE,
#                                           xlab = "Scenario", ylab = "Catch [t]") {
#   # Load required libraries
#   library(plotly)
#   library(dplyr)

#   # Ensure 'scenario' exists
#   if (!"scenario" %in% names(data)) {
#     stop("Error: 'scenario' column not found in data.")
#   }
#   data <- mutate(data, scenario = as.factor(scenario))

#   # Ensure adv has correct columns
#   adv <- adv %>% 
#     mutate(upper = ifelse("upper" %in% names(.), upper, advice),
#            lower = ifelse("lower" %in% names(.), lower, advice))

#   # Create a list of subplot traces
#   subplot_plots <- list()

#   # Loop over unique stocks to create individual plots for each stock
#   for (stk in unique(data$stock)) {
#     stock_data <- data %>% filter(stock == stk)
#     stock_adv <- adv %>% filter(stock == stk)
    
#     # Create the background rectangles based on advice levels for the current stock
#     background <- list(
#       list(type = "rect", x0 = -Inf, x1 = Inf, y0 = 0, y1 = min(stock_adv$advice, na.rm = TRUE), fillcolor = "green", opacity = 0.25, layer = "below"),
#       list(type = "rect", x0 = -Inf, x1 = Inf, y0 = min(stock_adv$advice, na.rm = TRUE), y1 = max(stock_adv$upper, na.rm = TRUE), fillcolor = "yellow", opacity = 0.25, layer = "below"),
#       list(type = "rect", x0 = -Inf, x1 = Inf, y0 = max(stock_adv$upper, na.rm = TRUE), y1 = Inf, fillcolor = "red", opacity = 0.25, layer = "below")
#     )
    
#     # Create the individual plot for the current stock
#     p <- plot_ly(x = stock_data$scenario, 
#                  y = stock_data$catch, 
#                  color = stock_data$stock, 
#                  type = 'bar', 
#                  text = paste("Stock:", stock_data$stock, "<br>Catch:", stock_data$catch)) %>%
#       layout(
#         title = list(text = paste(ylab, "-", stk), x = 0.5),
#         xaxis = list(title = xlab, tickangle = 90),
#         yaxis = list(title = ylab),
#         barmode = 'stack',
#         shapes = background,
#         showlegend = FALSE
#       ) %>%
#       # Add advice lines
#       add_trace(x = stock_adv$scenario, y = stock_adv$advice, type = "scatter", mode = "lines", line = list(color = "black", width = 2), name = "Advice") %>%
#       add_trace(x = stock_adv$scenario, y = stock_adv$upper, type = "scatter", mode = "lines", line = list(color = "black", dash = "dash"), name = "Upper Advice") %>%
#       add_trace(x = stock_adv$scenario, y = stock_adv$lower, type = "scatter", mode = "lines", line = list(color = "black", dash = "dash"), name = "Lower Advice")
    
#     # Append the plot for the current stock to the list
#     subplot_plots[[stk]] <- p
#   }

#   # Combine all subplots into one figure
#   combined_plot <- subplot(subplot_plots, nrows = length(subplot_plots), shareX = TRUE, shareY = TRUE)
  
#   return(combined_plot)
# }


# plot_catchScenStk_int <- function(data, adv, ofwhich = FALSE,
#                               xlab = "Scenario", ylab = "Catch [t]"){
  
#   # add dummy advice range values if missing
#   if(!"upper" %in% names(adv)){
#     adv$upper <- adv$advice
#   }
#   if(!"lower" %in% names(adv)){
#     adv$lower <- adv$advice
#   }
  
#   p <- ggplot2::ggplot(data = data) + ggplot2::aes(x = factor(scenario), y = catch, text = paste("Stock:", stock, "<br>Catch:", round(catch), "t")) +
#     ggplot2::facet_wrap(~ stock, scales = 'free_y') +
#     ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
#               mapping = ggplot2::aes(xmin = -1000, xmax = 1000, ymin = 0, ymax = advice),
#               fill = 'green', alpha = 0.25) +
#     ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
#               mapping = ggplot2::aes(xmin = -1000, xmax = 1000, ymin = advice, ymax = upper),
#               fill = 'yellow', alpha = 0.25) +
#     ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
#               mapping = ggplot2::aes(xmin = -1000, xmax = 1000, ymin = upper, ymax = Inf),
#               fill = 'red', alpha = 0.25) +
#     ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = advice), lty = 1, col = "black") +
#     ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = upper), lty = 3) +
#     ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = lower), lty = 3) +
#     ggplot2::geom_col(width = 0.5, fill = "grey35", color = "grey35") +
#     ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
#     ggplot2::xlab(label = xlab) +
#     ggplot2::ylab(label = ylab) +
#     ggplot2::theme(
#       text = ggplot2::element_text(size = 10), legend.position="none",
#       axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  
#   plotly::ggplotly(p, tooltip = "text")
# }


# plot_catchScenStk_int <- function(data, adv, ofwhich = FALSE,
#                                   xlab = "Scenario", ylab = "Catch [t]") {
  
#   # Add dummy advice range values if missing
#   if(!"upper" %in% names(adv)){
#     adv$upper <- adv$advice
#   }
#   if(!"lower" %in% names(adv)){
#     adv$lower <- adv$advice
#   }
  
#   # Add tooltip column to dataset
#   data$tooltip_text <- paste("Stock:", data$stock, "<br>Catch:", round(data$catch), "t")
  
#   # Create a new column in adv for the factor level positions
#   adv$scenario_pos <- as.numeric(factor(adv$stock))  # Assuming 'stock' correlates to factor levels
  
#   # Plot
#   p <- ggplot2::ggplot(data = data) + 
#     ggplot2::aes(x = factor(scenario), y = catch) +
#     ggplot2::facet_wrap(~ stock, scales = 'free_y') +
    
#     # Add the rects to highlight the advice range
#     ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
#                        mapping = ggplot2::aes(xmin = scenario_pos - 0.25, xmax = scenario_pos + 0.25, 
#                                               ymin = 0, ymax = advice),
#                        fill = 'green', alpha = 0.25) +
#     ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
#                        mapping = ggplot2::aes(xmin = scenario_pos - 0.25, xmax = scenario_pos + 0.25, 
#                                               ymin = advice, ymax = upper),
#                        fill = 'yellow', alpha = 0.25) +
#     ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
#                        mapping = ggplot2::aes(xmin = scenario_pos - 0.25, xmax = scenario_pos + 0.25, 
#                                               ymin = upper, ymax = Inf),
#                        fill = 'red', alpha = 0.25) +
    
#     # Add horizontal lines at advice, upper, and lower thresholds
#     ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = advice), lty = 1, col = "black") +
#     ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = upper), lty = 3) +
#     ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = lower), lty = 3) +
    
#     # Add the bar plot for actual catch values
#     ggplot2::geom_col(width = 0.5, fill = "grey35", color = "grey35") +
    
#     # Scale y axis
#     ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    
#     # Labels
#     ggplot2::xlab(label = xlab) +
#     ggplot2::ylab(label = ylab) +
    
#     # Theme adjustments
#     ggplot2::theme(
#       text = ggplot2::element_text(size = 10), 
#       legend.position = "none",
#       axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
#     )

#   # Convert to interactive plot
#   plotly::ggplotly(p, tooltip = c("tooltip_text"))
# }


plot_catchScenStk_int <- function(data, adv, #ofwhich = FALSE,
                                  xlab = "Scenario", ylab = "Catch [t]") {
  
  # Add dummy advice range values if missing
  if(!"upper" %in% names(adv)){
    adv$upper <- adv$advice
  }
  if(!"lower" %in% names(adv)){
    adv$lower <- adv$advice
  }
  
  # Add tooltip column to dataset
  data$tooltip_text <- paste("Stock:", data$stock, "<br>Catch:", round(data$catch), "t")
  
  # Create a new column in adv for the factor level positions
  adv$scenario_pos <- as.numeric(factor(adv$scenario))  # Assuming 'scenario' correlates to factor levels
  
  # Define a large number to replace Inf
  large_number <- max(data$catch, na.rm = TRUE) * 1.5
  
  # Plot
  p <- ggplot2::ggplot(data = data) + 
    ggplot2::aes(x = factor(scenario), y = catch) +
    ggplot2::facet_wrap(~ stock, scales = 'free_y') +
    
    # Add the rects to highlight the advice range
    ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
                       mapping = ggplot2::aes(xmin = scenario_pos - 0.25, xmax = scenario_pos + 0.25, 
                                              ymin = 0, ymax = advice),
                       fill = 'green', alpha = 0.25) +
    ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
                       mapping = ggplot2::aes(xmin = scenario_pos - 0.25, xmax = scenario_pos + 0.25, 
                                              ymin = advice, ymax = upper),
                       fill = 'yellow', alpha = 0.25) +
    ggplot2::geom_rect(stat = "identity", data = adv, inherit.aes = FALSE,
                       mapping = ggplot2::aes(xmin = scenario_pos - 0.25, xmax = scenario_pos + 0.25, 
                                              ymin = upper, ymax = large_number),
                       fill = 'red', alpha = 0.25) +
    
    # Add horizontal lines at advice, upper, and lower thresholds
    ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = advice), lty = 1, col = "black") +
    ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = upper), lty = 3) +
    ggplot2::geom_hline(data = adv, mapping = ggplot2::aes(yintercept = lower), lty = 3) +
    
    # Add the bar plot for actual catch values
    ggplot2::geom_col(width = 0.5, fill = "grey35", color = "grey35") +
    
    # Scale y axis
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    
    # Labels
    ggplot2::xlab(label = xlab) +
    ggplot2::ylab(label = ylab) +
    
    # Theme adjustments
    ggplot2::theme(
      text = ggplot2::element_text(size = 10), 
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

  # Convert to interactive plot
  plotly::ggplotly(p, tooltip = c("tooltip_text"))
}


# plot_catchScenStk_plotly <- function(data, adv, ofwhich = FALSE,
#                                      xlab = "Scenario", ylab = "Catch [t]") {
#   data <- data %>% filter(stock %in% adv$stock)
#   # Add dummy advice range values if missing
#   if(!"upper" %in% names(adv)){
#     adv$upper <- adv$advice
#   }
#   if(!"lower" %in% names(adv)){
#     adv$lower <- adv$advice
#   }
  
  
#   # Create a list to store the subplots
#   subplots <- list()
  
#   # Get unique stocks
#   unique_stocks <- unique(data$stock)
#   n_stocks <- length(unique_stocks)
#   n_cols <- min(4, n_stocks)  # Max 4 columns
#   n_rows <- ceiling(n_stocks / n_cols)  # Adjust row count based on column count
  
#   for (fishstock in unique_stocks) {
#     stock_data <- dplyr::filter(data, stock == fishstock)
#     stock_adv <- dplyr::filter(adv, stock == fishstock)
#     width_plot <- length(unique(stock_data$scenario))
#     p <- plotly::plot_ly(stock_data, 
#                         x = ~scenario, 
#                         y = ~catch, 
#                         type = 'bar', 
#                         name = fishstock, 
#                         marker = list(color = I('grey35')),
#                         hovertemplate = paste0(
#                           "Stock: ", fishstock, "<br>",
#                           "Scenario: %{x}<br>",
#                           "Catches (tonnes): %{y:.0f}<extra></extra>"                          
#                         )) %>% 
#       plotly::layout(
#         xaxis = list(title = xlab, tickangle = 45),
#         yaxis = list(title = ylab, range = c(0, max(stock_data$catch) + 0.1*max(stock_data$catch))),
#         # margin = list(l = 50, r = 50, t = 50, b = 50),
        
#         shapes = list(
#           list(type = 'rect', x0 = -0.5, x1 = width_plot-.4, y0 = 0, y1 = stock_adv$advice[1], 
#                fillcolor = 'green', opacity = 0.17, line = list(width = 0)),
#           list(type = 'rect', x0 = -0.5, x1 = width_plot-.4, y0 = stock_adv$advice[1], y1 = stock_adv$upper[1], 
#                fillcolor = 'yellow', opacity = 0.17, line = list(width = 0)),
#           list(type = 'rect', x0 = -0.5, x1 = width_plot-.4, y0 = stock_adv$upper[1], y1 = max(stock_data$catch)+0.1*max(stock_data$catch), 
#                fillcolor = 'red', opacity = 0.17, line = list(width = 0)),
#           list(type = "line", x0 = -0.5, x1 = width_plot-.4, y0 = stock_adv$advice[1], y1 = stock_adv$advice[1], 
#                line = list(color = "black", width = 2, dash = "solid")),
#           list(type = "line", x0 = -0.5, x1 = width_plot-.4, y0 = stock_adv$upper[1], y1 = stock_adv$upper[1], 
#                line = list(color = "black", width = 2, dash = "dash")),
#           list(type = "line", x0 = -0.5, x1 = width_plot-.4, y0 = stock_adv$lower[1], y1 = stock_adv$lower[1], 
#                line = list(color = "black", width = 2, dash = "dash"))
#         ),
#         annotations = list(
#              x = 0.5,
#              y = 1.05,
#              text = paste0(fishstock),
#              xref = "paper",
#              yref = "paper",
#              xanchor = "center",  # center of text
#              yanchor = "bottom",  # bottom of text
#              showarrow = FALSE
#            )
#       )
    
#     # if (ofwhich) {
#     #   p <- p %>%
#     #     plotly::add_trace(data = stock_adv, x = ~scenario, y = ~advice_ofwhich, type = 'scatter', mode = 'lines', 
#     #                       line = list(color = '#85AD00', dash = 'dash'), name = 'Advice of which') %>%
#     #     plotly::add_trace(data = stock_data, x = ~scenario, y = ~catch_ofwhich, type = 'bar', name = 'Catch of which', 
#     #                       marker = list(color = '#85AD00', pattern = list(shape = 'crosshatch')))
#     # }
    
#     subplots[[fishstock]] <- p
#   }
  
#   # Combine subplots into one figure
#   fig <- plotly::subplot(subplots, 
#                         nrows = ceiling(length(unique_stocks) / 4),
#                         shareX = TRUE,  
#                         shareY = FALSE, 
#                         titleY = FALSE, 
#                         titleX = TRUE,
#                         margin = 0.05) %>% #
#                         plotly::layout(
#                                         # height = 200 * n_rows,
#                                         grid = list(rows = n_rows, 
#                                                     columns = n_cols, 
#                                                     pattern = "independent"),
#                                                     showlegend = FALSE
                                                    
                                                    
                                                    
#                                                     # margin = list(l = 50, r = 50, t = 50, b = 50) 
#   )
# #   fig <- fig  %>% plotly::layout(height = 200 * ceiling(length(unique_stocks) / 4))
#   fig
# }


# plot_catchScenStk_plotly <- function(data, adv, refTable, ofwhich = FALSE,
#                                      xlab = "Scenarios", ylab = "Catch (tonnes)") {
#   data <- dplyr::filter(data, stock %in% adv$stock)

#   # Add dummy advice range values if missing
#   if (!"upper" %in% names(adv)) adv$upper <- adv$advice
#   if (!"lower" %in% names(adv)) adv$lower <- adv$advice

#   subplots <- list()
#   unique_stocks <- unique(data$stock)
#   n_stocks <- length(unique_stocks)
#   n_cols <- min(4, n_stocks)
#   n_rows <- ceiling(n_stocks / n_cols)

#   for (fishstock in unique_stocks) {
#     stock_data <- dplyr::filter(data, stock == fishstock)
#     stock_adv <- dplyr::filter(adv, stock == fishstock)
#     width_plot <- length(unique(stock_data$scenario))
#     max_y <- max(stock_data$catch) * 1.1
#     bar_color <- dplyr::filter(refTable, stock == fishstock)$col
#     if (length(bar_color) == 0) bar_color <- "grey" # fallback color

#     # Define background shape zones
#     shape_list <- list(
#       list(
#         type = "rect", x0 = -0.5, x1 = width_plot - 0.4, y0 = 0, y1 = stock_adv$advice[1],
#         fillcolor = "green", opacity = 0.15, line = list(width = 0), layer = "below"
#       ),
#       list(
#         type = "rect", x0 = -0.5, x1 = width_plot - 0.4, y0 = stock_adv$advice[1], y1 = stock_adv$upper[1],
#         fillcolor = "yellow", opacity = 0.15, line = list(width = 0), layer = "below"
#       ),
#       list(
#         type = "rect", x0 = -0.5, x1 = width_plot - 0.4, y0 = stock_adv$upper[1], y1 = max_y,
#         fillcolor = "red", opacity = 0.15, line = list(width = 0), layer = "below"
#       ),
#       list(
#         type = "line", x0 = -0.5, x1 = width_plot - 0.4, y0 = stock_adv$advice[1], y1 = stock_adv$advice[1],
#         line = list(color = "#6e6e6e", width = 1.5, dash = "solid")
#       ),
#       list(
#         type = "line", x0 = -0.5, x1 = width_plot - 0.4, y0 = stock_adv$upper[1], y1 = stock_adv$upper[1],
#         line = list(color = "#6e6e6e", width = 1.5, dash = "dash")
#       ),
#       list(
#         type = "line", x0 = -0.5, x1 = width_plot - 0.4, y0 = stock_adv$lower[1], y1 = stock_adv$lower[1],
#         line = list(color = "#6e6e6e", width = 1.5, dash = "dot")
#       )
#     )

#     # Base bar chart
#     p <- plotly::plot_ly(
#       data = stock_data,
#       x = ~scenario,
#       y = ~catch,
#       type = "bar",
#       name = fishstock,
#       marker = list(color = bar_color),
#       hovertemplate = paste0(
#         "Stock: ", fishstock, "<br>",
#         "Scenario: %{x}<br>",
#         "Catches (tonnes): %{y:.0f}<extra></extra>"
#       )
#     )

#     # Add invisible hover lines for background info
#     p <- p %>%
#       plotly::add_trace(
#         data = stock_data,
#         x = ~scenario, y = rep(stock_adv$advice[1], nrow(stock_data)),
#         type = "scatter",
#         mode = "lines",
#         line   = list(color = "rgba(0,0,0,0)"),
#         marker = list(color = "rgba(0,0,0,0)"),
#         hovertemplate = "Advice (tonnes): %{y:.0f}<extra></extra>",        
#         showlegend = FALSE
#       ) %>%
#       plotly::add_trace(
#         data = stock_data,
#         x = ~scenario, y = rep(stock_adv$upper[1], nrow(stock_data)),
#         type = "scatter",
#         mode = "lines",
#         line   = list(color = "rgba(0,0,0,0)"),
#         marker = list(color = "rgba(0,0,0,0)"),
#         hovertemplate = "Upper limit (tonnes): %{y:.0f}<extra></extra>",
#         showlegend = FALSE
#       ) %>%
#       plotly::add_trace(
#         data = stock_data,
#         x = ~scenario, y = rep(stock_adv$lower[1], nrow(stock_data)),
#         type = "scatter",
#         mode = "lines",
#         line   = list(color = "rgba(0,0,0,0)"),
#         marker = list(color = "rgba(0,0,0,0)"),
#         hovertemplate = "Lower limit (tonnes): %{y:.0f}<extra></extra>",
#         showlegend = FALSE
#       )

#     # Apply layout and background shapes
#     p <- p %>%
#       plotly::layout(
#         xaxis = list(tickangle = 45),
#         yaxis = list(title = ylab, range = c(0, max_y)),
#         shapes = shape_list,
#         annotations = list(
#           x = 0.5,
#           y = 1.05,
#           text = paste0(fishstock),
#           xref = "paper",
#           yref = "paper",
#           xanchor = "center",
#           yanchor = "bottom",
#           showarrow = FALSE
#         )
#       )

#     subplots[[fishstock]] <- p
#   }

#   # Combine all into one plot
#   fig <- plotly::subplot(
#     subplots,
#     nrows = n_rows,
#     shareX = TRUE,
#     shareY = FALSE,
#     titleY = FALSE,
#     titleX = FALSE,
#     margin = 0.05
#   )

#   # Final layout adjustments
#   fig <- fig %>%
#     plotly::layout(
#       grid = list(rows = n_rows, columns = n_cols, pattern = "independent"),
#       showlegend = TRUE,
#       legend = list(
#         orientation = "h",
#         x = 0.5, y = 1.05, # center above the plot
#         xanchor = "center",
#         yanchor = "bottom",
#         tracegroupgap = 20
#       ),
#       margin = list(l = 80, b = 90, t = 100),
#       annotations = list(
#         list(
#           text = ylab,
#           x = -0.05, y = 0.5,
#           xref = "paper", yref = "paper",
#           showarrow = FALSE,
#           xanchor = "center",
#           yanchor = "middle",
#           textangle = -90, # vertical orientation
#           font = list(size = 16) # adjust font size
#         ),
#         list(
#           text = xlab,
#           x = .5, y = -0.15,
#           xref = "paper", yref = "paper",
#           showarrow = FALSE,
#           xanchor = "middle",
#           yanchor = "center",
#           # textangle = -90,   # vertical orientation
#           font = list(size = 16) # adjust font size
#         )
#       )
#     )
# }
# plot_catchScenStk_plotly <- function(data, adv, refTable,
#                                      ofwhich = FALSE,
#                                      xlab = "Scenarios",
#                                      ylab = "Catch (tonnes)") {
#   stopifnot(all(c("stock", "scenario", "catch") %in% names(data)))
#   stopifnot(all(c("stock", "advice") %in% names(adv)))

#   if (!"upper" %in% names(adv)) adv$upper <- adv$advice
#   if (!"lower" %in% names(adv)) adv$lower <- adv$advice

#   data <- dplyr::filter(data, stock %in% adv$stock)

#   unique_stocks <- unique(data$stock)
#   n_stocks <- length(unique_stocks)
#   n_cols <- min(4, n_stocks)
#   n_rows <- ceiling(n_stocks / n_cols)

#   # Global category order (shared x across subplots)
#   global_x <- as.character(unique(data$scenario))
#   pad_left <- "..pad_left.."
#   pad_right <- "..pad_right.."
#   while (pad_left %in% global_x) pad_left <- paste0(pad_left, "_")
#   while (pad_right %in% global_x) pad_right <- paste0(pad_right, "_")
#   cat_array <- c(pad_left, global_x, pad_right)

#   show_once <- function(stock, first_stock) identical(stock, first_stock)
#   first_stock <- unique_stocks[1]

#   subplots <- vector("list", length(unique_stocks))

#   for (i in seq_along(unique_stocks)) {
#     fishstock <- unique_stocks[i]
#     stock_data <- dplyr::filter(data, stock == fishstock)
#     stock_adv <- dplyr::filter(adv, stock == fishstock)[1, , drop = FALSE]

#     # Bars color
#     bar_color <- dplyr::filter(refTable, stock == fishstock)$col
#     if (length(bar_color) == 0) bar_color <- "grey"

#     # Thresholds
#     y_advice <- as.numeric(stock_adv$advice)
#     y_upper <- as.numeric(stock_adv$upper)
#     y_lower <- as.numeric(stock_adv$lower)

#     y_lo <- min(y_advice, y_upper, na.rm = TRUE)
#     y_hi <- max(y_advice, y_upper, na.rm = TRUE)
#     max_y <- max(c(stock_data$catch, y_upper, y_advice, y_lower), na.rm = TRUE) * 1.1

#     # We’ll span zones/lines across the padded x (pad_left, global_x..., pad_right)
#     x_zone <- cat_array

#     p <- plotly::plot_ly()

#     # --- ZONES (as filled scatter, now extend to edges via pads) ---
#     p <- p %>%
#       # Green: 0 -> y_lo
#       plotly::add_trace(
#         x = x_zone,
#         y = rep(y_lo, length(x_zone)),
#         type = "scatter", mode = "lines",
#         line = list(width = 0),
#         fill = "tozeroy", fillcolor = "rgba(0,128,0,0.15)",
#         name = "Below advice (zone)",
#         legendgroup = "zone_green",
#         showlegend = show_once(fishstock, first_stock),
#         hoverinfo = "none"
#       ) %>%
#       # Yellow: y_lo -> y_hi
#       plotly::add_trace(
#         x = x_zone,
#         y = rep(y_hi, length(x_zone)),
#         type = "scatter", mode = "lines",
#         line = list(width = 0),
#         fill = "tonexty", fillcolor = "rgba(255,215,0,0.15)",
#         name = "Within range (zone)",
#         legendgroup = "zone_yellow",
#         showlegend = show_once(fishstock, first_stock),
#         hoverinfo = "none"
#       ) %>%
#       # Red: y_hi -> max_y
#       plotly::add_trace(
#         x = x_zone,
#         y = rep(max_y, length(x_zone)),
#         type = "scatter", mode = "lines",
#         line = list(width = 0),
#         fill = "tonexty", fillcolor = "rgba(255,0,0,0.15)",
#         name = "Above upper (zone)",
#         legendgroup = "zone_red",
#         showlegend = show_once(fishstock, first_stock),
#         hoverinfo = "none"
#       )

#     # --- BARS (catches) ---
#     p <- p %>%
#       plotly::add_trace(
#         data = stock_data,
#         x = ~ as.character(scenario),
#         y = ~catch,
#         type = "bar",
#         marker = list(color = bar_color),
#         name = "Catch bars",
#         legendgroup = "bars",
#         showlegend = show_once(fishstock, first_stock),
#         hovertemplate = paste0(
#           "Stock: ", fishstock, "<br>",
#           "Scenario: %{x}<br>",
#           "Catches (tonnes): %{y:.0f}<extra></extra>"
#         )
#       )

#     # --- HORIZONTAL LINES (extend across pads) ---
#     p <- p %>%
#       plotly::add_trace(
#         x = x_zone, y = rep(y_advice, length(x_zone)),
#         type = "scatter", mode = "lines",
#         line = list(color = "#6e6e6e", width = 1.5, dash = "solid"),
#         name = "Advice", legendgroup = "line_advice",
#         showlegend = show_once(fishstock, first_stock),
#         hovertemplate = "Advice (tonnes): %{y:.0f}<extra></extra>"
#       ) %>%
#       plotly::add_trace(
#         x = x_zone, y = rep(y_upper, length(x_zone)),
#         type = "scatter", mode = "lines",
#         line = list(color = "#6e6e6e", width = 1.5, dash = "dash"),
#         name = "Upper limit", legendgroup = "line_upper",
#         showlegend = show_once(fishstock, first_stock),
#         hovertemplate = "Upper limit (tonnes): %{y:.0f}<extra></extra>"
#       ) %>%
#       plotly::add_trace(
#         x = x_zone, y = rep(y_lower, length(x_zone)),
#         type = "scatter", mode = "lines",
#         line = list(color = "#6e6e6e", width = 1.5, dash = "dot"),
#         name = "Lower limit", legendgroup = "line_lower",
#         showlegend = show_once(fishstock, first_stock),
#         hovertemplate = "Lower limit (tonnes): %{y:.0f}<extra></extra>"
#       )

#     # Axes & small title per panel; hide pad ticks
#     p <- p %>%
#       plotly::layout(
#         xaxis = list(
#           title = "",
#           tickangle = 45,
#           type = "category",
#           categoryorder = "array",
#           categoryarray = cat_array,
#           tickmode = "array",
#           tickvals = global_x, # show only real categories
#           ticktext = global_x
#         ),
#         yaxis = list(title = ylab, range = c(0, max_y)),
#         annotations = list(list(
#           x = 0.5, y = 1.05, text = paste0(fishstock),
#           xref = "paper", yref = "paper",
#           xanchor = "center", yanchor = "bottom",
#           showarrow = FALSE
#         ))
#       )

#     subplots[[i]] <- p
#   }

#   fig <- plotly::subplot(
#     subplots,
#     nrows = n_rows, shareX = TRUE, shareY = FALSE,
#     titleY = FALSE, titleX = FALSE, margin = 0.05
#   )

#   fig <- fig %>%
#     plotly::layout(
#       barmode = "group",
#       showlegend = TRUE,
#       legend = list(
#         orientation = "h",
#         x = 0, # left-align so wrapping behaves nicely
#         xanchor = "left",
#         y = 1.08, # move above the plot
#         itemsizing = "constant",
#         itemwidth = 130, # << try 100–160 until it looks right
#         tracegroupgap = 30, # extra space between grouped items
#         font = list(size = 12)
#       ),
#       margin = list(l = 80, b = 90, t = 150, r = 120),
#       annotations = list(
#         list(
#           text = ylab,
#           x = -0.05, y = 0.5,
#           xref = "paper", yref = "paper",
#           showarrow = FALSE,
#           xanchor = "center", yanchor = "middle",
#           textangle = -90, font = list(size = 16)
#         ),
#         list(
#           text = xlab,
#           x = 0.5, y = -0.15,
#           xref = "paper", yref = "paper",
#           showarrow = FALSE,
#           xanchor = "middle", yanchor = "center",
#           font = list(size = 16)
#         )
#       )
#     )

#   fig
# }

plot_catchScenStk_plotly <- function(data, adv, refTable,
                                     ofwhich = FALSE,
                                     xlab = "Scenarios",
                                     ylab = "Catch (tonnes)") {
  stopifnot(all(c("stock", "scenario", "catch") %in% names(data)))
  stopifnot(all(c("stock", "advice") %in% names(adv)))

  if (!"upper" %in% names(adv)) adv$upper <- adv$advice
  if (!"lower" %in% names(adv)) adv$lower <- adv$advice

  data <- dplyr::filter(data, stock %in% adv$stock)

  unique_stocks <- unique(data$stock)
  n_stocks <- length(unique_stocks)
  n_cols <- min(4, n_stocks)
  n_rows <- ceiling(n_stocks / n_cols)

  # Global scenario order + numeric x index (for edge-to-edge zones)
  global_x <- as.character(unique(data$scenario))
  idx_map  <- setNames(seq_along(global_x), global_x)
  n_cat    <- length(global_x)

  show_once <- function(stock, first_stock) identical(stock, first_stock)
  first_stock <- unique_stocks[1]

  subplots <- vector("list", length(unique_stocks))

  for (i in seq_along(unique_stocks)) {
    fishstock <- unique_stocks[i]
    stock_data <- dplyr::filter(data, stock == fishstock)
    stock_adv  <- dplyr::filter(adv, stock == fishstock)[1, , drop = FALSE]

    # Numeric positions for bars
    stock_data$idx <- as.integer(idx_map[as.character(stock_data$scenario)])

    # Bar color
    bar_color <- dplyr::filter(refTable, stock == fishstock)$col
    if (length(bar_color) == 0) bar_color <- "grey"

    # Thresholds
    y_advice <- as.numeric(stock_adv$advice)
    y_upper  <- as.numeric(stock_adv$upper)
    y_lower  <- as.numeric(stock_adv$lower)

    y_lo <- min(y_advice, y_upper, na.rm = TRUE)
    y_hi <- max(y_advice, y_upper, na.rm = TRUE)
    max_y <- max(c(stock_data$catch, y_upper, y_advice, y_lower), na.rm = TRUE) * 1.1

    # X for zones/lines (reach edges)
    x_zone <- c(0.5, seq_len(n_cat), n_cat + 0.5)

    p <- plotly::plot_ly()

    # --- ZONES (legend shown once) ---
    p <- p %>%
      plotly::add_trace(
        x = x_zone, y = rep(y_lo, length(x_zone)),
        type = "scatter", mode = "lines",
        line = list(width = 0),
        fill = "tozeroy", fillcolor = "rgba(0,128,0,0.15)",
        name = "Below advice",
        legendgroup = "zone_green",
        showlegend = show_once(fishstock, first_stock),
        hoverinfo = "skip"
      ) %>%
      plotly::add_trace(
        x = x_zone, y = rep(y_hi, length(x_zone)),
        type = "scatter", mode = "lines",
        line = list(width = 0),
        fill = "tonexty", fillcolor = "rgba(255,215,0,0.15)",
        name = "Within advice range",
        legendgroup = "zone_yellow",
        showlegend = show_once(fishstock, first_stock),
        hoverinfo = "skip"
      ) %>%
      plotly::add_trace(
        x = x_zone, y = rep(max_y, length(x_zone)),
        type = "scatter", mode = "lines",
        line = list(width = 0),
        fill = "tonexty", fillcolor = "rgba(255,0,0,0.15)",
        name = "Above advice",
        legendgroup = "zone_red",
        showlegend = show_once(fishstock, first_stock),
        hoverinfo = "skip"
      )

    # --- BARS (legend hidden) ---
    p <- p %>%
      plotly::add_trace(
        data = stock_data,
        x = ~idx, y = ~catch,
        type = "bar",
        marker = list(color = bar_color),
        name = "Catch bars",
        legendgroup = "bars",
        showlegend = FALSE,  # << hidden as requested
        hovertemplate = paste0(
          "Stock: ", fishstock, "<br>",
          "Scenario: %{customdata}<br>",
          "Catches (tonnes): %{y:.0f}<extra></extra>"
        ),
        customdata = stock_data$scenario
      )

    # --- HORIZONTAL LINES (legend shown once) ---
    p <- p %>%
      plotly::add_trace(
        x = x_zone, y = rep(y_advice, length(x_zone)),
        type = "scatter", mode = "lines",
        line = list(color = "#6e6e6e", width = 2, dash = "solid"),
        name = "Advice",
        legendgroup = "line_advice",
        showlegend = show_once(fishstock, first_stock),  # << show in legend
        hovertemplate = "Advice (tonnes): %{y:.0f}<extra></extra>"
      ) %>%
      plotly::add_trace(
        x = x_zone, y = rep(y_upper, length(x_zone)),
        type = "scatter", mode = "lines",
        line = list(color = "#6e6e6e", width = 2, dash = "dash"),
        name = "F<sub>MSY upper</sub>",
        legendgroup = "line_upper",
        showlegend = show_once(fishstock, first_stock),
        hovertemplate = "Upper limit (tonnes): %{y:.0f}<extra></extra>"
      ) %>%
      plotly::add_trace(
        x = x_zone, y = rep(y_lower, length(x_zone)),
        type = "scatter", mode = "lines",
        line = list(color = "#6e6e6e", width = 2, dash = "dot"),
        name = "F<sub>MSY lower</sub>",
        legendgroup = "line_lower",
        showlegend = show_once(fishstock, first_stock),
        hovertemplate = "Lower limit (tonnes): %{y:.0f}<extra></extra>"
      )

    # Panel header & axes
    p <- p %>%
      plotly::layout(
        xaxis = list(
          title = "",
          tickangle = 45,
          tickmode = "array",
          tickvals = seq_len(n_cat),
          ticktext = global_x,
          range = c(0.5, n_cat + 0.5)
        ),
        yaxis = list(title = "", range = c(0, max_y)),
        bargap = 0.2,
        annotations = list(list(
          x = 0.5, y = 1.02, text = paste0(fishstock),
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "bottom",
          showarrow = FALSE
        ))
      )

    subplots[[i]] <- p
  }

  legend_itemwidth <- 40  # shorter legend line samples (try 50–80)

  fig <- plotly::subplot(
    subplots,
    nrows = n_rows, shareX = TRUE, shareY = FALSE,
    titleY = FALSE, titleX = FALSE, margin = 0.05
  ) %>%
    plotly::layout(
      barmode = "group",
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0, y = 1.12,            # top, left-aligned to avoid clipping
        xanchor = "left", yanchor = "bottom",
        itemsizing = "constant",
        itemwidth = legend_itemwidth,  # << shorter line samples
        tracegroupgap = 10,
        font = list(size = 12),
        bgcolor = "rgba(255,255,255,0.85)"
      ),
      margin = list(l = 80, b = 110, t = 130, r = 20),
      annotations = list(
        list(
          text = ylab,
          x = -0.05, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "center", yanchor = "middle",
          textangle = -90, font = list(size = 16)
        ),
        list(
          text = xlab,
          x = 0.5, y = -0.12,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "middle", yanchor = "top",
          font = list(size = 16)
        )
      )
    )

  fig
}





#############################################################################
#original
# plot_effortFltStk_plotly <- function(data, refTable,
#   xlab = "Stock", ylab = "Effort ['000 KW days]",
#   fillLegendTitle = "Stock", colLegendTitle = "Limiting stock",
#   linewidthDefault = 0.5, linewidthLimitation = 1)
# {
#   stkFill <- data.frame(stock = unique(data$stock))
#   stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
#   stkFill <- stkFill[order(stkFill$order), ]
#   stkColors <- stkFill$col
#   names(stkColors) <- stkFill$stock
#   stkColorScale <- scale_colour_manual(name = fillLegendTitle,
#     values = stkColors, aesthetics = c("fill"))
#   data$stock <- factor(data$stock, levels = stkFill$stock)

#   p <- ggplot(data) +
#     aes(x = stock, y = quotaEffort, fill = stock,
#       color = Limitation, group = fleet) +
#     facet_wrap(fleet ~ ., scales = "free_y", ncol = 3) +
#     geom_bar(stat = "identity", linewidth = linewidthDefault, fill = NA,
#       color = "black") +
#     geom_bar(stat = "identity", linewidth = linewidthLimitation) +
#     geom_hline(data = data, aes(yintercept = sqEffort), lty = 2) +
#     scale_color_manual(values = c('green', 'red'), na.value = NA,
#       limits = c('least','most'), labels = c("least", "most (*)")) +
#     geom_text(data = subset(data, Limitation == "most"),
#       aes(label = "*"), vjust = 0.2, show.legend = FALSE) +
#     xlab(xlab) + ylab(ylab) + stkColorScale + theme_bw() +
#     theme(
#       axis.text.x = element_text(angle = 90, hjust = 1,
#       vjust = 0.5, size = 7), panel.grid = element_blank(),
#       text = element_text(size = 9), strip.text = element_text(size = 9)) +
#     guides(
#       colour = guide_legend(order = 2, override.aes = list(fill = NA)),
#       fill = guide_legend(order = 1,
#         override.aes = list(color = "black", linewidth = linewidthDefault))) +
#     labs(fill = fillLegendTitle, color = colLegendTitle)
#   return(plotly::ggplotly(p))
# }

################################################################################

plot_effortFltStk_plotly <- function(
  data, refTable,
  xlab = "Stock", ylab = "Effort (Thousands KW days)",
  linewidthDefault = 0.5, linewidthLimitation = 1.5,
  ncol = 4, rowHeight = 200) {

  # Build stock color mapping (keep order from refTable)
  stkFill <- data.frame(stock = unique(data$stock))
  stkFill <- merge(x = stkFill, y = refTable, all.x = TRUE)
  stkFill <- stkFill[order(stkFill$order), ]
  stkColors <- stkFill$col
  names(stkColors) <- stkFill$stock

  # Convert stocks to factors to maintain order everywhere
  stocks <- stkFill$stock
  data$stock <- factor(as.character(data$stock), levels = stocks)

  # Get unique fleets (keep input order)
  fleets <- unique(data$fleet)

  # Calculate dynamic height
  # subplots <- list()
  unique_fleets <- unique(data$fleet)
  n_fleets <- length(unique_fleets)
  n_cols <- min(4, n_fleets)
  n_rows <- ceiling(length(fleets) / ncol)
  fig_height <- rowHeight * n_rows

  # Legend tracking
  legend_shown_stocks <- setNames(rep(FALSE, length(stocks)), stocks)
  legend_shown_lim <- list(most = FALSE, least = FALSE)
  legend_shown_sq <- FALSE

  plot_list <- list()

  for (i in seq_along(fleets)) {
    fleet <- fleets[i]
    fleet_data <- data[data$fleet == fleet, ]

    p <- plotly::plot_ly()

    # Bars: one trace per stock
    for (stock in stocks) {
      stock_data <- fleet_data[fleet_data$stock == stock, , drop = FALSE]
      if (nrow(stock_data) == 0) next

      # Show legend for each stock only once across all fleets
      show_legend_stock <- !legend_shown_stocks[[stock]]
      if (show_legend_stock) legend_shown_stocks[[stock]] <- TRUE

      p <- p %>% plotly::add_bars(
        data = stock_data,
        x = ~stock,
        y = ~quotaEffort,
        name = stock,
        width = 1,
        legendgroup = paste0("stock_", stock),
        marker = list(color = stkColors[stock], line = list(width = 0)),
        showlegend = show_legend_stock,
        hovertemplate = paste0(
        "<b>Stock:</b> ", stock, "<br>",
        "<b>Effort:</b> %{y:.2f}<br>",
        "<b>Fleet:</b> ", fleet, "<extra></extra>"
    )
      )
    }

    # Limitation outlines: only for "most" and "least"
for (stock in stocks) {
  stock_data <- fleet_data[fleet_data$stock == stock, , drop = FALSE]
  if (nrow(stock_data) == 0) next
  if (all(is.na(stock_data$Limitation))) next

  lim_val <- stock_data$Limitation[1]
  if (is.na(lim_val) || lim_val == "intermediate") next   # <-- skip intermediate

  limitation_color <- ifelse(lim_val == "most", "red", "green")
  lim_name <- ifelse(lim_val == "most", "most (*)", "least")

  show_legend_lim <- FALSE
  if (lim_val == "most" && !legend_shown_lim$most) {
    show_legend_lim <- TRUE; legend_shown_lim$most <- TRUE
  } else if (lim_val == "least" && !legend_shown_lim$least) {
    show_legend_lim <- TRUE; legend_shown_lim$least <- TRUE
  }

  p <- p %>% plotly::add_bars(
    data = stock_data,
    x = ~stock,
    y = ~quotaEffort,
    name = lim_name,
    width = 1,
    legendgroup = paste0("lim_", lim_val),
    marker = list(
      color = "rgba(0,0,0,0)",
      line = list(color = limitation_color, width = linewidthLimitation)
    ),
    showlegend = show_legend_lim,
    hovertemplate = paste0(
      "<b>Stock:</b> ", stock, "<br>",
      "<b>Effort:</b> %{y:.2f}<br>",
      "<b>Limitation:</b> ", lim_val, "<br>",
      "<b>Fleet:</b> ", fleet, "<extra></extra>"
    )
  )

  if (lim_val == "most") {
    y_pos <- max(stock_data$quotaEffort, na.rm = TRUE)
    p <- p %>% plotly::add_annotations(
      x = stock,
      y = y_pos,
      text = "*",
      color = "red",
      showarrow = FALSE,
      font = list(size = 14),
      yshift = 10
    )
  }
}

    # Status quo line
    if (nrow(fleet_data) > 0 && !is.na(fleet_data$sqEffort[1])) {
      show_legend_sq <- !legend_shown_sq
      if (show_legend_sq) legend_shown_sq <- TRUE

      line_df <- data.frame(stock = factor(stocks, levels = stocks),
                            sq = rep(fleet_data$sqEffort[1], length(stocks)),
                            stringsAsFactors = FALSE)

      p <- p %>% plotly::add_lines(
        data = line_df,
        x = ~stock,
        y = ~sq,
        name = "Status quo effort",
        legendgroup = "sq",
        line = list(dash = "dash", color = "black"),
        showlegend = show_legend_sq,
        hovertemplate = paste0("Status quo effort: ", fleet_data$sqEffort[1],
                               "<br>Fleet: ", fleet, "<extra></extra>")
      )
    }

    # Compute axis titles only for bottom / left plots
    row_index <- ceiling(i / ncol)
    show_x_title <- if (row_index == n_rows) xlab else ""
    show_y_title <- if (((i - 1) %% ncol) == 0) ylab else ""

    p <- p %>% plotly::layout(
      xaxis = list(
        title = show_x_title,
        tickangle = 45,
        categoryorder = "array",
        categoryarray = stocks,
        type = "category"
      ),
      yaxis = list(
        title = show_y_title,
        type = "linear"
      ),
      barmode = "overlay"
    )

    # Fleet title annotation
    p <- p %>% plotly::add_annotations(
      x = 0.5, 
      y = 1.05, 
      xref = "paper", 
      yref = "paper",
      text = fleet, 
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE, 
      font = list(size = 16)
      # yshift = 10
    )

    

    plot_list[[i]] <- p
  }

  # Combine subplots
  fig <- plotly::subplot(
    plot_list, 
    nrows = n_rows,  
    shareX = TRUE, 
    shareY = FALSE,
    titleX = FALSE, 
    titleY = FALSE
  )
    
  fig <- fig %>% plotly::layout( 
    height = fig_height,   
    margin = list(t = 120, b = 90, r = 90),
    legend = list(
      orientation = "h",
      x = 0.5, y = 1.05,                # center above the plot
      xanchor = "center", 
      yanchor = "bottom",
      tracegroupgap = 30
    ),
    barmode = "overlay",
    xaxis = list(categoryorder = "array", categoryarray = stocks, tickangle = 45),
    annotations = list(
      list(
        text = ylab,
        x = -0.05, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "middle",
        textangle = -90,   # vertical orientation
        font = list(size = 16) # adjust font size
      ),
      list(
        text = xlab,
        x = .5, y = -0.8,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "middle",
        yanchor = "center",
        # textangle = -90,   # vertical orientation
        font = list(size = 16) # adjust font size
      )
    )
  )

  return(fig)
}





#############################################################################

plot_landByMetStock_plotly <- function(data, refTable,
                                       xlab = "Métiers", ylab = "Landings (Thousand tonnes)",
                                       fillLegendTitle = "Stock") {
  if (is.null(data)) {
    stop("object, data, does not exist")
  }

  if (!all(c("metier", "stock", "value") %in% colnames(data))) {
    stop("Column names not as expected")
  }

  # Merge color reference
  data <- dplyr::left_join(data, refTable, by = "stock")
  
  # Order stocks by 'order' column in refTable and convert value to '000 t
  data <- dplyr::mutate(
    data,
    stock = factor(stock, levels = refTable$stock[order(refTable$order, decreasing = TRUE)]),
    value = value / 1000
  )
  data$metier <- as.factor(data$metier)
  # Initialize plotly object
  plot <- plotly::plot_ly()

  # Add a trace per stock
  for (stk in levels(data$stock)) {
    stk_data <- dplyr::filter(data, stock == stk)
    stk_col <- unique(stk_data$col)

    plot <- plotly::add_trace(
      plot,
      data = stk_data,
      x = ~metier,
      y = ~value,
      type = 'bar',
      name = stk,
      marker = list(color = stk_col),
      hovertemplate = paste(
        "<b>Metier:</b> %{x}<br>",
        "<b>Stock:</b> ", stk, "<br>",
        "<b>Landings:</b> %{y:.2f} thousand t<br><extra></extra>"
      )
    )
  }

  # Apply layout
  plot <- plotly::layout(
    plot,
    barmode = 'stack',
    xaxis = list(title = xlab, tickangle = 45),
    yaxis = list(title = ylab),
    legend = list(title = list(text = fillLegendTitle)),
    margin = list(b = 90),
    font = list(size = 16)
  )

  return(plot)
}


#######################################################################
plot_landByStock_plotly <- function(data, refTable,
                                    ylab = "Landings (Thousand tonnes)",
                                    fillLegendTitle = "Stock") {
  if (is.null(data)) stop("object, data, does not exist")
  if (!all(c("stock", "value") %in% colnames(data))) {
    stop("Column names not as expected")
  }

  # Merge color and ordering info
  data <- dplyr::left_join(data, refTable, by = "stock")
  data$stock <- factor(data$stock, levels = refTable$stock[order(refTable$order)])
  data$value <- round(data$value / 1000, 1) # Convert to '000 t
  data <- droplevels(data)
  data <- data[order(data$stock), ]

  # Create the pie chart
  plot <- plotly::plot_ly(
    data = data,
    labels = ~stock,
    values = ~value,
    type = "pie",
    marker = list(colors = data$col, line = list(color = "black", width = 1)),
    textinfo = "label+percent",
    hovertemplate = paste(
      "<b>Stock:</b> %{label}<br>",
      "<b>Landings:</b> %{value} thousand t<br>",
      "<b>Percent:</b> %{percent}<extra></extra>"
    ),
    domain = list(x = c(0, 1), y = c(0.1, 0.9)) # shrink vertically to avoid cutoffs
    # hole = 0.3   # uncomment if you prefer donut style
  )

  # Apply layout
  plot <- plotly::layout(
    plot,
    title = list(text = ylab, x = 0.5),
    showlegend = TRUE,
    legend = list(title = list(text = fillLegendTitle), font = list(size = 16)),
    margin = list(t = 40, b = 40, l = 20, r = 20),
    font = list(size = 14)
  )

  return(plot)
}


#######################################################################
plot_catchComp_plotly <- function(dataComposition, refTable, filters=NULL,
  selectors = "metier", divider = NULL, yvar = "landings"){

    data <- subset(dataComposition, scenario == "min")
    # add country and area identifiers (if desired)
      tmp <- strsplit(as.character(data$metier), ".", fixed = TRUE)
      data$area <- unlist(lapply(tmp, FUN = function(x) {
        ifelse(length(x) == 2, x[2], NA)
      }))
      tmp <- strsplit(data$fleet, "_", fixed = TRUE)
      data$country <- unlist(lapply(tmp, FUN = function(x) {
        ifelse(length(x) == 2, x[1], NA)
      }))
      # replace stock with ICES stock code
      data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]


  # filters filter the data
  # selectors select the level of data aggregation. They get pasted together
  # into a label which is used to aggregate. i.e. the x axis labels
  # divider provides a variable over which to disaggregate the data for
  # comparison. i.e. facets in the plot
  if(!is.null(filters)){
    # filter
    for (var in names(filters)){
      data <- data %>% filter(.data[[var]] %in% filters[[var]]) # this works but might be a deprecated method
    }
  }

  if(length(divider)>1){
    stop("only 1 variable can be provided as a divider")
  }

  # check area codes. NA = notSpecified
  if(any(is.na(data$area))){
    data$area[is.na(data$area)] <- "notSpecified"
  }

  # aggregate by selectors by concatenating selectors into 1 label
  # label and stock are always selectors
  data$label <- apply(select(ungroup(data),all_of(selectors)),1,paste,collapse="_")
  data <- data %>% group_by(across(all_of(c("label","stock",divider)))) %>%
    summarise(VAR=sum(get(yvar),na.rm=T))

  # get colour scale by merging with refTable
  data <- left_join(data,refTable,by="stock")
  tmp <- unique(data[,c("stock","col", "order")])
  tmp <- tmp[order(tmp$order),]
  stkColors <- tmp$col
  names(stkColors) <- tmp$stock
  stkColorScale <- scale_colour_manual(name = "stock", values = stkColors,
    aesthetics = c("colour", "fill"))

  # ensure plotting order
  data$stock <- factor(data$stock, levels = tmp$stock)
  
  # plot
  p <- ggplot(data,aes(x=label,y=VAR,colour=stock,fill=stock))+
    geom_col(position="fill")+
    coord_flip()+ labs(x="",y="",fill="",colour="")+
    theme_bw()+stkColorScale +guides(fill=guide_legend(ncol=1))+guides(colour=guide_legend(ncol=1))

  if(!is.null(divider)){
    p <- p + facet_wrap(divider, scales = "free")
  }
  p <- plotly::ggplotly(p, tooltip = c("label", "VAR", "stock")) %>%
    plotly::layout(xaxis = list(title = ""), yaxis = list(title = ""),
                   legend = list(title = list(text = 'stock')))
  return(p)

}



# plot_catchComp_plotly <- function(data, refTable, filters=NULL,
#   selectors = "metier", divider = NULL, yvar = "landings"){

#   # Check if the necessary columns exist in the data
#   required_columns <- c(selectors, "stock", yvar, divider)
#   missing_columns <- setdiff(required_columns, names(data))
#   if(length(missing_columns) > 0){
#     stop(paste("The following required columns are missing from the data:", paste(missing_columns, collapse = ", ")))
#   }

#   # filters filter the data
#   if(!is.null(filters)){
#     for (var in names(filters)){
#       if(!var %in% names(data)){
#         stop(paste("Filter column", var, "not found in data"))
#       }
#       data <- dplyr::filter(data, .data[[var]] %in% filters[[var]])
#     }
#   }

#   if(length(divider) > 1){
#     stop("only 1 variable can be provided as a divider")
#   }

#   # check area codes. NA = notSpecified
#   if(any(is.na(data$area))){
#     data$area[is.na(data$area)] <- "notSpecified"
#   }

#   # aggregate by selectors by concatenating selectors into 1 label
#   data$label <- apply(dplyr::select(dplyr::ungroup(data), dplyr::all_of(selectors)), 1, paste, collapse = "_")
#   data <- data %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("label", "stock", divider)))) %>%
#     dplyr::summarise(VAR = sum(.data[[yvar]], na.rm = TRUE), .groups = 'drop')

#   # get colour scale by merging with refTable
#   data <- dplyr::left_join(data, refTable, by = "stock")
#   tmp <- unique(data[, c("stock", "col", "order")])
#   tmp <- tmp[order(tmp$order), ]
#   stkColors <- tmp$col
#   names(stkColors) <- tmp$stock

#   # ensure plotting order
#   data$stock <- factor(data$stock, levels = tmp$stock)

#   # plot
#   p <- plotly::plot_ly(data, x = ~label, y = ~VAR, color = ~stock, colors = stkColors, type = 'bar') %>%
#     plotly::layout(barmode = 'stack', xaxis = list(title = ''), yaxis = list(title = ''), 
#            legend = list(title = list(text = 'stock')))

#   if(!is.null(divider)){
#     p <- p %>% plotly::layout(facets = as.formula(paste('~', divider)), scales = 'free')
#   }

#   return(p)
# }
