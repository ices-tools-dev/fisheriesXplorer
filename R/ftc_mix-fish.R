# function to download data from github
download_github_data <- function(repo_owner, repo_name, file_path) {
    # Fetch file metadata from GitHub API
    response <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}", 
                   owner = repo_owner, 
                   repo = repo_name, 
                   path = file_path)
    
    # Extract raw file URL
    download_url <- response$download_url
    
    # Download and read the file
    df <- read.csv(download_url)
    
    return(df)
}


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


plot_catchScenStk_plotly <- function(data, adv, ofwhich = FALSE,
                                     xlab = "Scenario", ylab = "Catch [t]") {
  
  # Add dummy advice range values if missing
  if(!"upper" %in% names(adv)){
    adv$upper <- adv$advice
  }
  if(!"lower" %in% names(adv)){
    adv$lower <- adv$advice
  }
  
  
  # Create a list to store the subplots
  subplots <- list()
  
  # Get unique stocks
  unique_stocks <- unique(data$stock)
  n_stocks <- length(unique_stocks)
  n_cols <- min(4, n_stocks)  # Max 4 columns
  n_rows <- ceiling(n_stocks / n_cols)  # Adjust row count based on column count
  
  for (fishstock in unique_stocks) {
    stock_data <- dplyr::filter(data, stock == fishstock)
    stock_adv <- dplyr::filter(adv, stock == fishstock)
    
    p <- plotly::plot_ly(stock_data, 
                        x = ~scenario, 
                        y = ~catch, 
                        type = 'bar', 
                        name = fishstock, 
                        marker = list(color = I('grey35')),
                        hovertemplate = paste0(
                          "Stock: ", fishstock, "<br>",
                          "Scenario: %{x}<br>",
                          "Catches (tonnes): %{y:.0f}<extra></extra>"                          
                        )) %>% 
      plotly::layout(
        xaxis = list(title = xlab, tickangle = 45),
        yaxis = list(title = ylab, range = c(0, max(stock_data$catch)+0.1*max(stock_data$catch))),
        # margin = list(l = 50, r = 50, t = 50, b = 50),
        shapes = list(
          list(type = 'rect', x0 = -0.5, x1 = 5, y0 = 0, y1 = stock_adv$advice[1], 
               fillcolor = 'green', opacity = 0.17, line = list(width = 0)),
          list(type = 'rect', x0 = -0.5, x1 = 5, y0 = stock_adv$advice[1], y1 = stock_adv$upper[1], 
               fillcolor = 'yellow', opacity = 0.17, line = list(width = 0)),
          list(type = 'rect', x0 = -0.5, x1 = 5, y0 = stock_adv$upper[1], y1 = max(stock_data$catch)+0.1*max(stock_data$catch), 
               fillcolor = 'red', opacity = 0.17, line = list(width = 0)),
          list(type = "line", x0 = -0.5, x1 = 5, y0 = stock_adv$advice[1], y1 = stock_adv$advice[1], 
               line = list(color = "black", width = 2, dash = "solid")),
          list(type = "line", x0 = -0.5, x1 = 5, y0 = stock_adv$upper[1], y1 = stock_adv$upper[1], 
               line = list(color = "black", width = 2, dash = "dash")),
          list(type = "line", x0 = -0.5, x1 = 5, y0 = stock_adv$lower[1], y1 = stock_adv$lower[1], 
               line = list(color = "black", width = 2, dash = "dash"))
        ),
        annotations = list(
             x = 0.5,
             y = 1.05,
             text = paste0(fishstock),
             xref = "paper",
             yref = "paper",
             xanchor = "center",  # center of text
             yanchor = "bottom",  # bottom of text
             showarrow = FALSE
           )
      )
    
    # if (ofwhich) {
    #   p <- p %>%
    #     plotly::add_trace(data = stock_adv, x = ~scenario, y = ~advice_ofwhich, type = 'scatter', mode = 'lines', 
    #                       line = list(color = '#85AD00', dash = 'dash'), name = 'Advice of which') %>%
    #     plotly::add_trace(data = stock_data, x = ~scenario, y = ~catch_ofwhich, type = 'bar', name = 'Catch of which', 
    #                       marker = list(color = '#85AD00', pattern = list(shape = 'crosshatch')))
    # }
    
    subplots[[fishstock]] <- p
  }
  
  # Combine subplots into one figure
  fig <- plotly::subplot(subplots, 
                        nrows = ceiling(length(unique_stocks) / 4),
                        shareX = TRUE,  
                        shareY = FALSE, 
                        titleY = FALSE, 
                        titleX = TRUE,
                        margin = 0.05) %>% #
                        plotly::layout(
                                        height = 200 * n_rows,
                                        grid = list(rows = n_rows, 
                                                    columns = n_cols, 
                                                    pattern = "independent"),
                                                    showlegend = FALSE
                                                    
                                                    
                                                    
                                                    # margin = list(l = 50, r = 50, t = 50, b = 50) 
  )
#   fig <- fig  %>% plotly::layout(height = 200 * ceiling(length(unique_stocks) / 4))
  fig
}