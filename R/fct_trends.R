#' trends 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom dplyr n anti_join case_match
#' @noRd

prepare_stock_trends <- function(df) {
  
  # Identify and remove duplicate values for 'MEAN'
  mean_duplicates <- df %>%
    group_by(Value) %>%
    filter(n() > 1, StockKeyLabel == "MEAN")
  
  # Remove duplicates and filter relevant metrics
  cleaned_data <- df %>%
    anti_join(mean_duplicates) %>%
    filter(Metric %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
    mutate(Metric = case_match(Metric, 
                           "F_FMSY" ~ "F/F[MSY]", 
                           "SSB_MSYBtrigger" ~ "SSB/MSY~B[trigger]"))
  
  return(cleaned_data)
}

#' Title
#'
#' @param df 
#' @param guild 
#' @param caption_year 
#' @param caption_month 
#'
#' @return
#' @export
#' @importFrom ggthemes tableau_color_pal
#' @importFrom scales breaks_extended
#' @import ggplot2 
#' 
plot_stock_trends_app <- function(df, caption_year, caption_month) {
  
  # Adjust stock key labels and set color values
  stock_labels <- sort(setdiff(unique(df$StockKeyLabel), "MEAN"))
  stock_colors <- tableau_color_pal("Tableau 20")(length(stock_labels))
  names(stock_colors) <- stock_labels
  stock_colors["MEAN"] <- "black"  # Add black color for 'MEAN'
  
  # Prepare caption text
  caption_text <- sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen", caption_month, caption_year)
  
  # Split data into 'mean' and 'non-mean' subsets
  mean_data <- df %>%
    filter(StockKeyLabel == "MEAN")
  non_mean_data <- df %>%
    filter(StockKeyLabel != "MEAN")
  
  # Set up the plot aesthetics and layers
  plot <- ggplot(non_mean_data, aes(x = Year, y = Value, color = StockKeyLabel, fill = StockKeyLabel)) +
    geom_hline(yintercept = 1, color = "grey60") +
    theme_bw(base_size = 9) +
    scale_color_manual(values = stock_colors) +
    scale_fill_manual(values = stock_colors) +
    scale_x_continuous(breaks = breaks_extended(n = 5)) +
    guides(fill = FALSE) +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 9, angle = 0, hjust = 0),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.title = element_blank(),
          plot.caption = element_text(size = 6)) +
    labs(title = element_blank(), x = "Year", y = "", caption = caption_text) +
    facet_wrap(~Metric, scales = "free_y", labeller = label_parsed, strip.position = "left", ncol = 1, nrow = 2) +
    geom_line(data = non_mean_data, alpha = 0.8) +
    geom_line(data = mean_data, alpha = 0.9, size = 1.15)
  
  return(plot)
}