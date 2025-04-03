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
    theme_bw(base_size = 22) +
    scale_color_manual(values = stock_colors) +
    scale_fill_manual(values = stock_colors) +
    scale_x_continuous(breaks = breaks_extended(n = 5)) +
    guides(fill = FALSE) +
    theme(legend.position = "right",
          strip.text = element_text(size = 13, angle = 0, hjust = 0),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.title = element_blank(),
          plot.caption = element_text(size = 10)) +
    labs(title = element_blank(), x = "Year", y = "", caption = caption_text) +
    facet_wrap(~Metric, scales = "free_y", labeller = label_parsed, strip.position = "left", ncol = 1, nrow = 2) +
    geom_line(data = non_mean_data, alpha = 0.8) +
    geom_line(data = mean_data, alpha = 0.9, size = 1.15)
  
  return(plot)
}

plot_catch_trends_app <- function(x,type = c("COMMON_NAME", "COUNTRY", "GUILD"),
                              line_count = 10,
                              plot_type = c("line", "area"),
                              preliminary_catches = TRUE,
                              official_catches_year = cap_year-1,
                              return_data = FALSE) {
        capyear <- official_catches_year-1
        capyear <- as.character(capyear)
        cap_lab <-ggplot2::labs(x = "",
                       y = "Landings (thousand tonnes)",
                       caption = sprintf(paste0("Historical Nominal Catches 1950-2010, \nOfficial Nominal Catches 2006-",capyear,"\nPreliminary Catches ", official_catches_year, "\n ICES, Copenhagen.")))


        df <- dplyr::rename(x, type_var = setNames(type , "type_var"))
        df <- dplyr::rename(df, type_var= type_var...type_var)

        if(type == "COMMON_NAME"){
        df$type_var[which(df$type_var == "Angler(=Monk)")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Monkfishes nei")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Atlantic herring")] <- "herring"
        df$type_var[which(df$type_var == "Atlantic cod")] <- "cod"
        df$type_var[which(df$type_var == "Anglerfishes nei")] <- "Anglerfish spp"
        df$type_var[which(df$type_var == "Megrims nei")] <- "Megrim"
        df$type_var[which(df$type_var == "Norway lobster")] <- "Nephrops"
        df <- dplyr::mutate(df,type_var = gsub("European ", "", type_var),
                              type_var = gsub("Sandeels.*", "sandeel", type_var),
                              type_var = gsub("Finfishes nei", "undefined finfish", type_var),
                              type_var = gsub("Blue whiting.*", "blue whiting", type_var),
                              type_var = gsub("Saithe.*", "saithe", type_var),
                              type_var = ifelse(grepl("Norway", type_var),
                                                type_var,
                                                tolower(type_var)))
                              }

        plot <- dplyr::group_by(df, type_var)
        plot <- dplyr::summarise(plot,typeTotal = sum(VALUE, na.rm = TRUE))
        plot <- dplyr::arrange(plot, -typeTotal)
        plot <- dplyr::filter(plot, typeTotal >= 1)
        plot <- dplyr::mutate(plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal)))
        plot <- dplyr::inner_join(plot,df, by = "type_var")

        plot$RANK<-as.numeric(plot$RANK)

        plot <- dplyr::mutate(plot, type_var = replace(type_var, RANK > line_count, "other"))
        plot <- dplyr::group_by(plot,type_var, YEAR)
        plot <- dplyr::summarise(plot, typeTotal= sum(VALUE, na.rm = TRUE) / 1000)
        plot <- dplyr::filter(plot,!is.na(YEAR))

        plot <- rbind(plot[!plot$type_var == "other",],
                           plot[plot$type_var == "other",])

        colList <- ggthemes::tableau_color_pal('Tableau 20')(line_count + 1)

        order <- dplyr::group_by(plot,type_var)
        order <- dplyr::summarise(order,total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order,-total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))

        plot$type_var <- factor(plot$type_var,
                                     levels = order$type_var[order(order$total)])

        myColors <- colList[1:length(unique(plot$type_var))]
        names(myColors) <- levels(plot$type_var)
        myColors["other"] <- "#7F7F7F"

        pl <- ggplot2::ggplot(dplyr::ungroup(plot), ggplot2::aes(x = YEAR, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(plot$YEAR),
                                                max(plot$YEAR), by = 10)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = max(plot$YEAR), y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(plot$YEAR), max(plot$YEAR) + 20)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 15) +
                ggplot2::theme(legend.position = "right",
                      plot.caption = ggplot2::element_text(size = 10),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank())

        if(plot_type == "area") {
                cumPlot <- dplyr::filter(plot,YEAR == max(YEAR, na.rm = TRUE))
                cumPlot <- dplyr::ungroup(cumPlot)
                cumPlot <- dplyr::arrange(cumPlot, dplyr::desc(type_var))
                cumPlot <- dplyr::mutate(cumPlot, cs = cumsum(as.numeric(typeTotal)), # cumulative sum
                               mp = lag(cs, order_by = dplyr::desc(type_var)), # midpoint
                               mp = ifelse(is.na(mp), 1, mp)) # midpoint
                cumPlot <- dplyr::ungroup(cumPlot)
                cumPlot <- dplyr::arrange(cumPlot, dplyr::desc(type_var))
                cumPlot <- dplyr::mutate(cumPlot, td = rowMeans(cumPlot[4:5]))

                pl <- pl + ggplot2::geom_area(ggplot2::aes(fill = type_var, color = type_var),
                                     alpha = .8,
                                     position = "stack")

        }

        if(plot_type == "line") {
                pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                     alpha = .8, position = "identity")

        }

        if (return_data == TRUE){
                plot
        } else {
                pl
        }
}

plot_catch_trends_app_new <- function(x, type = c("COMMON_NAME", "COUNTRY", "GUILD"),
                                      line_count = 10,
                                      plot_type = c("line", "area"),
                                      official_catches_year = cap_year - 1,
                                      return_data = FALSE) {
    capyear <- official_catches_year - 1
    cap_text <- sprintf("Historical Nominal Catches 1950-2010,\nOfficial Nominal Catches 2006-%s\nPreliminary Catches %s\nICES, Copenhagen.", capyear, official_catches_year)

    df <- dplyr::rename(x, type_var = dplyr::all_of(type))
    
    if (type == "COMMON_NAME") {
        df$type_var <- gsub("European ", "", df$type_var)
        df$type_var <- gsub("Sandeels.*", "sandeel", df$type_var)
        df$type_var <- gsub("Finfishes nei", "undefined finfish", df$type_var)
        df$type_var <- gsub("Blue whiting.*", "blue whiting", df$type_var)
        df$type_var <- gsub("Saithe.*", "saithe", df$type_var)
        df$type_var <- ifelse(grepl("Norway", df$type_var), df$type_var, tolower(df$type_var))
    }
    
    plot <- df %>%
        dplyr::group_by(type_var) %>%
        dplyr::summarise(typeTotal = sum(VALUE, na.rm = TRUE)) %>%
        dplyr::arrange(dplyr::desc(typeTotal)) %>%
        dplyr::filter(typeTotal >= 1) %>%
        dplyr::mutate(RANK = dplyr::min_rank(dplyr::desc(typeTotal))) %>%
        dplyr::inner_join(df, by = "type_var") %>%
        dplyr::mutate(type_var = ifelse(RANK > line_count, "other", type_var)) %>%
        dplyr::group_by(type_var, YEAR) %>%
        dplyr::summarise(typeTotal = sum(VALUE, na.rm = TRUE) / 1000) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(YEAR))
    
    unique_types <- unique(plot$type_var)
#     print(length(unique_types))  # Debugging step
#     print(unique_types)  # Debugging step
#     colors <- setNames(RColorBrewer::brewer.pal(min(length(unique_types), 11), "Set1"), unique_types)
#     colors["other"] <- "#7F7F7F"
#     print(length(colors))  # Debugging step
#     print(colors)  # Debugging step
# Create a highlight key
    plot <- plotly::highlight_key(plot, key = ~type_var)
    
    p <- plotly::plot_ly(plot, x = ~YEAR, y = ~typeTotal, color = ~type_var)
    
    if (plot_type == "area") {
        # p <- p %>% plotly::add_trace(fill = "tozeroy", mode = "none")
        p <- p %>% plotly::add_trace(type = 'scatter', mode = 'none', stackgroup = 'one')
    } else {
        p <- p %>% plotly::add_trace(type = "scatter", mode = "lines", line = list(width = 3))
    }
    
    p <- p %>% plotly::layout(
        title = "Catch Trends",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Landings (thousand tonnes)"),
        margin = list(b = 100),
        annotations = list(
            list(
                x = 1, y = -0.28, text = cap_text,
                showarrow = FALSE, xref = "paper", yref = "paper",
                xanchor = "right", yanchor = "bottom"
            )
        )
    )
    p <- p %>% plotly::highlight(
                                on = 'plotly_hover',
                                off = 'plotly_doubleclick',
                                selected = plotly::attrs_selected(
                                        opacity = 0.7,
                                        showlegend = TRUE,
                                        line = list(width = 5) 
                                )
                        )
    
    if (return_data) {
        return(plot)
    } else {
        return(p)
    }
}

plot_discard_trends_app <- function(x, year, caption = FALSE, cap_year, cap_month, return_data = FALSE){
        df <- dplyr::filter(x,Year %in% seq(year-5, year -1))
        df2 <- tidyr::expand(df,Year, tidyr::nesting(StockKeyLabel,FisheriesGuild))
        df <- dplyr::left_join(df,df2,
                          by = c("Year", "StockKeyLabel", "FisheriesGuild"))
        df3 <- dplyr::select(df, StockKeyLabel, Year, Discards)
        df3 <- unique(df3)
        df3 <- tibble::rowid_to_column(df3)
        df3 <- tidyr::spread(df3,Year, Discards)
        df3 <- tidyr::gather(df3,Year, Discards, 4:ncol(df3))
        df3 <- dplyr::mutate(df3,Year = as.numeric(Year),
                       Discards = as.numeric(Discards))

        df4<- dplyr::select(df,StockKeyLabel, Year, Landings)
        df4 <- unique(df4)
        df4 <- tibble::rowid_to_column(df4)
        df4 <- dplyr::group_by(df4,StockKeyLabel)
        df4 <- tidyr::spread(df4,Year, Landings)
        df4 <- tidyr::gather(df4,Year, Landings, 4:ncol(df4))
        df4 <- dplyr::mutate(df4,Year = as.numeric(Year),
                       Landings = as.numeric(Landings))
        df5 <- dplyr::select(df,-Discards,
                       -Landings)
        df5 <- dplyr::left_join(df5,df3, by = c("Year", "StockKeyLabel"))
        df5 <- dplyr::left_join(df5,df4, by = c("Year", "StockKeyLabel"))
        df5 <- dplyr::group_by(df5,Year, FisheriesGuild)
        df5 <- dplyr::summarize(df5, guildLandings = sum(Landings, na.rm = TRUE)/ 1000,
                          guildDiscards = sum(Discards, na.rm = TRUE)/ 1000)

        df5 <- dplyr::mutate(df5,guildRate = guildDiscards/ (guildLandings + guildDiscards))
        df5 <- tidyr::gather(df5,variable, value, -Year, -FisheriesGuild)
        df5 <- dplyr::filter(df5,!variable %in% c("guildDiscards", "guildLandings"))
        df5 <- dplyr::filter(df5,!is.na(value))
        df6 <- dplyr::filter(df5, Year == year - 1)
        plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                               ggplot2::aes(x = Year,
                                   y = value,
                                   color = FisheriesGuild)) +
                ggplot2::geom_line() +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_x_continuous(breaks = seq(min(df5$Year, na.rm = TRUE),
                                                max(df5$Year, na.rm = TRUE), by = 1)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = max(df5$Year, na.rm = TRUE),
                                 y = -Inf, yend = -Inf), color = "grey50") +
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf,
                                 x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(df5$Year, na.rm = TRUE), year + 1)) +
                ggplot2::scale_color_brewer(type = "qual", palette = "Set2") +
                ggplot2::scale_fill_brewer(type = "qual", palette = "Set2") +
                ggplot2::theme_bw(base_size = 15) +
                ggplot2::theme(legend.position = "right",
                      plot.caption = ggplot2::element_text(size = 10),
                      panel.grid = ggplot2::element_blank(),
                      legend.key = ggplot2::element_rect(colour = NA)) +
                ggplot2::labs(x = "Year", y = "Discard rate", caption = "")


        if(caption == TRUE){
                cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                                           cap_month,
                                                           cap_year))
                plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                                        ggplot2::aes(x = Year,
                                                     y = value,
                                                     color = FisheriesGuild)) +
                        ggplot2::geom_line() +
                        ggplot2::scale_y_continuous(labels = scales::percent) +
                        ggplot2::scale_x_continuous(breaks = seq(min(df5$Year, na.rm = TRUE),
                                                                 max(df5$Year, na.rm = TRUE), by = 1)) +
                        ggplot2::geom_segment(ggplot2::aes(x = min(df5$Year), xend = max(df5$Year, na.rm = TRUE),
                                                           y = -Inf, yend = -Inf), color = "grey50") +
                        ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf,
                                                           x = min(df5$Year), xend = -Inf), color = "grey50")+
                        ggplot2::expand_limits(x = c(min(df5$Year, na.rm = TRUE), year + 1)) +
                        ggplot2::scale_color_brewer(type = "qual", palette = "Set2") +
                        ggplot2::scale_fill_brewer(type = "qual", palette = "Set2") +
                        ggplot2::theme_bw(base_size = 15) +
                        ggplot2::theme(legend.position = "right",
                                       plot.caption = ggplot2::element_text(size = 10),
                                       panel.grid = ggplot2::element_blank(),
                                       legend.key = ggplot2::element_rect(colour = NA)) +
                        ggplot2::labs(x = "Year", y = "Discard rate", caption = "")+
                        cap_lab

        }

        if(return_data == TRUE){
                df5
        }else{
                plot
        }
}



plot_discard_current_app <- function(x, year, position_letter = "",
                                 caption = TRUE, cap_year, cap_month,
                                 return_data = FALSE){
  df <- dplyr::filter(x,Year %in% seq(year-5, year -1))
  df2 <- tidyr::expand(df,Year, tidyr::nesting(StockKeyLabel,FisheriesGuild))
  df <- dplyr::left_join(df,df2,
                         by = c("Year", "StockKeyLabel", "FisheriesGuild"))
  df <- df[, -11]
  df3 <- dplyr::select(df, StockKeyLabel, Year, Discards)
  df3 <- unique(df3)
  df3 <- tibble::rowid_to_column(df3)
  df3 <- dplyr::group_by(df3,StockKeyLabel)
  df3 <- tidyr::spread(df3,Year, Discards)
  df3 <- tidyr::gather(df3,Year, Discards, 3:ncol(df3))
  df3 <- dplyr::mutate(df3,Year = as.numeric(Year),
                       Discards = as.numeric(Discards))
  df5 <- dplyr::select(df,-Discards)
  df5 <- dplyr::left_join(df5,df3, by = c("Year", "StockKeyLabel"))
  df5 <- dplyr::group_by(df5,Year, FisheriesGuild)
  df5$Landings <- ifelse(!is.na(df5$Landings), df5$Landings, df5$Catches)

  df5 <- dplyr::summarize(df5,guildLandings = sum(Landings, na.rm = TRUE),
                          guildDiscards = sum(Discards, na.rm = TRUE))
  
  df5 <- tidyr::gather(df5,variable, value, -Year, -FisheriesGuild)
  df5 <- dplyr::filter(df5, FisheriesGuild %in% c("demersal", "pelagic", "benthic"))
  df5 <- dplyr::filter(df5,Year == year-1)
  df5$value <- df5$value/1000

  plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                          ggplot2::aes(x = reorder(FisheriesGuild, value, sum), y = value, fill = variable)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
          ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
          ggplot2::coord_flip() +
          ggplot2::theme_bw(base_size = 15) +
          ggplot2::theme(legend.position = "none",
                plot.caption = ggplot2::element_text(size = 10),
                panel.grid = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(colour = NA)) +
          ggplot2::labs(x = "", y = "Discards and Landings (thousand tonnes)",title = position_letter)

  if(caption == TRUE) {
    cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                               cap_month,
                                               cap_year))
    plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                            ggplot2::aes(x = reorder(FisheriesGuild, value, sum), y = value, fill = variable)) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
            ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw(base_size = 15) +
            ggplot2::theme(legend.position = "none",
                           plot.caption = ggplot2::element_text(size = 10),
                           panel.grid = ggplot2::element_blank(),
                           legend.key = ggplot2::element_rect(colour = NA)) +
            ggplot2::labs(x = "", y = "Discards and Landings (thousand tonnes)")+
            cap_lab
  }

  if(return_data == T){
          df5
  }else{
          plot
  }

}
