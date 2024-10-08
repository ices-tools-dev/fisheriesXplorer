#' stock_status 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
prepare_ices_stock_status <- function (df, cap_month = "November", cap_year = "2018") {
  
  df
  df_stock <- dplyr::select(df, StockKeyLabel, FisheriesGuild, 
                            lineDescription, FishingPressure, StockSize, SBL)
  df_stock <- tidyr::gather(df_stock, Variable, Colour, FishingPressure:SBL, 
                            factor_key = TRUE)
  df2 <- dplyr::group_by(df_stock, FisheriesGuild, lineDescription, 
                         Variable, Colour)
  df2 <- dplyr::summarize(df2, COUNT = dplyr::n())
  df2 <- tidyr::spread(df2, Colour, COUNT)
  df2[is.na(df2)] <- 0
  df3 <- subset(df2, select = -c(FisheriesGuild))
  df3 <- dplyr::group_by(df3, lineDescription, Variable)
  df3 <- dplyr::summarise_each(df3, dplyr::funs(sum))
  df3$FisheriesGuild <- "total"
  df2 <- rbind(df2, df3)
  df4 <- dplyr::filter(df2, Variable == "SBL")
  df4$lineDescription <- ""
  df4 <- unique(df4)
  df2 <- dplyr::filter(df2, Variable != "SBL")
  df2 <- rbind(df2, df4)
  df2$lineDescription <- gsub("Maximum sustainable yield", 
                              "MSY", df2$lineDescription)
  df2$lineDescription <- gsub("Precautionary approach", "PA", 
                              df2$lineDescription)
  df2$header <- paste0(df2$Variable, "\n", df2$lineDescription)
  df2 <- tidyr::gather(df2, colour, value, GREEN:RED, factor_key = TRUE)
  df2 <- dplyr::filter(df2, value > 0)
  tot <- dplyr::filter(df2, FisheriesGuild == "total")
  tot <- dplyr::group_by(tot, header)
  tot <- dplyr::mutate(tot, tot = sum(value))
  max <- unique(tot$tot)
  df2 <- dplyr::group_by(df2, FisheriesGuild, header)
  df2 <- dplyr::mutate(df2, sum = sum(value))
  df2$fraction <- df2$value * max/df2$sum
  df2$header <- factor(df2$header, levels = c("FishingPressure\nMSY", 
                                              "StockSize\nMSY", "FishingPressure\nPA", "StockSize\nPA", 
                                              "SBL\n"))
  df2$FisheriesGuild <- factor(df2$FisheriesGuild, levels = c("total", 
                                                              "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))

  df2
}


plot_status_prop_pies_app <- function(df, cap_month, cap_year){
  
  cap_lab <- ggplot2::labs(title = "", x = "", y = "", 
                           caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen", 
                                             cap_month, cap_year))
  
  colList <- c(GREEN = "#00B26D", GREY = "#d3d3d3", ORANGE = "#ff7f00", 
               RED = "#d93b1c", qual_RED = "#d93b1c", qual_GREEN = "#00B26D")
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = "", y = fraction, fill = colour)) + 
    ggplot2::geom_bar(stat = "identity", width = 1) + 
    ggplot2::geom_text(ggplot2::aes(label = value), position = ggplot2::position_stack(vjust = 0.5), size = 3) + 
    ggplot2::scale_fill_manual(values = colList) + 
    ggplot2::theme_bw(base_size = 13) + 
    ggplot2::theme(panel.grid = ggplot2::element_blank(), 
                   panel.border = ggplot2::element_blank(), 
                   panel.background = ggplot2::element_blank(), 
                   legend.position = "none") + 
    ggplot2::theme(axis.text = ggplot2::element_blank(), 
                   axis.ticks = ggplot2::element_blank(), 
                   strip.background = ggplot2::element_blank()) + 
    cap_lab + 
    ggplot2::coord_polar(theta = "y", direction = 1) + 
    ggplot2::facet_grid(FisheriesGuild ~ header)
}


prepare_ges_stock_status <- function (status_df, catch_df){

  df_stock <- dplyr::filter(status_df, lineDescription == "Maximum sustainable yield")
  df_stock <- dplyr::select(df_stock, StockKeyLabel, FishingPressure, 
                            StockSize)
  df_stock <- tidyr::gather(df_stock, Variable, Colour, FishingPressure:StockSize, 
                            factor_key = TRUE)
  df2 <- dplyr::group_by(df_stock, Variable, Colour) %>% dplyr::summarize(COUNT = dplyr::n()) %>% 
    tidyr::spread(Colour, COUNT)
  df2[is.na(df2)] <- 0
  df3 <- dplyr::filter(catch_df, StockKeyLabel %in% df_stock$StockKeyLabel)
  df3 <- dplyr::mutate(df3, CATCH = ifelse(is.na(Catches) & 
                                             !is.na(Landings), Landings, Catches))
  df3 <- dplyr::select(df3, c(StockKeyLabel, CATCH))
  df4 <- dplyr::left_join(df_stock, df3)
  df4[is.na(df4)] <- 0
  df4 <- dplyr::group_by(df4, Variable, Colour) %>% dplyr::summarize(CATCH = sum(CATCH)) %>% 
    tidyr::spread(Colour, CATCH)
  df4 <- tidyr::gather(df4, Color, Catch, GREEN:RED, factor_key = TRUE)
  df2 <- tidyr::gather(df2, Color, Stocks, GREEN:RED, factor_key = TRUE)
  df5 <- merge(df2, df4)
  df5[is.na(df5)] <- 0
  tot <- sum(df5$Catch)/2
  stocks <- sum(df5$Stocks)/2
  df5 <- tidyr::gather(df5, Metric, Value, Stocks:Catch)
  df5 <- dplyr::group_by(df5, Metric)
  df5 <- dplyr::mutate(df5, sum = sum(Value)/2)
  df5$fraction <- ifelse(df5$Metric == "Stocks", (df5$Value * 
                                                    tot)/stocks, df5$Value)
  df5$Variable <- plyr::revalue(df5$Variable, c(FishingPressure = "D3C1", 
                                                StockSize = "D3C2"))
  df5$Metric <- plyr::revalue(df5$Metric, c(Stocks = "Number of stocks", 
                                            Catch = "Proportion of catch \n(thousand tonnes)"))
  df5$Value2 <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)", 
                       df5$Value/1000, df5$Value)
  df5$sum2 <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)", 
                     df5$sum/1000, df5$sum)
  df5$Value <- as.integer(df5$Value)
  df5$Value2 <- as.integer(df5$Value2)
  df5$sum2 <- as.integer(df5$sum2)
  # df5 <- subset(df5, select = -c(Value2, sum2))
  df5
  
}
  
plot_GES_pies_app <- function(df, cap_month = "August", cap_year = "2019"){
  

  cap_lab <- ggplot2::labs(title = "", x = "", y = "", 
                           caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen", 
                                             cap_month, cap_year))
  
  colList <- c(GREEN = "#00B26D", GREY = "#d3d3d3", ORANGE = "#ff7f00", 
               RED = "#d93b1c", qual_RED = "#d93b5c", qual_GREEN = "#00B28F")
  
  p1 <- ggplot2::ggplot(data = df, ggplot2::aes(x = "", y = fraction, fill = Color)) + 
    ggplot2::geom_bar(stat = "identity", width = 1) + 
    ggplot2::geom_text(ggplot2::aes(label = Value2), position = ggplot2::position_stack(vjust = 0.5), size = 4) + 
    ggplot2::geom_text(ggplot2::aes(label = paste0("total = ", sum2), x = 0, y = 0), size = 4) + 
    ggplot2::scale_fill_manual(values = colList) + 
    ggplot2::theme_bw(base_size = 13) + 
    ggplot2::theme(panel.grid = ggplot2::element_blank(), 
                   panel.border = ggplot2::element_blank(), 
                   panel.background = ggplot2::element_blank(), 
                   legend.position = "none",
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(), 
                   strip.background = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(size = 9)) + 
    cap_lab + 
    ggplot2::coord_polar(theta = "y") + 
    ggplot2::facet_grid(Metric ~ Variable)
  p1
}




plot_kobe_app <- function(x, guild, caption = FALSE, cap_year, cap_month, return_data = FALSE){
        if(guild == "All"){
                df <-x
        }else(df <- dplyr::filter(x,FisheriesGuild %in% guild))
        xmax = max(df$F_FMSY, na.rm = TRUE)
        ifelse(xmax < 3, xmax2 <- 3, xmax2 <- (xmax + 0.5))
        ymax = max(df$SSB_MSYBtrigger, na.rm = TRUE)
        ifelse(ymax < 3, ymax2 <- 3, ymax2 <- (ymax + 0.5))
        kobe <- ggplot2::ggplot(df, ggplot2::aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                         data_id = StockKeyLabel)) +
                ggplot2::coord_cartesian(xlim = c(0, xmax2), ylim = c(0, ymax2))+
                ggplot2::geom_point(ggplot2::aes(color = Status), size = 15,
                           alpha = 0.7, na.rm = TRUE) +
                ggplot2::geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
                ggplot2::geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
                ggrepel::geom_text_repel(ggplot2::aes(label = StockKeyLabel),
                                         segment.size = .25,
                                         force = 5,
                                         size = 5) +
                ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                              "RED" = "#e41a1c",
                                              "GREY" = "#d3d3d3")) +
                ggplot2::labs(x = expression(F/F[MSY]),
                     y = expression(SSB/MSY~B[trigger]),
                     caption = "") +
                ggplot2::theme_bw(base_size = 20) +
                ggplot2::theme(legend.position = 'none',
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 10))
      
        
        if(return_data == T){
                df
        }else{
                kobe
        }
}


plot_CLD_bar_app <- function(x, guild, caption = TRUE, cap_year, cap_month, return_data = FALSE){
        if(guild == "All"){
                df <-x
        }else(df <- dplyr::filter(x,FisheriesGuild %in% guild))
      
        df <- df %>% dplyr::group_by(StockKeyLabel)
        df <- dplyr::mutate(df,total = ifelse(all(is.na(Catches) & is.na(Landings)),
                                      NA,
                                      max(Catches, Landings, na.rm = TRUE))) 
        df <- dplyr::ungroup (df) %>% 
          dplyr::filter(!is.na(total))
        
        df <- dplyr::mutate(df,StockKeyLabel = forcats::fct_reorder(StockKeyLabel, total))
        
        plot <- ggplot2::ggplot(df, ggplot2::aes(x =StockKeyLabel, y = Catches/1000)) +
               ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Catches/1000,
                                 xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
               ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Landings/1000,
                                 xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
               ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Catches/1000,
                                                  fill = Status), color = "grey50",
                           shape = 24, size = 7, alpha = 0.8, na.rm = TRUE) +
               ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Landings/1000,
                                                  fill = Status), color = "grey50",
                           shape = 21, size = 7, alpha = 0.8, na.rm = TRUE) +
               ggplot2::scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                             "RED" = "#e41a1c",
                                             "GREY" = "#d3d3d3")) +
               ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                              "RED" = "#e41a1c",
                                              "GREY" = "#d3d3d3")) +
               ggplot2::coord_equal() +
               ggplot2::coord_flip() +
               ggplot2::theme_bw(base_size = 20) + 
               ggplot2::labs(y = expression("Catch and Landings (thousand tonnes)"))+
               ggplot2::theme(legend.position = 'none',
                      plot.caption = ggplot2::element_text(size = 10),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.grid.major.y = ggplot2::element_blank(),
                      panel.grid.major.x = ggplot2::element_line( size = 0.1, color = "grey80"))
        
        
        if(caption == T){
                cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                                           cap_month,
                                                           cap_year))
                plot <- ggplot2::ggplot(df, ggplot2::aes(x =StockKeyLabel, y = Catches/1000)) +
                        ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Catches/1000,
                                                           xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
                        ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Landings/1000,
                                                           xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
                        ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Catches/1000,
                                                                            fill = Status), color = "grey50",
                                            shape = 24, size = 7, alpha = 0.8, na.rm = TRUE) +
                        ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Landings/1000,
                                                                            fill = Status), color = "grey50",
                                            shape = 21, size = 7, alpha = 0.8, na.rm = TRUE) +
                        ggplot2::scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                                              "RED" = "#e41a1c",
                                                              "GREY" = "#d3d3d3")) +
                        ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                                               "RED" = "#e41a1c",
                                                               "GREY" = "#d3d3d3")) +
                        ggplot2::coord_equal() +
                        ggplot2::coord_flip() +
                        ggplot2::theme_bw(base_size = 20) + 
                        ggplot2::labs(y = expression("Catch and Landings (thousand tonnes)"))+
                        ggplot2::theme(legend.position = 'none',
                                       plot.caption = ggplot2::element_text(size = 10),
                                       panel.grid.minor = ggplot2::element_blank(),
                                       panel.grid.major.y = ggplot2::element_blank(),
                                       panel.grid.major.x = ggplot2::element_line( size = 0.1, color = "grey80"))+
                        cap_lab
        }
        
        
        if(return_data == T){
                df
        }else{
                plot
        }
}

