#' stock_status 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# prepare_ices_stock_status <- function (df, cap_month = "November", cap_year = "2018") {
  
#   df
#   df_stock <- dplyr::select(df, StockKeyLabel, FisheriesGuild, 
#                             lineDescription, FishingPressure, StockSize, SBL)
#   df_stock <- tidyr::gather(df_stock, Variable, Colour, FishingPressure:SBL, 
#                             factor_key = TRUE)
#   df2 <- dplyr::group_by(df_stock, FisheriesGuild, lineDescription, 
#                          Variable, Colour)
#   df2 <- dplyr::summarize(df2, COUNT = dplyr::n())
#   df2 <- tidyr::spread(df2, Colour, COUNT)
#   df2[is.na(df2)] <- 0
#   df3 <- subset(df2, select = -c(FisheriesGuild))
#   df3 <- dplyr::group_by(df3, lineDescription, Variable)
#   df3 <- dplyr::summarise_each(df3, dplyr::funs(sum))
#   df3$FisheriesGuild <- "total"
#   df2 <- rbind(df2, df3)
#   df4 <- dplyr::filter(df2, Variable == "SBL")
#   df4$lineDescription <- ""
#   df4 <- unique(df4)
#   df2 <- dplyr::filter(df2, Variable != "SBL")
#   df2 <- rbind(df2, df4)
#   df2$lineDescription <- gsub("Maximum sustainable yield", 
#                               "MSY", df2$lineDescription)
#   df2$lineDescription <- gsub("Precautionary approach", "PA", 
#                               df2$lineDescription)
#   df2$header <- paste0(df2$Variable, "\n", df2$lineDescription)
#   df2 <- tidyr::gather(df2, colour, value, GREEN:RED, factor_key = TRUE)
#   df2 <- dplyr::filter(df2, value > 0)
#   tot <- dplyr::filter(df2, FisheriesGuild == "total")
#   tot <- dplyr::group_by(tot, header)
#   tot <- dplyr::mutate(tot, tot = sum(value))
#   max <- unique(tot$tot)
#   df2 <- dplyr::group_by(df2, FisheriesGuild, header)
#   df2 <- dplyr::mutate(df2, sum = sum(value))
#   df2$fraction <- df2$value * max/df2$sum
#   df2$header <- factor(df2$header, levels = c("FishingPressure\nMSY", 
#                                               "StockSize\nMSY", "FishingPressure\nPA", "StockSize\nPA", 
#                                               "SBL\n"))
#   df2$FisheriesGuild <- factor(df2$FisheriesGuild, levels = c("total", 
#                                                               "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))

#   df2
# }


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

plot_CLD_bar <- function(x, guild, caption = TRUE, cap_year, cap_month, return_data = FALSE){
        if(guild == "All"){
                df <-x
        }else(df <- dplyr::filter(x,FisheriesGuild %in% guild))
        df <- dplyr::mutate(df,total = ifelse(all(is.na(Catches) & is.na(Landings)),
                                      NA,
                                      max(Catches, Landings, na.rm = TRUE))) 
        df <- dplyr::ungroup (df)
        df <- df[order(-df$total), ] 
        df$StockKeyLabel <- factor(df$StockKeyLabel, levels = rev(df$StockKeyLabel))
        
        plot <- ggplot2::ggplot(df, ggplot2::aes(x =StockKeyLabel, y = Catches/1000)) +
               ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Catches/1000,
                                 xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
               ggplot2::geom_segment(ggplot2::aes(x = StockKeyLabel, y = Landings/1000,
                                 xend = StockKeyLabel, yend = 0, color = Status), size = 2, na.rm = TRUE) +
               ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Catches/1000,
                                                  fill = Status), color = "grey50",
                           shape = 24, size = 2, alpha = 0.8, na.rm = TRUE) +
               ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Landings/1000,
                                                  fill = Status), color = "grey50",
                           shape = 21, size = 2, alpha = 0.8, na.rm = TRUE) +
               ggplot2::scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                             "RED" = "#e41a1c",
                                             "GREY" = "#d3d3d3")) +
               ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                              "RED" = "#e41a1c",
                                              "GREY" = "#d3d3d3")) +
               ggplot2::coord_equal() +
               ggplot2::coord_flip() +
               ggplot2::theme_bw(base_size = 7) + 
               ggplot2::labs(y = expression("Catch and Landings(thousand tonnes)"))+
               ggplot2::theme(legend.position = 'none',
                      plot.caption = ggplot2::element_text(size = 6),
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
                                            shape = 24, size = 2, alpha = 0.8, na.rm = TRUE) +
                        ggplot2::geom_point(stat = "identity", ggplot2::aes(y = Landings/1000,
                                                                            fill = Status), color = "grey50",
                                            shape = 21, size = 2, alpha = 0.8, na.rm = TRUE) +
                        ggplot2::scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                                              "RED" = "#e41a1c",
                                                              "GREY" = "#d3d3d3")) +
                        ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
                                                               "RED" = "#e41a1c",
                                                               "GREY" = "#d3d3d3")) +
                        ggplot2::coord_equal() +
                        ggplot2::coord_flip() +
                        ggplot2::theme_bw(base_size = 7) + 
                        ggplot2::labs(y = expression("Catch and Landings(thousand tonnes)"))+
                        ggplot2::theme(legend.position = 'none',
                                       plot.caption = ggplot2::element_text(size = 6),
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


getSID <- function(year, EcoR) {
        message("Downloading SID data for year: ", year)
        stock_list_long <- jsonlite::fromJSON(
                URLencode(
                        sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel,
                        EcoRegion,
                        YearOfLastAssessment,
                        AssessmentKey,
                        StockKeyDescription,
                        SpeciesScientificName,
                        SpeciesCommonName,
                        AdviceCategory,
                        DataCategory,
                        YearOfLastAssessment,
                        FisheriesGuild", year)
                )
        )$value

        stock_list_long <- stock_list_long %>%
                mutate(EcoRegion = as.character(EcoRegion)) %>%
                tidyr::separate_rows(EcoRegion, sep = ", ")

        stock_list_long <- stock_list_long %>%
                filter(EcoRegion == EcoR)


        stock_list_long <- stock_list_long[!is.na(stock_list_long$AssessmentKey), ]
        # message("SID Data processing complete.")
        return(stock_list_long)
}




# getStatus <- function(stock_list_long) {
        
        
#         unique_keys <- unique(stock_list_long$AssessmentKey)

#         #Fetch stock status values in parallel
#         status_list <- future.apply::future_lapply(unique_keys, function(key) {
#                 tryCatch(
#                         icesSAG::getStockStatusValues(key),
#                         error = function(e) {
#                                 message(sprintf("Error fetching data for AssessmentKey: %s", key))
#                                 return(NULL)
#                         }
#                 )
#         })
        
#         # status <- do.call(rbind, status)
#         # Combine results into a single dataframe
#         status <- plyr::rbind.fill(lapply(status_list, as.data.frame))
#         # status <- do.call(rbind.data.frame, status_list)

#         # Merge stock status values with SID data
#         df_status <- merge(stock_list_long, status, by = "AssessmentKey", all.x = TRUE)
#         df_status$FisheriesGuild <- tolower(df_status$FisheriesGuild)
        
#         return(df_status)
# }

getStatusWebService <- function(Ecoregion, sid) {
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        status <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/test_api/LatestStocks/Status?ecoregion=%s", EcoregionCode)
                )
        )
        status_long <- status %>%
                tidyr::unnest(YearStatus)

        df_status <- merge(sid, status_long, by = "AssessmentKey", all.x = TRUE)
        df_status$FisheriesGuild <- tolower(df_status$FisheriesGuild)

        return(df_status)
}


format_sag_status_new <- function(df) {
     
        df <- dplyr::mutate(df,status = dplyr::case_when(status == 0 ~ "UNDEFINED",
                                                  status == 1 ~ "GREEN",
                                                  status == 2 ~ "GREEN", #qualitative green
                                                  status == 3 ~ "ORANGE",
                                                  status == 4 ~ "RED",
                                                  status == 5 ~ "RED", #qualitative red
                                                  status == 6 ~ "GREY",
                                                  status == 7 ~ "qual_UP",
                                                  status == 8 ~ "qual_STEADY",
                                                  status == 9 ~ "qual_DOWN",
                                                  TRUE ~ "OTHER"),
                            fishingPressure = dplyr::case_when(fishingPressure == "-" &
                                                                type == "Fishing pressure" ~ "FQual",
                                                        TRUE ~ fishingPressure),
                            stockSize = dplyr::case_when(stockSize == "-" &
                                                          type == "Stock Size" ~ "SSBQual",
                                                  TRUE ~ stockSize),
                            stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
                            variable = dplyr::case_when(type == "Fishing pressure" ~ fishingPressure,
                                                 type == "Stock Size" ~ stockSize,
                                                 TRUE ~ type),
                            variable = dplyr::case_when(lineDescription == "Management plan" &
                                                         type == "Fishing pressure" ~ "FMGT",
                                                 lineDescription == "Management plan" &
                                                         type == "Stock Size" ~ "SSBMGT",
                                                 TRUE ~ variable),
                            variable = dplyr::case_when(
                                    grepl("Fpa", variable) ~ "FPA",
                                    grepl("Bpa", variable) ~ "BPA",
                                    grepl("^Qual*", variable) ~ "SSBQual",
                                    grepl("-", variable) ~ "FQual",
                                    grepl("^BMGT", variable) ~ "SSBMGT",
                                    grepl("MSYBtrigger", variable) ~ "BMSY",
                                    grepl("FMSY", variable) ~ "FMSY",
                                    TRUE ~ variable
                            )) 
        
        df <- dplyr::filter(df,variable != "-")
        
        df <- dplyr::filter(df, lineDescription != "Management plan")
        df <- dplyr::filter(df, lineDescription != "Qualitative evaluation")
        df <- dplyr::mutate(df,key = paste(StockKeyLabel, lineDescription, type))
        # df <- dplyr::mutate(df,key = paste( lineDescription, type)) #stockComponent,
        df<- df[order(-df$year),]
        df <- df[!duplicated(df$key), ]
        df<- subset(df, select = -key)
        df<- subset(df, select = c(StockKeyLabel, AssessmentKey,lineDescription, type, status, FisheriesGuild)) #, stockComponent,adviceValue
        df<- tidyr::spread(df,type, status)
        
        df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
        df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
        df <- df %>% dplyr::rename(FishingPressure = `Fishing pressure`,
                            StockSize = `Stock Size`)
      
        df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
        df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
        return(df)
}


plot_status_prop_pies <- function(df, cap_month = "November",
                         cap_year = "2018",
                         return_data = FALSE) {


        # df <- dplyr::filter(df, !is.na(FisheriesGuild))
        
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = "",
                        caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen",
                                          cap_month,
                                          cap_year))
        colList <- c("GREEN" = "#00B26D",
                     "GREY" = "#d3d3d3",
                     "ORANGE" = "#ff7f00",
                     "RED" = "#d93b1c",
                     "qual_RED" = "#d93b1c",
                     "qual_GREEN" = "#00B26D",
                     "UNDEFINED" = "#006aff")


        df_stock <- dplyr::select(df,StockKeyLabel,
                       FisheriesGuild,
                       lineDescription,
                       FishingPressure,
                       StockSize)#,
                #        SBL)
        df_stock <- tidyr::gather(df_stock,Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)
        df2 <- dplyr::group_by(df_stock, FisheriesGuild, lineDescription, Variable, Colour)
        df2 <- dplyr::summarize(df2, COUNT = dplyr::n())
        df2 <- tidyr::spread(df2, Colour, COUNT)
        
        # replace NAs with 0 in int columns
        # browser()
        # is.na(df2$FisheriesGuild)
        # df[is.na(df$FisheriesGuild),]
        # df2 <- dplyr::filter(df2, !is.na(FisheriesGuild))
        # str(df2)
        # df2 <- df2 %>% na.omit(FisheriesGuild)
        df2[is.na(df2)] <- 0
        df3 <- subset(df2,select =-c(FisheriesGuild))
        df3 <- dplyr::group_by(df3,lineDescription, Variable)
        df3 <- dplyr::summarise_each(df3,dplyr::funs(sum))
        df3$FisheriesGuild <- "total"
        df2 <- rbind(df2,df3)

        # df4 <- dplyr::filter(df2,Variable == "SBL")
        # df4$lineDescription <- ""
        # df4 <- unique(df4)
        # df2 <- dplyr::filter(df2,Variable != "SBL")
        # df2 <- rbind(df2,df4)
        df2$lineDescription <- gsub("Maximum sustainable yield","MSY", df2$lineDescription)
        df2$lineDescription <- gsub("Precautionary approach", "PA", df2$lineDescription)
        df2$header <- paste0(df2$Variable, "\n" , df2$lineDescription)
        # browser()
        # df2 <- tidyr::gather(df2,colour, value,GREEN:UNDEFINED, factor_key = TRUE)
        df2 <- tidyr::gather(df2,colour, value,GREEN:RED, factor_key = TRUE)
        df2 <- dplyr::filter(df2,value > 0)
        

        tot <- dplyr::filter(df2,FisheriesGuild == "total")
        tot <- dplyr::group_by(tot,header)
        tot <- dplyr::mutate(tot, tot = sum(value))
        max <- unique(tot$tot)
        df2 <- dplyr::group_by(df2, FisheriesGuild, header)
        df2 <- dplyr::mutate(df2,sum = sum(value))
         
        # testMax <- max(max) ############################## modified here because I would get 3 values
        df2$fraction <- df2$value*max/df2$sum
        df2$header <- factor(df2$header, levels = c("FishingPressure\nMSY", "StockSize\nMSY",
                                                    "FishingPressure\nPA" ,"StockSize\nPA",
                                                    "SBL\n" ))
        df2$FisheriesGuild <- tolower(df2$FisheriesGuild)
        df2$FisheriesGuild <- factor(df2$FisheriesGuild, levels= c("total", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))
        
        
        p1 <- ggplot2::ggplot(data = df2, ggplot2::aes(x = "", y = fraction, fill = colour)) +
                ggplot2::geom_bar(stat = "identity", width = 1) +
                ggplot2::geom_text(ggplot2::aes(label = value),
                          position = ggplot2::position_stack(vjust = 0.5),
                          size = 5) +
                ggplot2::scale_fill_manual(values = colList) +
                ggplot2::theme_bw(base_size = 14) +
                ggplot2::theme(panel.grid = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      legend.position="none") +
                ggplot2::theme(axis.text=ggplot2::element_blank(),
                      axis.ticks=ggplot2::element_blank(),
                      strip.background = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 8)) +
                # cap_lab +
                ggplot2::coord_polar(theta = "y", direction = 1) +
                ggplot2::facet_grid(FisheriesGuild ~ header)
                
        
        if(return_data == T){
                df2
        }else{
                p1
        }
}


format_annex_table <- function(status, year, sid, sag) {
        
        # add AssessmentComponent from SAG to SID by assessment key
        sid <- merge(sag %>% dplyr::select(StockKeyLabel, AssessmentKey, AssessmentComponent), sid, by = "StockKeyLabel")
       
        sid <- sid %>%
                dplyr::distinct() %>%
                dplyr::rename(AssessmentKey = AssessmentKey.x) %>%
                dplyr::select(-AssessmentKey.y) %>%
                dplyr::filter(StockKeyLabel %in% status$StockKeyLabel)
        
        
        df <- dplyr::left_join(status, sid, by = "StockKeyLabel")
        
        df <- dplyr::rename(df,                
                AssessmentKey = AssessmentKey.y,                
                FisheriesGuild = FisheriesGuild.x )  %>% 
                dplyr::select(-c(AssessmentKey.x, FisheriesGuild.y)) %>% 
                # if AssessmentComponent is "", make it empty NA
                dplyr::mutate(AssessmentComponent = dplyr::if_else(AssessmentComponent == "", NA_character_, AssessmentComponent))

        df <- dplyr::mutate(df,
                D3C1 = FishingPressure,
                D3C2 = StockSize,
                GES = dplyr::case_when(
                        FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                        FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                        FishingPressure == "GREY" | StockSize == "GREY" ~ "GREY",
                        TRUE ~ "GREY"
                )
        )

        df$StockKeyDescription <- gsub("\\s*\\([^\\)]+\\)", "", df$StockKeyDescription, perl = TRUE)
        
        return(df)
}


# getSAG_ecoregion <- function(year, ecoregion, sid){
#         years <- ((year-4):year)
#         ecoreg <- gsub(" ", "%20", ecoregion, fixed = TRUE)
#         # sid <- icesSD::getSD(NULL,year)
#         out <- data.frame()
#         res <- data.frame()
#         for(n in 1:5){
#                 x <- years[n]
#                 url <- paste0("https://sag.ices.dk/SAG_API/api/SAGDownload?year=", x, "&EcoRegion=", ecoreg)
#                 tmpSAG <- tempfile(fileext = ".zip")
#                 download.file(url, destfile = tmpSAG, mode = "wb", quiet = FALSE)
#                 names <-unzip(tmpSAG, list = TRUE)
#                 res <- read.csv(unz(tmpSAG, names$Name[1]),
#                                 stringsAsFactors = FALSE,
#                                 header = TRUE,
#                                 fill = TRUE)
#                 res<- unique(res)
#                 out <- rbind(out, res)
#         }
#         out <- dplyr::filter(out, Purpose == "Advice")
#         # out <- data.table::as.data.table(out) 
#         # out <- out[out[, .I[AssessmentKey == max(AssessmentKey)], by=FishStock]$V1]
        
#         # out <- out[out[, data.table::.I[AssessmentYear == max(AssessmentYear)], by=FishStock]$V1]
#         out <- as.data.frame(out)
#         out <- dplyr::filter(out,out$FishStock %in% sid$StockKeyLabel)
# }
# getSAG_ecoregion <- function(year, ecoregion, sid) {
# #   years <- ((year - 4):year)
#   years <- unique(sid$YearOfLastAssessment)
#   ecoreg <- gsub(" ", "%20", ecoregion, fixed = TRUE)
  
# #   future::plan(future::multisession)  # Enable parallel execution
  
#   results <- lapply(years, function(x) {
#     url <- paste0("https://sag.ices.dk/SAG_API/api/SAGDownload?year=", x, "&EcoRegion=", ecoreg)
#     tmpSAG <- tempfile(fileext = ".zip")
#     download.file(url, destfile = tmpSAG, mode = "wb", quiet = FALSE)
#     names <- unzip(tmpSAG, list = TRUE)
#     res <- read.csv(unz(tmpSAG, names$Name[1]),
#                     stringsAsFactors = FALSE,
#                     header = TRUE,
#                     fill = TRUE)
#     unique(res)
#   })
  
#   out <- do.call(rbind, results)
#   out <- dplyr::filter(out, Purpose == "Advice")
#   out <- dplyr::filter(out, FishStock %in% sid$StockKeyLabel)
  
#   return(out)
# }

getSAG_ecoregion_new <- function(Ecoregion) {
        # sag <- icesSAG::getStockDownloadData(assessment_keys)
        # # sag <- icesSAG::getSAG(stock_code, year,combine = TRUE, purpose = "Advice")
        # # sag <- dplyr::filter(sag, Purpose == "Advice")
        # # browser()
        # sag <- plyr::rbind.fill(lapply(sag, as.data.frame))
        # # sag <- do.call(rbind, sag)
        # return(sag)



        # sag_list <- future.apply::future_lapply(assessment_keys, function(key) {
        #         tryCatch(
        #                 {
        #                         icesSAG::getStockDownloadData(key)
        #                 },
        #                 error = function(e) NULL
        #         ) # Handle errors without stopping
        # })

        # sag <- plyr::rbind.fill(lapply(sag_list, as.data.frame))


        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/test_api/LatestStocks/Download?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag)
}


format_sag <- function(sag,sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        # sag <- dplyr::mutate(sag, StockKeyLabel=FishStock)
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        # df1 <- left_join(x, y)
        # df1 <- left_join(x, y, by = c("StockKeyLabel", "AssessmentYear"))
        df1 <-as.data.frame(df1)
        
        df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        # df1 <- subset(df1, select = -c(FishStock))
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        # check <-unique(df1[c("StockKeyLabel", "FisheriesGuild")])
        out <- dplyr::anti_join(df1, check)
}

stockstatus_CLD_current <- function(x) {
        df<- dplyr::select(x,Year,
                           StockKeyLabel,
                           FisheriesGuild,
                           FishingPressure,
                           AssessmentYear,
                           FMSY,
                           StockSize,
                           MSYBtrigger,
                           Catches,
                           Landings,
                           Discards)
        df$FishingPressure <- as.numeric(df$FishingPressure)
        df$StockSize <- as.numeric(df$StockSize)
        df$FMSY <- as.numeric(df$FMSY)
        df$MSYBtrigger <- as.numeric(df$MSYBtrigger)
        df$AssessmentYear <- as.numeric(df$AssessmentYear)
        df$Catches <- as.numeric(df$Catches)
        df$Landings <- as.numeric(df$Landings)
        df$Discards <- as.numeric(df$Discards)
        df$Year <- as.numeric(df$Year)

        df2 <- dplyr::group_by(df,StockKeyLabel)
        df2 <- dplyr::filter(df2,Year == AssessmentYear - 1)
        df2 <- dplyr::mutate(df2,F_FMSY =  ifelse(!is.na(FMSY),
                                                                FishingPressure / FMSY,
                                                                NA))
        df2 <- dplyr::select(df2,StockKeyLabel,
                                               FisheriesGuild,
                                               F_FMSY,
                                               Catches,
                                               Landings,
                                               Discards,
                                               FMSY,
                                               FishingPressure)
        df3 <- dplyr::group_by(df,StockKeyLabel)
        df3 <- dplyr::filter(df3, Year %in% c(AssessmentYear, (AssessmentYear - 1)))
        df3 <- dplyr::mutate(df3, SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                                        StockSize / MSYBtrigger,
                                                                        NA))
        df3 <- dplyr::select(df3, StockKeyLabel,Year,
                                               FisheriesGuild,
                                               SSB_MSYBtrigger,
                                               StockSize,
                                               MSYBtrigger)
        check <- unique(df3[c("StockKeyLabel", "Year", "MSYBtrigger")])
        check <- check[order(-check$Year),]
        check2 <- check[duplicated(check$StockKeyLabel),]
        df3 <- dplyr::anti_join(df3,check2)
        df4 <- dplyr::full_join(df2, df3)
        df4 <- dplyr::mutate(df4, Status = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
                                      "GREY",
                                      if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                                              "GREEN",
                                              "RED",
                                              "GREY")))
        df4
}

plot_GES_pies <- function(x, y, cap_month = "August",
                         cap_year = "2019",
                         return_data = FALSE) {
        df <- x
        cap_lab <- ggplot2::labs(title = "", x = "", y = "",
                        caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen",
                                          cap_month,
                                          cap_year))
        colList <- c("GREEN" = "#00B26D",
                     "GREY" = "#d3d3d3",
                     "ORANGE" = "#ff7f00",
                     "RED" = "#d93b1c",
                     "qual_RED" = "#d93b5c",
                     "qual_GREEN" = "#00B28F")


        df_stock <- dplyr::filter (df, lineDescription == "Maximum sustainable yield")
        df_stock <- dplyr::select(df_stock, StockKeyLabel,
                       FishingPressure,
                       StockSize)
        df_stock <- tidyr::gather(df_stock,Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)
        df2 <- dplyr::group_by(df_stock,Variable, Colour) %>%
                dplyr::summarize(COUNT = dplyr::n())%>%
                tidyr::spread(Colour, COUNT)
        df2[is.na(df2)] <- 0

        df3 <- dplyr::filter(y, StockKeyLabel %in% df_stock$StockKeyLabel)
        df3 <- dplyr::mutate(df3,CATCH = ifelse(is.na(Catches) & !is.na(Landings),
                                         Landings,
                                         Catches))
        df3 <- dplyr::select(df3,c(StockKeyLabel, CATCH))
        df4 <- dplyr::left_join(df_stock,df3)
        df4[is.na(df4)] <- 0
        df4 <- dplyr::group_by(df4,Variable, Colour) %>%
                dplyr::summarize(CATCH = sum(CATCH))%>%
                tidyr::spread(Colour, CATCH)
        df4 <- tidyr::gather(df4,Color, Catch, GREEN:RED, factor_key = TRUE)
        df2 <- tidyr::gather(df2,Color, Stocks, GREEN:RED, factor_key = TRUE)
        df5 <- merge(df2,df4)
        df5[is.na(df5)] <- 0
        tot <- sum(df5$Catch)/2
        stocks <- sum(df5$Stocks)/2
        df5 <- tidyr::gather(df5,Metric, Value, Stocks:Catch)
        df5 <- dplyr::group_by(df5,Metric)
        df5 <- dplyr::mutate(df5,sum = sum(Value)/2)
        # df5 <- df5 %>% group_by(Metric) %>% mutate(max = max(Value)/2)
        
        # filter out metric == Stocks
        df5 <- df5 %>% filter(Metric != "Stocks")
        df5$fraction <- ifelse(df5$Metric == "Stocks", (df5$Value*tot)/stocks, df5$Value)
        df5$Variable <- plyr::revalue(df5$Variable, c("FishingPressure"="Fishing Pressure", "StockSize"="Stock Size"))
        df5$Metric <- plyr::revalue(df5$Metric, c("Stocks"="Number of stocks", "Catch"="Proportion of catch \n(thousand tonnes)"))
        df5$Value2 <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)", df5$Value/1000, df5$Value)
        df5$sum2 <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)", df5$sum/1000, df5$sum)
        df5$Value <- as.integer(df5$Value)
        df5$Value2 <- as.integer(df5$Value2)
        df5$sum2 <- as.integer(df5$sum2)
        df5 <- df5 %>% filter(Value2 > 0)
        p1 <- ggplot2::ggplot(data = df5, ggplot2::aes(x = "", y = fraction, fill = Color)) +
                ggplot2::geom_bar(stat = "identity", width = 1) +
                ggplot2::geom_text(ggplot2::aes(label = Value2),
                          position = ggplot2::position_stack(vjust = 0.5),                          
                          size = 7) +
                ggplot2::geom_text(ggplot2::aes(label = paste0("total = ", sum2) ,x = 0, y = 0), size = 5)+
                ggplot2::scale_fill_manual(values = colList) +
                ggplot2::theme_bw(base_size = 20) +
                ggplot2::theme(panel.grid = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      legend.position="none") +
                ggplot2::theme(axis.text = ggplot2::element_blank(),
                      axis.ticks = ggplot2::element_blank(),
                      strip.background = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 13)) +
                cap_lab +
                ggplot2::coord_polar(theta = "y") +
                ggplot2::facet_grid(Metric ~ Variable)

        if(return_data == T){
                df5 <- subset(df5,select= -c(Value2, sum2))
                df5
        }else{
                p1
        }
}
plot_GES_pies_interactive <- function(x, y, cap_month = "August",
                                      cap_year = "2019",
                                      return_data = FALSE) {
    df <- x
    colList <- c("GREEN" = "#00B26D",
                 "GREY" = "#d3d3d3",
                 "ORANGE" = "#ff7f00",
                 "RED" = "#d93b1c",
                 "qual_RED" = "#d93b5c",
                 "qual_GREEN" = "#00B28F")

    df_stock <- dplyr::filter(df, lineDescription == "Maximum sustainable yield") %>%
        dplyr::select(StockKeyLabel, FishingPressure, StockSize) %>%
        tidyr::pivot_longer(cols = FishingPressure:StockSize, names_to = "Variable", values_to = "Colour")

    df3 <- dplyr::filter(y, StockKeyLabel %in% df_stock$StockKeyLabel) %>%
        dplyr::mutate(CATCH = ifelse(is.na(Catches) & !is.na(Landings), Landings, Catches)) %>%
        dplyr::select(StockKeyLabel, CATCH)

    df4 <- dplyr::left_join(df_stock, df3, by = "StockKeyLabel") %>%
        replace(is.na(.), 0) %>%
        dplyr::group_by(Variable, Colour) %>%
        dplyr::summarise(CATCH = sum(CATCH), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Colour, values_from = CATCH, values_fill = 0) %>%
        tidyr::pivot_longer(cols = GREEN:RED, names_to = "Color", values_to = "Catch")

    df4$Metric <- "Proportion of catch (thousand tonnes)"
    df4$fraction <- df4$Catch
    df4$Value2 <- as.integer(df4$Catch / 1000)
    df4$sum2 <- as.integer(sum(df4$Catch) / 1000)

    if (return_data) {
        return(df4)
    }

    # Create interactive pie chart using plotly
    p <- plotly::plot_ly(df4, labels = ~Color, values = ~fraction, type = "pie",
                          textinfo = "label+percent", text = ~paste("Catch:", Value2, "kt"),
                          hoverinfo = "text", marker = list(colors = colList)) %>%
        plotly::layout(title = paste("ICES Stock Assessment Database,", cap_month, cap_year),
                       showlegend = TRUE)

    p
}

# stock_trends <- function(x){
#         x$FishingPressure <- as.numeric(x$FishingPressure)
#         x$StockSize <- as.numeric(x$StockSize)
#         x$FMSY <- as.numeric(x$FMSY)
#         x$MSYBtrigger <- as.numeric(x$MSYBtrigger)
#         x$Year <- as.numeric(x$Year)
#         df <- dplyr::mutate(x,FMEAN = mean(FishingPressure, na.rm = TRUE),
#                        SSBMEAN = mean(StockSize, na.rm = TRUE),
#                        FMEAN = ifelse(!grepl("F|F(ages 3-6)", FishingPressureDescription),
#                                       NA,
#                                       FMEAN),
#                        SSBMEAN = ifelse(!grepl("StockSize", StockSizeDescription),
#                                         NA,
#                                         SSBMEAN))
#         df <- dplyr::mutate(df,F_FMSY = ifelse(!is.na(FMSY),
#                                        FishingPressure / FMSY,
#                                        NA),
#                        SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
#                                                 StockSize / MSYBtrigger,
#                                                 NA))
#         df <- dplyr::mutate(df,F_FMEAN = ifelse(!is.na(FMEAN),
#                                         FishingPressure / FMEAN, 
#                                         NA),
#                        SSB_SSBMEAN = ifelse(!is.na(SSBMEAN),
#                                             StockSize / SSBMEAN,
#                                           NA))
                                 
#         df <- df %>%
#                 # dplyr::rename("StockKeyLabel" = FishStock) %>%
#                 dplyr::select(
#                         Year,
#                         StockKeyLabel,
#                         FisheriesGuild,
#                         F_FMSY,
#                         SSB_MSYBtrigger,
#                         F_FMEAN,
#                         SSB_SSBMEAN
#                 )
#         df2 <-tidyr::gather(df,Metric, Value, -Year, -StockKeyLabel, -FisheriesGuild) 
#         df2 <- dplyr::filter(df2,!is.na(Year))
        
#         df3 <- dplyr::group_by(df2,StockKeyLabel, FisheriesGuild,Metric, Year)
#         df3 <- dplyr::summarize(df3,Value = mean(Value, na.rm = TRUE))
#         df3 <- dplyr::select(df3,FisheriesGuild,
#                        StockKeyLabel,
#                        Year,
#                        Metric,
#                        Value) 
#         df3 <- dplyr::filter(df3, !is.na(Value))
        
#         means <- dplyr::group_by(df2,FisheriesGuild, Metric, Year) 
#         # means <- dplyr::summarize(means, Value = mean(Value, na.rm = TRUE),
#         #                   StockKeyLabel = "MEAN")
#         means <- dplyr::summarize(means, 
#                           Value = dplyr::case_when(dplyr::n() >= 2 ~ mean(Value, na.rm = TRUE), 
#                                                    TRUE ~ as.numeric(NA)), 
#                           StockKeyLabel = "Mean")
#         means <- dplyr::select(means, FisheriesGuild,
#                        StockKeyLabel,
#                        Year,
#                        Metric,
#                        Value)
#         means <- dplyr::filter(means, !is.na(Value))
        
#         df4 <- dplyr::bind_rows(df3,means) 
#         df4 <- dplyr::distinct(df4,.keep_all = TRUE)
#         return(df4)
# }
stock_trends <- function(x){
  x$FishingPressure <- as.numeric(x$FishingPressure)
  x$StockSize <- as.numeric(x$StockSize)
  x$FMSY <- as.numeric(x$FMSY)
  x$MSYBtrigger <- as.numeric(x$MSYBtrigger)
  x$Year <- as.numeric(x$Year)

  df <- dplyr::mutate(x,
    FMEAN = mean(FishingPressure, na.rm = TRUE),
    SSBMEAN = mean(StockSize, na.rm = TRUE),
    FMEAN = ifelse(!grepl("F|F(ages 3-6)", FishingPressureDescription), NA, FMEAN),
    SSBMEAN = ifelse(!grepl("StockSize", StockSizeDescription), NA, SSBMEAN)
  )

  df <- dplyr::mutate(df,
    F_FMSY = ifelse(!is.na(FMSY), FishingPressure / FMSY, NA),
    SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger), StockSize / MSYBtrigger, NA),
    F_FMEAN = ifelse(!is.na(FMEAN), FishingPressure / FMEAN, NA),
    SSB_SSBMEAN = ifelse(!is.na(SSBMEAN), StockSize / SSBMEAN, NA)
  )

  df <- df %>%
    dplyr::select(Year, StockKeyLabel, FisheriesGuild, F_FMSY, SSB_MSYBtrigger, F_FMEAN, SSB_SSBMEAN)

  df2 <- tidyr::gather(df, Metric, Value, -Year, -StockKeyLabel, -FisheriesGuild) %>%
    dplyr::filter(!is.na(Year))

  df3 <- df2 %>%
    dplyr::group_by(StockKeyLabel, FisheriesGuild, Metric, Year) %>%
    dplyr::summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(Value))

  means <- df2 %>%
    dplyr::group_by(FisheriesGuild, Metric, Year) %>%
    dplyr::summarize(
      non_na_n = sum(!is.na(Value)),
      Value = ifelse(non_na_n >= 2, mean(Value, na.rm = TRUE), NA_real_),
      StockKeyLabel = "Mean",
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::select(FisheriesGuild, StockKeyLabel, Year, Metric, Value)

  df4 <- dplyr::bind_rows(df3, means) %>%
    dplyr::distinct(.keep_all = TRUE)

  return(df4)
}



# plot_stock_trends <- function(x, guild, cap_year, cap_month, return_data = FALSE){
#         df<- dplyr::filter(x,FisheriesGuild == guild)
#         adj_names <- sort(setdiff(unique(df$StockKeyLabel), "MEAN"))
#         values <- ggthemes::tableau_color_pal('Tableau 20')(length(adj_names))
#         legend_pos <- "bottom"
#         names(values) <- adj_names
#         values <- c(values, c(MEAN = "black"))
#         plot_title <- guild
#         cap_lab <- ggplot2::labs(title = plot_title, x = "Year", y = "",
#                 caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
#                                   cap_month,
#                                   cap_year))
#         duplicates <- dplyr::group_by(df,Value)
#         duplicates <- dplyr::filter(duplicates, dplyr::n()>1)
#         duplicates <- dplyr::filter(duplicates,StockKeyLabel == "MEAN")
#         df <- dplyr::anti_join(df,duplicates)
#         df <- dplyr::filter(df,Metric %in% c("F_FMSY", "SSB_MSYBtrigger"))
#         df$Metric[which(df$Metric == "F_FMSY")] <- "F/F[MSY]"
#         df$Metric[which(df$Metric == "SSB_MSYBtrigger")] <- "SSB/MSY~B[trigger]"
#         mean <- dplyr::filter(df, StockKeyLabel == "MEAN")
#         df2 <- dplyr::filter(df,StockKeyLabel != "MEAN")
        
#         plot <- ggplot2::ggplot(df2, ggplot2::aes(x = Year, y = Value,
#                       color = StockKeyLabel,
#                       fill = StockKeyLabel)) +
#                 ggplot2::geom_hline(yintercept = 1, col = "grey60") +
#                 ggplot2::theme_bw(base_size = 14) +
#                 ggplot2::scale_color_manual(values = values) +
#                 ggplot2::scale_fill_manual(values = values) +
#                 ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#                 # ggplot2::scale_y_continuous(limits = c(0, 10)) + ##
#                 ggplot2::guides(fill = FALSE) +
#                 ggplot2::theme(legend.position = legend_pos,
#                         strip.text = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
#                         strip.background = ggplot2::element_blank(),
#                         strip.placement = "outside",
#                         panel.grid.major = ggplot2::element_blank(),
#                         panel.grid.minor = ggplot2::element_blank(),
#                         legend.key = ggplot2::element_rect(colour = NA),
#                         legend.title = ggplot2::element_blank(),
#                         plot.caption = ggplot2::element_text(size = 10)) +
#                 cap_lab +
#                 ggplot2::facet_wrap(~ Metric, scales = "free_y", labeller = ggplot2::label_parsed, strip.position = "left", ncol = 1, nrow = 2)
#         plot <- plot + ggplot2::geom_line(data = df,alpha = 0.8)
#         plot <- plot + ggplot2::geom_line(data = mean,
#                                alpha = 0.9, size = 1.15)
        
#         if(return_data == T){
#                 df
#         }else{
#                 plotly::ggplotly(plot) %>% 
#                 plotly::highlight(on = "plotly_hover", selected = plotly::attrs_selected(showlegend = FALSE))
#         }
# }


# plot_stock_trends <- function(x, guild, cap_year, cap_month, return_data = FALSE) {
#   df <- dplyr::filter(x, FisheriesGuild == guild)
#   adj_names <- sort(setdiff(unique(df$StockKeyLabel), "MEAN"))
#   values <- ggthemes::tableau_color_pal('Tableau 20')(length(adj_names))
#   names(values) <- adj_names
#   values <- c(values, c(MEAN = "black"))
  
#   plot_title <- guild
#   cap_lab <- ggplot2::labs(
#     title = plot_title, 
#     x = "Year", 
#     caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen", cap_month, cap_year)
#   )

#   df <- df %>%
#     dplyr::filter(Metric %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
#     dplyr::mutate(Metric = dplyr::recode(Metric, "F_FMSY" = "F/F[MSY]", "SSB_MSYBtrigger" = "SSB/MSY~B[trigger]"))

#   mean_df <- dplyr::filter(df, StockKeyLabel == "MEAN")
#   df2 <- dplyr::filter(df, StockKeyLabel != "MEAN")

#   plot <- ggplot2::ggplot(df2, aes(x = Year, y = Value, color = StockKeyLabel, fill = StockKeyLabel)) +
#     ggplot2::geom_hline(yintercept = 1, col = "grey60") +
#     ggplot2::geom_line(alpha = 0.8) +
#     ggplot2::geom_line(data = mean_df, alpha = 0.9, size = 1.15) +
#     ggplot2::scale_color_manual(values = values) +
#     ggplot2::scale_fill_manual(values = values) +
#     ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#     ggplot2::theme_bw(base_size = 14) +
#     ggplot2::theme(
#       legend.position = "bottom",
#       strip.text = element_text(size = 14, hjust = 0.5),
#       strip.background = element_blank(),
#       strip.placement = "outside",
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       plot.caption = element_text(size = 10),
#       axis.title.y = element_blank()  # Removes default y-axis label
#     ) +
#     cap_lab +
#     ggplot2::facet_wrap(~ Metric, scales = "free_y", labeller = label_parsed, strip.position = "left", ncol = 1, nrow = 2) 

#   p <- plotly::ggplotly(plot) %>% plotly::highlight(on = "plotly_hover", selected = plotly::attrs_selected(showlegend = FALSE))

#   if (return_data) {
#     return(df)
#   } else {
#     return(p)
#   }
# }
plot_stock_trends <- function(x, guild, cap_year, cap_month, return_data = FALSE) {
        df <- dplyr::filter(x, FisheriesGuild == guild)
        adj_names <- sort(setdiff(unique(df$StockKeyLabel), "Mean"))
        values <- ggthemes::tableau_color_pal("Tableau 20")(length(adj_names))
        names(values) <- adj_names
        values <- c(values, c(MEAN = "black"))

        df <- df %>%
                dplyr::filter(Metric %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
                dplyr::mutate(Metric = dplyr::recode(Metric,
                        "F_FMSY" = "F/F<sub>MSY</sub>",
                        "SSB_MSYBtrigger" = "SSB/MSY B<sub>trigger</sub>"
                ))

        mean_df <- dplyr::filter(df, StockKeyLabel == "Mean")
        df2 <- dplyr::filter(df, StockKeyLabel != "Mean")

        # Function to create an individual plot for a given metric
        create_plot <- function(metric_name, yaxis_title, show_legend=TRUE) {
                df_metric <- dplyr::filter(df2, Metric == metric_name)
                mean_metric <- dplyr::filter(mean_df, Metric == metric_name)
                
                df_metric <- plotly::highlight_key(df_metric, key = ~StockKeyLabel)

                plotly::plot_ly() %>%
                        plotly::add_trace(
                                data = df_metric, x = ~Year, y = ~Value, color = ~StockKeyLabel,
                                colors = values, type = "scatter", mode = "lines",
                                line = list(width = 3), name = ~StockKeyLabel,
                                showlegend = show_legend
                        ) %>%
                        plotly::add_trace(
                                data = mean_metric, x = ~Year, y = ~Value, name = "Mean",
                                type = "scatter", mode = "lines", line = list(color = "black", width = 5),
                                showlegend = show_legend
                        ) %>%
                        plotly::layout(
                                yaxis = list(title = yaxis_title, zeroline = TRUE),
                                shapes = list(
                                        list(
                                                type = "rect", x0 = min(df$Year), x1 = max(df$Year),
                                                y0 = 1, y1 = 1, line = list(color = "#000000", width = 1)
                                        )
                                )
                        ) %>%
                        plotly::highlight(
                                on = 'plotly_hover',
                                off = 'plotly_doubleclick',
                                selected = plotly::attrs_selected(
                                        opacity = 0.7,
                                        showlegend = TRUE,
                                        line = list(width = 5) 
                                )
                        )
        }

        # Create individual subplots
        plot1 <- create_plot("F/F<sub>MSY</sub>", "F/F<sub>MSY</sub>", show_legend = TRUE)
        plot2 <- create_plot("SSB/MSY B<sub>trigger</sub>", "SSB/MSY B<sub>trigger</sub>", show_legend = FALSE)

        # Combine plots into a subplot layout
        final_plot <- plotly::subplot(plot1, plot2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
                plotly::layout(
                        title = guild,
                        xaxis = list(title = "Year"),
                        margin = list(b = 100),
                        annotations = list(
                                list(
                                        x = 1, y = -0.25, xref = "paper", yref = "paper",
                                        text = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen", cap_month, cap_year),
                                        showarrow = FALSE, xanchor = "right", yanchor = "bottom"
                                )
                        )
                )  
                

        if (return_data) {
                return(df)
        } else {
                return(final_plot)
        }
}

match_stockcode_to_illustration <- function(StockKeyLabel, df) {

  sapply(StockKeyLabel, function(key) {
    temp <- list.files("inst/app/www/fish", pattern = substr(key, 1, 3))
    if (length(temp) == 0) "fish.png" else temp[1]
  })
}