get_ecoregion_acronym <- function(ecoregion) {
  switch(ecoregion,
         "Baltic Sea" = "BtS",
         "Bay of Biscay and the Iberian Coast" = "BI",
         "Bay of Biscay" = "BoB",
         "Iberian Waters" = "IW",
         "Celtic Seas" = "CS",
         "Celtic Sea" = "CSx",
         "Irish Sea" = "IrS",
         "Greater North Sea" = "NrS",
         "Norwegian Sea" = "NwS",
         "Icelandic Waters" = "IS",
         "Barents Sea" = "BrS",
         "Greenland Sea" = "GS",
         "Faroes" = "FO",
         "Oceanic Northeast Atlantic" = "ONA",
         "Azores" = "AZ",
         stop("Unknown ecoregion")
  )
}

get_active_region_acronym <- function(subregion, ecoregion) {
  region <- if (!is.null(subregion)) subregion else ecoregion
  get_ecoregion_acronym(region)
}



CLD_trends <- function(x){
        df<- dplyr::select(x,Year,
                       StockKeyLabel,
                       FisheriesGuild,
                       FishingPressure,
                       FMSY,
                       StockSize,
                       MSYBtrigger,
                       Catches,
                       Landings,
                       Discards)
        df["Discards"][df["Discards"] == 0] <- NA
        df["Catches"][df["Catches"] == 0] <- NA
        df["Landings"][df["Landings"] == 0] <- NA
        # df <- dplyr::bind_rows(
        #                 # all present and accounted for
        #                dplyr::filter(df,!is.na(Catches),
        #                                !is.na(Landings),
        #                                !is.na(Discards)), 
        #                  # Missing discards, but catches == landings
        #                dplyr::filter(df, is.na(Discards),
        #                                Catches == Landings),
        #                  dplyr::mutate(df, Discards = 0), 
        #                # Missing catches, but have landings and discards
        #                dplyr::filter(df,is.na(Catches),
        #                                !is.na(Landings),
        #                                !is.na(Discards)), 
        #                dplyr::mutate(df,Catches = Landings + Discards), 
        #                 # missing catches, but have landings
        #                dplyr::filter(df, is.na(Catches),
        #                                !is.na(Landings),
        #                                is.na(Discards)),
        #                dplyr::mutate(df,Catches = NA,
        #                                Discards = NA),
        #                 # missing everything
        #                dplyr::filter(df, is.na(Catches),
        #                                is.na(Landings),
        #                                is.na(Discards)), 
        #                dplyr::mutate(df,Catches = NA,
        #                                Discards = NA,
        #                                Landings = NA),
        #                 # missing landings and discards
        #                dplyr::filter(df, !is.na(Catches),
        #                                is.na(Landings),
        #                                is.na(Discards)),
        #                dplyr::mutate(df,Landings = NA,
        #                                Discards = NA),
        #                 # landings and catches
        #                dplyr::filter(df, is.na(Catches),
        #                                is.na(Landings),
        #                                !is.na(Discards)),
        #                dplyr::mutate(df,Catches = NA,
        #                                Landings = NA),
        #                 # Missing discards, but have landings and catches
        #                dplyr::filter(df, !is.na(Catches),
        #                                !is.na(Landings),
        #                                is.na(Discards),
        #                                Landings != Catches),
        #                dplyr::mutate(df,Discards = Catches - Landings),
        #                 # Missing landings, but have catches and discards
        #                dplyr::filter(df, !is.na(Catches),
        #                                is.na(Landings),
        #                                !is.na(Discards)),
        #                dplyr::mutate(df,Landings = Catches - Discards)
        #         )
        return(df)
}

plot_discard_trends <- function(x, year, caption = FALSE, cap_year, cap_month, return_data = FALSE){
        df <- dplyr::filter(x,Year %in% seq(2011, year -1))
        df2 <- tidyr::expand(df,Year, tidyr::nesting(StockKeyLabel,FisheriesGuild))
        df <- dplyr::left_join(df,df2,
                          by = c("Year", "StockKeyLabel", "FisheriesGuild"))
        df3 <- dplyr::select(df, StockKeyLabel, Year, Discards)
        df3 <- unique(df3)
        df3 <- tibble::rowid_to_column(df3)
        df3 <- tidyr::spread(df3,Year, Discards)
        # df3<- dplyr::mutate(df3,`2017` = ifelse(AssessmentYear == 2017 &
        #                                        is.na(`2017`) &
        #                                        !is.na(`2016`),
        #                                `2016`,
        #                                `2017`))
        df3 <- tidyr::gather(df3,Year, Discards, 4:ncol(df3))
        df3 <- dplyr::mutate(df3,Year = as.numeric(Year),
                       Discards = as.numeric(Discards))

        df4<- dplyr::select(df,StockKeyLabel, Year, Landings)
        df4 <- unique(df4)
        df4 <- tibble::rowid_to_column(df4)
        df4 <- dplyr::group_by(df4,StockKeyLabel)
        df4 <- tidyr::spread(df4,Year, Landings)
        # df4 <- dplyr::mutate(df4,`2017` = ifelse(AssessmentYear == 2017 &
        #                                        is.na(`2017`) &
        #                                        !is.na(`2016`),
        #                                `2016`,
        #                                `2017`))
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
        df5 <- dplyr::filter(df5, value > 0)
        df6 <- df5 %>% dplyr::group_by(FisheriesGuild) %>% dplyr::slice(which.max(Year))
        plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                               ggplot2::aes(x = Year,
                                   y = value,
                                   color = FisheriesGuild)) +
                ggplot2::geom_line() +
                ggrepel::geom_label_repel(data = df6,
                                          ggplot2::aes(label = FisheriesGuild,
                                              color = FisheriesGuild,
                                              fill = FisheriesGuild),
                                          nudge_x = 1,
                                          label.size = 0.2,
                                          segment.size = 0.25,
                                          size = 2,
                                          color = 'white',
                                          force = 2,
                                          segment.color = 'grey60') +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_x_continuous(breaks = seq(min(df5$Year, na.rm = TRUE),
                                                max(df5$Year, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = max(df5$Year, na.rm = TRUE),
                                 y = -Inf, yend = -Inf), color = "grey50") +
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf,
                                 x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(df5$Year, na.rm = TRUE), year + 1)) +
                ggplot2::scale_color_brewer(type = "qual", palette = "Set2") +
                ggplot2::scale_fill_brewer(type = "qual", palette = "Set2") +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = "none",
                      plot.caption = ggplot2::element_text(size = 6),
                      panel.grid = ggplot2::element_blank(),
                      legend.key = ggplot2::element_rect(colour = NA)) +
                ggplot2::labs(x = "Year", y = "Discard rate", caption = "", title = "a)")

        if(caption == TRUE){
                cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                                           cap_month,
                                                           cap_year))
                plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                                        ggplot2::aes(x = Year,
                                                     y = value,
                                                     color = FisheriesGuild)) +
                        ggplot2::geom_line() +
                        ggrepel::geom_label_repel(data = df6,
                                                  ggplot2::aes(label = FisheriesGuild,
                                                               color = FisheriesGuild,
                                                               fill = FisheriesGuild),
                                                  nudge_x = 1,
                                                  label.size = 0.2,
                                                  segment.size = 0.25,
                                                  size = 2,
                                                  color = 'white',
                                                  force = 2,
                                                  segment.color = 'grey60') +
                        ggplot2::scale_y_continuous(labels = scales::percent) +
                        ggplot2::scale_x_continuous(breaks = seq(min(df5$Year, na.rm = TRUE),
                                                                 max(df5$Year, na.rm = TRUE), by = 2)) +
                        ggplot2::geom_segment(ggplot2::aes(x = min(df5$Year), xend = max(df5$Year, na.rm = TRUE),
                                                           y = -Inf, yend = -Inf), color = "grey50") +
                        ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf,
                                                           x = min(df5$Year), xend = -Inf), color = "grey50")+
                        ggplot2::expand_limits(x = c(min(df5$Year, na.rm = TRUE), year + 1)) +
                        ggplot2::scale_color_brewer(type = "qual", palette = "Set2") +
                        ggplot2::scale_fill_brewer(type = "qual", palette = "Set2") +
                        ggplot2::theme_bw(base_size = 9) +
                        ggplot2::theme(legend.position = "none",
                                       plot.caption = ggplot2::element_text(size = 6),
                                       panel.grid = ggplot2::element_blank(),
                                       legend.key = ggplot2::element_rect(colour = NA)) +
                        ggplot2::labs(x = "Year", y = "Discard rate", caption = "", title = "a)")+
                        cap_lab

        }

        if(return_data == TRUE){
                df5
        }else{
                plot
        }
}


plot_discard_trends_app <- function(x, year, caption = FALSE, cap_year, cap_month, return_data = FALSE){
        df <- dplyr::filter(x,Year %in% seq(2011, year -1))
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

plot_discard_trends_app_plotly <- function(x, year, caption = FALSE, cap_year, cap_month, return_data = FALSE) {

  df <- dplyr::filter(x, Year %in% seq(2011, year - 1))
  df2 <- tidyr::expand(df, Year, tidyr::nesting(StockKeyLabel, FisheriesGuild))
  df <- dplyr::left_join(df, df2, by = c("Year", "StockKeyLabel", "FisheriesGuild"))

  df3 <- df %>%
    dplyr::select(StockKeyLabel, Year, Discards) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    tidyr::spread(Year, Discards) %>%
    tidyr::gather(Year, Discards, 4:ncol(.)) %>%
    dplyr::mutate(Year = as.numeric(Year),
                  Discards = as.numeric(Discards))

  df4 <- df %>%
    dplyr::select(StockKeyLabel, Year, Landings) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(StockKeyLabel) %>%
    tidyr::spread(Year, Landings) %>%
    tidyr::gather(Year, Landings, 4:ncol(.)) %>%
    dplyr::mutate(Year = as.numeric(Year),
                  Landings = as.numeric(Landings))

  df5 <- df %>%
    dplyr::select(-Discards, -Landings) %>%
    dplyr::left_join(df3, by = c("Year", "StockKeyLabel")) %>%
    dplyr::left_join(df4, by = c("Year", "StockKeyLabel")) %>%
    dplyr::group_by(Year, FisheriesGuild) %>%
    dplyr::summarize(
      guildLandings = sum(Landings, na.rm = TRUE) / 1000,
      guildDiscards = sum(Discards, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
    dplyr::mutate(guildRate = guildDiscards / (guildLandings + guildDiscards)) %>%
    tidyr::pivot_longer(cols = guildRate, names_to = "variable", values_to = "value") %>%
    dplyr::filter(!is.na(value))

  if (return_data) {
    return(df5)
  }

  p <- plotly::plot_ly(
    data = df5,
    x = ~Year,
    y = ~value,
    color = ~FisheriesGuild,
    colors = "Set2",
    type = 'scatter',
    mode = 'lines',
    hoverinfo = 'text',
    text = ~paste(
      "Guild:", FisheriesGuild,
      "<br>Year:", Year,
      "<br>Discard rate:", scales::percent(value, accuracy = 0.1)
    )
  )

  p <- plotly::layout(
    p,
    yaxis = list(title = "Discard rate", tickformat = ".0%"),
    xaxis = list(title = "Year", dtick = 1),
    legend = list(title = list(text = "<b>Fisheries Guild</b>")),
    margin = list(t = ifelse(caption, 80, 50)),
    annotations = if (caption) list(
      list(
        xref = "paper", yref = "paper",
        x = 0, y = 1.1, showarrow = FALSE,
        text = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen", cap_month, cap_year),
        font = list(size = 12)
      )
    ) else NULL
  )

  return(p)
}


plot_discard_current <- function(x, year, position_letter = "c)",
                                 caption = TRUE, cap_year, cap_month,
                                 return_data = FALSE){
  df <- dplyr::filter(x,Year %in% seq(year-5, year -1))
  df2 <- tidyr::expand(df,Year, tidyr::nesting(StockKeyLabel,FisheriesGuild))
  df <- dplyr::left_join(df,df2,
                         by = c("Year", "StockKeyLabel", "FisheriesGuild"))
  # df <- df[, -11]
  df3 <- dplyr::select(df, StockKeyLabel, Year, Discards)
  df3 <- unique(df3)
  df3 <- tibble::rowid_to_column(df3)
  df3 <- dplyr::group_by(df3,StockKeyLabel)
  df3 <- tidyr::spread(df3,Year, Discards)
  # df3<- dplyr::mutate(df3,`2017` = ifelse(AssessmentYear == 2017 &
  #                                                 is.na(`2017`) &
  #                                                 !is.na(`2016`),
  #                                         `2016`,
  #                                         `2017`))
  df3 <- tidyr::gather(df3,Year, Discards, 3:ncol(df3))
  df3 <- dplyr::mutate(df3,Year = as.numeric(Year),
                       Discards = as.numeric(Discards))
  df5 <- dplyr::select(df,-Discards)
  df5 <- dplyr::left_join(df5,df3, by = c("Year", "StockKeyLabel"))
  
  df5$sum <- rowSums(df5[ , c(8:9,11)], na.rm = T)
  df5 <- dplyr::group_by(df5,Year, StockKeyLabel) %>% dplyr::slice_max(order_by = sum, n = 1, with_ties = FALSE)
  
  # df5 <- dplyr::left_join(df5,df4, by = c("Year", "StockKeyLabel", "AssessmentYear"))
  
         
  df5$Landings <- as.numeric(df5$Landings)
  df5$Catches <- as.numeric(df5$Catches)
  df5$Discards <- as.numeric(df5$Discards)
  
  df5[is.na(df5)] <- 0
  df5 <- unique(df5)
  
  df5 <- df5 %>% group_by(Year, FisheriesGuild) %>% summarise(across(where(is.numeric),sum))
  df5$Landings <- ifelse(!is.na(df5$Landings), df5$Landings, df5$Catches)
  

  # df7 <- dplyr::summarize(df5,guildLandings = sum(Landings, na.rm = TRUE),
  #                        guildDiscards = sum(Discards, na.rm = TRUE))
  # 
  # df5 <- dplyr::mutate(df5,guildRate = guildDiscards/ (guildLandings + guildDiscards))
  names(df5)[names(df5) == "Landings"] <- "guildLandings"
  names(df5)[names(df5) == "Discards"] <- "guildDiscards"
  
  
  df5 <- tidyr::gather(df5,variable, value, -Year, -FisheriesGuild)
  df5 <- dplyr::filter(df5, variable %in% c("guildLandings", "guildDiscards"))
  df5 <- dplyr::filter(df5,Year == year-1)
  df5$value <- df5$value/1000

  # df5 <- dplyr::filter(df5,!variable %in% c("guildDiscards", "guildLandings"))
  #out?
  # df5 <- dplyr::filter(df5,Year == year - 1)

  # df5_order <- dplyr::group_by(df5,FisheriesGuild) %>%
  #         summarize(total = sum(value, na.rm = TRUE)) %>%
  #         arrange(-total) %>%
  #         ungroup()
  # df5_order <- dplyr::mutate(df5_order,FisheriesGuild = factor(FisheriesGuild, FisheriesGuild))

  # df5$FisheriesGuild <- factor(df5$FisheriesGuild,
  #                                 levels = df5_order$FisheriesGuild[order(df5_order$total)])
  plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                          ggplot2::aes(x = reorder(FisheriesGuild, value, sum), y = value, fill = variable)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
          ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
          ggplot2::coord_flip() +
          ggplot2::theme_bw(base_size = 8) +
          ggplot2::theme(legend.position = "none",
                plot.caption = ggplot2::element_text(size = 6),
                panel.grid = ggplot2::element_blank(),
                legend.key = ggplot2::element_rect(colour = NA)) +
          ggplot2::labs(x = "", y = "Landings and Discards(thousand tonnes)",title = position_letter)

  if(caption == TRUE) {
    cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                               cap_month,
                                               cap_year))
    # df5$value <- df5$value/100
    
    plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                            ggplot2::aes(x = reorder(FisheriesGuild, value, sum), y = value, fill = variable)) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
            ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw(base_size = 8) +
            ggplot2::theme(legend.position = "none",
                           plot.caption = ggplot2::element_text(size = 6),
                           panel.grid = ggplot2::element_blank(),
                           legend.key = ggplot2::element_rect(colour = NA)) +
            ggplot2::labs(x = "", y = "Landings and Discards(thousand tonnes)",title = position_letter)+
            cap_lab
  }

  if(return_data == T){
          df5$value <- df5$value*1000
          df5
  }else{
          plot
  }

}


plot_discard_current_order <- function(x, year, order_df,position_letter = "c)",
                                 caption = TRUE, cap_year, cap_month,
                                 return_data = FALSE){
        df <- dplyr::filter(x,Year %in% seq(year-5, year -1))
        df2 <- tidyr::expand(df,Year, tidyr::nesting(StockKeyLabel,FisheriesGuild))
        df <- dplyr::left_join(df,df2,
                               by = c("Year", "StockKeyLabel", "FisheriesGuild"))
        # df <- df[, -11]
        df3 <- dplyr::select(df, StockKeyLabel, Year, Discards)
        df3 <- unique(df3)
        df3 <- tibble::rowid_to_column(df3)
        df3 <- dplyr::group_by(df3,StockKeyLabel)
        df3 <- tidyr::spread(df3,Year, Discards)
        # df3<- dplyr::mutate(df3,`2017` = ifelse(AssessmentYear == 2017 &
        #                                                 is.na(`2017`) &
        #                                                 !is.na(`2016`),
        #                                         `2016`,
        #                                         `2017`))
        df3 <- tidyr::gather(df3,Year, Discards, 3:ncol(df3))
        df3 <- dplyr::mutate(df3,Year = as.numeric(Year),
                             Discards = as.numeric(Discards))
        df5 <- dplyr::select(df,-Discards)
        df5 <- dplyr::left_join(df5,df3, by = c("Year", "StockKeyLabel"))
        
        df5$sum <- rowSums(df5[ , c(8:9,11)], na.rm = T)
        df5 <- dplyr::group_by(df5,Year, StockKeyLabel)%>% top_n(1,sum)
        
        # df5 <- dplyr::left_join(df5,df4, by = c("Year", "StockKeyLabel", "AssessmentYear"))
        
        
        df5$Landings <- as.numeric(df5$Landings)
        df5$Catches <- as.numeric(df5$Catches)
        df5$Discards <- as.numeric(df5$Discards)
        
        df5[is.na(df5)] <- 0
        df5 <- unique(df5)
        
        df5 <- df5 %>% group_by(Year, FisheriesGuild) %>% summarise(across(where(is.numeric),sum))
        df5$Landings <- ifelse(!is.na(df5$Landings), df5$Landings, df5$Catches)
        
        
        # df7 <- dplyr::summarize(df5,guildLandings = sum(Landings, na.rm = TRUE),
        #                        guildDiscards = sum(Discards, na.rm = TRUE))
        # 
        # df5 <- dplyr::mutate(df5,guildRate = guildDiscards/ (guildLandings + guildDiscards))
        names(df5)[names(df5) == "Landings"] <- "guildLandings"
        names(df5)[names(df5) == "Discards"] <- "guildDiscards"
        
        
        df5 <- tidyr::gather(df5,variable, value, -Year, -FisheriesGuild)
        df5 <- dplyr::filter(df5, variable %in% c("guildLandings", "guildDiscards"))
        df5 <- dplyr::filter(df5,Year == year-1)
        df5$value <- df5$value/1000
        
        # df5 <- dplyr::filter(df5,!variable %in% c("guildDiscards", "guildLandings"))
        #out?
        # df5 <- dplyr::filter(df5,Year == year - 1)
        
        # df5_order <- dplyr::group_by(df5,FisheriesGuild) %>%
        #         summarize(total = sum(value, na.rm = TRUE)) %>%
        #         arrange(-total) %>%
        #         ungroup()
        # df5_order <- dplyr::mutate(df5_order,FisheriesGuild = factor(FisheriesGuild, FisheriesGuild))
        
        # df5$FisheriesGuild <- factor(df5$FisheriesGuild,
        #                                 levels = df5_order$FisheriesGuild[order(df5_order$total)])
        plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                                ggplot2::aes(x = reorder(order_df$FisheriesGuild, order_df$value, sum), y = value, fill = variable)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
                ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
                ggplot2::coord_flip() +
                ggplot2::theme_bw(base_size = 8) +
                ggplot2::theme(legend.position = "none",
                               plot.caption = ggplot2::element_text(size = 6),
                               panel.grid = ggplot2::element_blank(),
                               legend.key = ggplot2::element_rect(colour = NA)) +
                ggplot2::labs(x = "", y = "Landings and Discards(thousand tonnes)",title = position_letter)
        
        if(caption == TRUE) {
                cap_lab <- ggplot2::labs(caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                                           cap_month,
                                                           cap_year))
                # df5$value <- df5$value/100
                
                plot <- ggplot2::ggplot(dplyr::ungroup(df5),
                                        ggplot2::aes(x = reorder(order_df$FisheriesGuild, order_df$value, sum), y = value, fill = variable)) +
                        ggplot2::geom_bar(stat = "identity") +
                        ggplot2::scale_color_brewer(type = "qual", palette = "Dark2", direction = -1) +
                        ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
                        ggplot2::coord_flip() +
                        ggplot2::theme_bw(base_size = 8) +
                        ggplot2::theme(legend.position = "none",
                                       plot.caption = ggplot2::element_text(size = 6),
                                       panel.grid = ggplot2::element_blank(),
                                       legend.key = ggplot2::element_rect(colour = NA)) +
                        ggplot2::labs(x = "", y = "Landings and Discards(thousand tonnes)",title = position_letter)+
                        cap_lab
        }
        
        if(return_data == T){
                df5$value <- df5$value*1000
                df5
        }else{
                plot
        }
        
}


plot_discard_current_plotly <- function(x, year, position_letter = NULL,
                                        caption = TRUE, cap_year, cap_month,
                                        return_data = FALSE, order_df = NULL) {
  df <- dplyr::filter(x, Year %in% seq(year - 5, year - 1))
  df2 <- tidyr::expand(df, Year, tidyr::nesting(StockKeyLabel, FisheriesGuild))
  df <- dplyr::left_join(df, df2, by = c("Year", "StockKeyLabel", "FisheriesGuild"))
  
  df3 <- dplyr::select(df, StockKeyLabel, Year, Discards) %>%
    unique() %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(StockKeyLabel) %>%
    tidyr::spread(Year, Discards) %>%
    tidyr::gather(Year, Discards, -StockKeyLabel, -rowid) %>%
    dplyr::mutate(Year = as.numeric(Year), Discards = as.numeric(Discards))
  
  df5 <- dplyr::select(df, -Discards)
  df5 <- dplyr::left_join(df5, df3, by = c("Year", "StockKeyLabel"))
  
  df5$sum <- rowSums(df5[, c("Landings", "Catches", "Discards")], na.rm = TRUE)
  df5 <- dplyr::group_by(df5, Year, StockKeyLabel) %>%
    dplyr::top_n(1, sum) %>%
    dplyr::ungroup()
  
  df5 <- dplyr::mutate(df5,
                       Landings = as.numeric(Landings),
                       Catches = as.numeric(Catches),
                       Discards = as.numeric(Discards))
  df5[is.na(df5)] <- 0
  
  df5 <- df5 %>%
    dplyr::group_by(Year, FisheriesGuild) %>%
    dplyr::summarise(across(where(is.numeric), sum), .groups = "drop")
  
  df5$Landings <- ifelse(!is.na(df5$Landings), df5$Landings, df5$Catches)
  names(df5)[names(df5) == "Landings"] <- "guildLandings"
  names(df5)[names(df5) == "Discards"] <- "guildDiscards"
  
  df5 <- tidyr::gather(df5, variable, value, -Year, -FisheriesGuild) %>%
    dplyr::filter(variable %in% c("guildLandings", "guildDiscards"),
                  Year == year - 1) %>%
    dplyr::mutate(value = value / 1000)

  # Apply ordering if provided
  if (!is.null(order_df)) {
    # Ensure unique ordering levels
    unique_levels <- unique(order_df$FisheriesGuild)

    df5 <- dplyr::left_join(df5, order_df, by = "FisheriesGuild") %>%
    dplyr::mutate(FisheriesGuild = factor(FisheriesGuild, levels = unique_levels)) %>%
    dplyr::select(-value.y) %>%
    dplyr::rename(value = value.x)
  }

  if (return_data) {
    df5$value <- df5$value * 1000
    return(df5)
  }
  
    # Create color scale
  color_scale <- c("guildLandings" = "#1d9e76", "guildDiscards" = "#d86003")
  
  plot <- plotly::plot_ly(
    data = df5,
    x = ~value,
    y = ~FisheriesGuild,
    color = ~variable,
    colors = color_scale,
    type = "bar",
    orientation = "h"
  ) %>%
    plotly::layout(
      barmode = "stack",
      title = list(text = position_letter, font = list(size = 14)),
      xaxis = list(title = "Landings and Discards (thousand tonnes)"),
      yaxis = list(title = ""),
      showlegend = FALSE,
      margin = list(l = 100),
      annotations = if (caption) list(
        list(
          text = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen", cap_month, cap_year),
          xref = "paper", yref = "paper",
          x = 0, y = -0.25, showarrow = FALSE,
          font = list(size = 10), align = "left"
        )
      ) else NULL
    )

  return(plot)
}
