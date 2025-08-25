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

plot_catch_trends_plotly <- function(x, type = c("Common name", "Country", "Fisheries guild"),
                                      line_count = 10,
                                      plot_type = c("line", "area"),
                                      official_catches_year = NULL,
                                      return_data = FALSE) {
    
    names(x) <- c("Year", "Country", "iso3", "Fisheries guild", "Ecoregion", "Species name", "Species code", "Common name", "Value")
    capyear <- official_catches_year - 1
    cap_text <- sprintf("Historical Nominal Catches 1950-2010,\nOfficial Nominal Catches 2006-%s\nPreliminary Catches %s\nICES, Copenhagen.", capyear, official_catches_year)

    df <- dplyr::rename(x, type_var = dplyr::all_of(type))
    
    if (type == "Common name") {
        df$type_var <- gsub("European ", "", df$type_var)
        df$type_var <- gsub("Sandeels.*", "sandeel", df$type_var)
        df$type_var <- gsub("Finfishes nei", "undefined finfish", df$type_var)
        df$type_var <- gsub("Blue whiting.*", "blue whiting", df$type_var)
        df$type_var <- gsub("Saithe.*", "saithe", df$type_var)
        df$type_var <- ifelse(grepl("Norway", df$type_var), df$type_var, tolower(df$type_var))
    }
    
    plot <- df %>%
        dplyr::group_by(type_var) %>%
        dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE)) %>%
        dplyr::arrange(dplyr::desc(typeTotal)) %>%
        dplyr::filter(typeTotal >= 1) %>%
        dplyr::mutate(RANK = dplyr::min_rank(dplyr::desc(typeTotal))) %>%
        dplyr::inner_join(df, by = "type_var") %>%
        dplyr::mutate(type_var = ifelse(RANK > line_count, "other", type_var)) %>%
        dplyr::group_by(type_var, Year) %>%
        dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE) / 1000) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(Year))

    unique_types <- unique(plot$type_var)

# Create a highlight key
    plot <- plotly::highlight_key(plot, key = ~type_var)    
    p <- plotly::plot_ly(plot, x = ~Year, y = ~typeTotal, color = ~type_var)
    
    if (plot_type == "area") {
        p <- p %>% plotly::add_trace(type = 'scatter', mode = 'none', stackgroup = 'one')
    } else {
        p <- p %>% plotly::add_trace(type = "scatter", mode = "lines", line = list(width = 3))
    }
    
    p <- p %>% plotly::layout(
        title = "Landings Trends",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Landings (thousand tonnes)"),
        margin = list(b = 100),
        annotations = list(
            list(
                x = 1, y = -0.38, text = cap_text,
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

plot_discard_trends_app_plotly <- function(x, year, caption = FALSE, cap_year, cap_month, return_data = FALSE) {
  
  # Check for non-numeric Year values and warn if any NAs are introduced
  year_numeric <- suppressWarnings(as.numeric(x$Year))
  if (any(is.na(year_numeric) & !is.na(x$Year))) {
    warning("Non-numeric values detected in 'Year' column. These rows will be removed.")
  }
  df <- x %>%
    dplyr::mutate(Year = year_numeric) %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::filter(Year %in% seq(2011, year - 1))

  df2 <- tidyr::expand(df, Year, tidyr::nesting(StockKeyLabel, FisheriesGuild))
  df <- dplyr::left_join(df, df2, by = c("Year", "StockKeyLabel", "FisheriesGuild"))

  df3 <- df %>%
    dplyr::select(StockKeyLabel, Year, Discards) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    tidyr::spread(Year, Discards) %>%
    tidyr::gather(Year, Discards, 4:ncol(.)) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Discards = as.numeric(Discards)
    )

  df4 <- df %>%
    dplyr::select(StockKeyLabel, Year, Landings) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(StockKeyLabel) %>%
    tidyr::spread(Year, Landings) %>%
    tidyr::gather(Year, Landings, 4:ncol(.)) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Landings = as.numeric(Landings)
    )

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
    type = "scatter",
    mode = "lines",
    line = list(width = 3),
    hoverinfo = "text",
    text = ~ paste(
      "Guild:", FisheriesGuild,
      "<br>Year:", Year,
      "<br>Discard rate:", scales::percent(value, accuracy = 0.1)
    )
  )

  p <- plotly::layout(
    p,
    yaxis = list(
      title = "Discard rate",
      tickformat = ".0%",
      font = list(size = 14),
      tickfont = list(size = 13)
    ),
    xaxis = list(
      title = "Year",
      dtick = 1,
      font = list(size = 14),
      tickfont = list(size = 13)
    ),
    legend = list(title = list(text = "<b>Fisheries Guild</b>")),
    margin = list(t = ifelse(caption, 80, 50)),
    annotations = if (caption) {
      list(
        list(
          xref = "paper", yref = "paper",
          x = 0, y = 1.1, showarrow = FALSE,
          text = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen", cap_month, cap_year),
          font = list(size = 12)
        )
      )
    } else {
      NULL
    }
  )

  return(p)
}


plot_discard_current_plotly <- function(x, year, position_letter = NULL,
                                        caption = FALSE, cap_year, cap_month,
                                        return_data = FALSE, order_df = NULL) {
  df <- x %>% dplyr::mutate(Year = as.numeric(Year),
                            FMSY = as.numeric(FMSY),
                            MSYBtrigger = as.numeric(MSYBtrigger)) %>% dplyr::filter(Year %in% seq(year - 5, year - 1))
  
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
  
  ## rename the rows of variable guildLandings and guildDiscards to Landings and Discards
  df5 <- dplyr::mutate(df5, variable = dplyr::recode(variable, guildLandings = "Landings", guildDiscards = "Discards"))
    # Create color scale
  color_scale <- c("Landings" = "#1d9e76", "Discards" = "#d86003")
  
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
      title = list(text = position_letter, font = list(size = 15)),
      xaxis = list(
        title = "Discards and Landings (thousand tonnes)", 
        font = list(size = 14),
        tickfont = list(size = 13)), 
      yaxis = list(title = "", 
        # font = list(size = 13),
        tickfont = list(size = 13)),
      showlegend = TRUE,
      margin = list(l = 100),
      annotations = if (caption) list(
        list(
          text = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen", cap_month, cap_year),
          xref = "paper", yref = "paper",
          x = 0, y = -0.1, showarrow = FALSE,
          font = list(size = 10), align = "left"
        )
      ) else NULL
    )

  return(plot)
}
