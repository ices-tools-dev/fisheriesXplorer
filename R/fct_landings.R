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
      
        return(df)
}



plot_catch_trends_plotly <- function(
  x,
  type = c("Common name", "Country", "Fisheries guild"),
  line_count = 10,
  dataUpdated = NULL,
  return_data = FALSE,
  session = NULL,
  per_panel_height = 380,
  ecoregion = NULL
) {
  type <- match.arg(type)

  # --- Responsive font sizes (fallback to 800px)
  w <- tryCatch({
    if (!is.null(session)) session$clientData[["output_landings_1-landings_layer_width"]] else NA_real_
  }, error = function(e) NA_real_)
  if (is.na(w) || is.null(w)) w <- 800

  base_size         <- max(9,  min(18, round(w / 55)))
  axis_title_size   <- max(10, min(20, round(w / 50)))
  tick_size         <- max(9,  min(16, round(w / 55)))
  legend_title_size <- max(10, min(18, round(w / 55)))
  legend_text_size  <- max(9,  min(16, round(w / 65)))
  title_annot_size  <- max(12, min(22, round(w / 40)))
  caption_size      <- max(8,  min(14, round(w / 70)))

  # --- Expected columns in this order
  names(x) <- c("Year", "Country", "iso3", "Fisheries guild", "Ecoregion",
                "Species name", "Species code", "Common name", "Value")

  cap_text <- paste0(
    "Historical Nominal Catches 1950–2006.\n",
    "Official Nominal Catches 2006–2023.\n",
    dataUpdated, ", ICES, Copenhagen."
  )

  # --- Helpers
  sanitize_stub <- function(s) gsub("[^A-Za-z0-9]+", "_", s)
  date_stamp <- format(Sys.Date(), "%d-%b-%y")

  df <- dplyr::rename(x, type_var = dplyr::all_of(type))

  if (type == "Common name") {
    df$type_var <- gsub("European ", "", df$type_var)
    df$type_var <- gsub("Sandeels.*", "sandeel", df$type_var)
    df$type_var <- gsub("Finfishes nei", "undefined finfish", df$type_var)
    df$type_var <- gsub("Blue whiting.*", "blue whiting", df$type_var)
    df$type_var <- gsub("Saithe.*", "saithe", df$type_var)
    df$type_var <- ifelse(grepl("Norway", df$type_var), df$type_var, tolower(df$type_var))
  }

  # --- Palette helper: Temps
  palette_vec <- function(n) grDevices::hcl.colors(max(n, 1), palette = "Temps")

  # Prep per-guild dataset (rank within guild; keep top 'line_count', others -> "other")
  prep_one_guild <- function(.g) {
    df_g <- df %>% dplyr::filter(`Fisheries guild` == .g)

    ranks <- df_g %>%
      dplyr::group_by(type_var) %>%
      dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(typeTotal)) %>%
      dplyr::filter(typeTotal >= 1) %>%
      dplyr::mutate(RANK = dplyr::min_rank(dplyr::desc(typeTotal)))

    df_g2 <- df_g %>%
      dplyr::inner_join(ranks, by = "type_var") %>%
      dplyr::mutate(type_var = ifelse(RANK > line_count, "other", type_var)) %>%
      dplyr::group_by(`Fisheries guild`, type_var, Year) %>%
      dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") %>%
      dplyr::filter(!is.na(Year))

    levels_i <- df_g2 %>%
      dplyr::group_by(type_var) %>%
      dplyr::summarise(tt = sum(typeTotal), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(tt)) %>%
      dplyr::pull(type_var)

    transform(df_g2, type_var = factor(type_var, levels = levels_i))
  }

  # ----------------------------
  # FORCED STACKED MODE when type == "Common name"
  # ----------------------------
  if (type == "Common name") {
    guilds <- df %>%
      dplyr::filter(!is.na(`Fisheries guild`) & `Fisheries guild` != "" & `Fisheries guild` != "undefined") %>%
      dplyr::pull(`Fisheries guild`) %>%
      unique() %>%
      sort()

    if (length(guilds) == 0) {
      # fall back to single-plot below
    } else {
      guild_dfs <- lapply(guilds, prep_one_guild)
      if (return_data) return(dplyr::bind_rows(guild_dfs))

      x_rng <- range(df$Year, na.rm = TRUE)

      plot_list <- vector("list", length(guilds))
      for (i in seq_along(guilds)) {
        gname  <- guilds[i]
        plot_i <- guild_dfs[[i]]
        n_types_i <- length(levels(plot_i$type_var))
        pal_i <- palette_vec(n_types_i)

        file_stub <- paste0(ecoregion, "_landings_", sanitize_stub(gname), "_", date_stamp)

        p_i <- plotly::plot_ly(
          plot_i, x = ~Year, y = ~typeTotal,
          color = ~type_var, colors = pal_i, showlegend = TRUE
        ) %>%
          plotly::add_trace(type = "scatter", mode = "lines", line = list(width = 3)) %>%
          plotly::layout(
            height = per_panel_height,
            font = list(size = base_size),
            xaxis = list(
              title = list(text = "Year", font = list(size = axis_title_size)),
              tickfont = list(size = tick_size),
              range = x_rng,
              automargin = TRUE
            ),
            yaxis = list(
              title = list(text = "Landings (thousand tonnes)",
                           font = list(size = axis_title_size), standoff = 18),
              tickfont = list(size = tick_size),
              automargin = TRUE
            ),
            margin = list(l = 80, r = 20, t = 110, b = 90),
            annotations = list(
              list(
                text = paste0("Landings trends: ", gname, " (", ecoregion, ")"),
                x = 0.01, y = 0.98,
                xref = "paper", yref = "paper",
                showarrow = FALSE,
                xanchor = "left", yanchor = "top",
                font = list(size = title_annot_size, color = "black")
              ),
              list(
                x = 1, y = -0.42,
                text = cap_text,
                showarrow = FALSE,
                xref = "paper", yref = "paper",
                xanchor = "right", yanchor = "bottom",
                font = list(size = caption_size, color = "black")
              )
            ),
            legend = list(
              title = list(text = "<b>Common name</b>", font = list(size = legend_title_size)),
              orientation = "h",
              y = 1.12, x = 0, xanchor = "left", yanchor = "bottom",
              font = list(size = legend_text_size),
              itemwidth = 50
            ),
            hoverlabel = list(font = list(size = base_size))
          ) %>%
          plotly::highlight(
            on = "plotly_hover",
            off = "plotly_doubleclick",
            selected = plotly::attrs_selected(opacity = 0.7, line = list(width = 5))
          ) %>%
          plotly::config(
            responsive = TRUE,
            toImageButtonOptions = list(
              filename = file_stub,
              format   = "png",
              scale    = 3
              # width  = 1600,
              # height = 900
            )
          )

        plot_list[[i]] <- p_i
      }

      return(htmltools::tagList(plot_list))
    }
  }

  # ----------------------------
  # SINGLE-PLOT path for other 'type' values
  # ----------------------------
  plot_df <- df %>%
    dplyr::group_by(type_var) %>%
    dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(typeTotal)) %>%
    dplyr::filter(typeTotal >= 1) %>%
    dplyr::mutate(RANK = dplyr::min_rank(dplyr::desc(typeTotal))) %>%
    dplyr::inner_join(df, by = "type_var") %>%
    dplyr::mutate(type_var = ifelse(RANK > line_count, "other", type_var)) %>%
    dplyr::group_by(type_var, Year) %>%
    dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") %>%
    dplyr::filter(!is.na(Year))

  if (return_data) return(plot_df)

  n_types <- length(unique(plot_df$type_var))
  pal <- palette_vec(n_types)

  file_stub <- paste0("landings_", sanitize_stub(type), "_", date_stamp)

  plotly::plot_ly(plot_df, x = ~Year, y = ~typeTotal, color = ~type_var, colors = pal) %>%
    plotly::add_trace(type = "scatter", mode = "lines", line = list(width = 3)) %>%
    plotly::layout(
      font = list(size = base_size),
      xaxis = list(
        title = list(text = "Year", font = list(size = axis_title_size)),
        tickfont = list(size = tick_size),
        automargin = TRUE
      ),
      yaxis = list(
        title = list(text = "Landings (thousand tonnes)",
                     font = list(size = axis_title_size), standoff = 18),
        tickfont = list(size = tick_size),
        automargin = TRUE
      ),
      margin = list(l = 80, r = 20, t = 60, b = 90),
      annotations = list(
        list(
          x = 1, y = -0.3,
          text = cap_text,
          showarrow = FALSE,
          xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "bottom",
          font = list(size = caption_size, color = "black")
        ),
        list(
          text = paste0("Landings Trends (", ecoregion, ")"),
          x = 0.01, y = 0.99,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "left", yanchor = "top",
          font = list(size = title_annot_size, color = "black")
        )
      ),
      legend = list(
        title = list(text = paste0("<b>", type, "</b>"), font = list(size = legend_title_size)),
        orientation = "h",
        x = 0.5, y = 1.08, xanchor = "center", yanchor = "bottom",
        font = list(size = legend_text_size),
        itemwidth = 50
      ),
      hoverlabel = list(font = list(size = base_size))
    ) %>%
    plotly::highlight(
      on = "plotly_hover",
      off = "plotly_doubleclick",
      selected = plotly::attrs_selected(opacity = 0.7, line = list(width = 5))
    ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = file_stub,
        format   = "png",
        scale    = 3
      )
    )
}









plot_discard_trends_app_plotly <- function(x, year, return_data = FALSE, ecoregion = NULL) {
  # Check for non-numeric Year values and warn if any NAs are introduced

    # --- Responsive font sizes (fallback to 800px)
  w <- tryCatch({
    if (!is.null(session)) session$clientData[["output_landings_1-discard_trends_width"]] else NA_real_
  }, error = function(e) NA_real_)
  if (is.na(w) || is.null(w)) w <- 800

  base_size         <- max(9,  min(18, round(w / 55)))
  axis_title_size   <- max(10, min(20, round(w / 50)))
  tick_size         <- max(9,  min(16, round(w / 55)))
  legend_title_size <- max(10, min(18, round(w / 55)))
  legend_text_size  <- max(9,  min(16, round(w / 65)))
  title_annot_size  <- max(12, min(22, round(w / 40)))
  caption_size      <- max(8,  min(14, round(w / 70)))


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
      font = list(size = axis_title_size),
      tickfont = list(size = tick_size)
    ),
    xaxis = list(
      title = "Year",
      dtick = 1,
      font = list(size = axis_title_size),
      tickfont = list(size = tick_size)
    ),
    legend = list(title = list(text = "<b>Fisheries Guild</b>")),
    margin = list(b = 100),
    annotations = list(
      list(
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        yanchor = "bottom",
        x = 1, y = -0.2,
        showarrow = FALSE,
        text = paste0("ICES Stock Assessment Database,", format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
        font = list(size = caption_size)
      ),
      list(
        text = paste0("Discard trends ", " (", ecoregion, ")"),
        x = 0.01, y = 0.98,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "left", yanchor = "top",
        font = list(size = title_annot_size, color = "black")
      )
    )
  ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = paste0(ecoregion, "_DiscardTrends_", format(Sys.Date(), "%d-%b-%y")),
        format   = "png",
        scale    = 3
        # width  = 1600,
        # height = 900
      )
    )

  return(p)
}


plot_discard_current_plotly <- function(x, year, position_letter = NULL, return_data = FALSE, order_df = NULL, ecoregion = NULL) {
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
  
  df5 <- df5 %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0)))

  
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
      yaxis = list(title = "Fisheries Guild", 
        # font = list(size = 13),
        tickfont = list(size = 13)),
      showlegend = TRUE,
      margin = list(l = 20, r = 20, t = 50, b = 120),
      annotations = list(
        list(
          xref = "paper", 
          yref = "paper",
          xanchor = "right", 
          yanchor = "bottom",
          x = 1, y = -0.4, 
          showarrow = FALSE,
          text = paste0("ICES Stock Assessment Database,", format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
          font = list(size = 12)
        )
      )  
    ) %>% 
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = paste0(ecoregion, "_CurrentDiscards_", position_letter, "_", format(Sys.Date(), "%d-%b-%y")),
        format   = "png",
        scale    = 3
        # width  = 1600,
        # height = 900
      )
    )

  return(plot)
}
