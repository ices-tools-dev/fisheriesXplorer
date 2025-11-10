################# Data processing ################################################
getSID <- function(year, EcoR) {
        
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

getSAG_ecoregion_new <- function(Ecoregion) {
       
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Download?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag)
}

getStatusWebService <- function(Ecoregion, sid) {
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        status <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Status?ecoregion=%s", EcoregionCode)
                )
        )
        status_long <- status %>%
                tidyr::unnest(YearStatus)

        df_status <- merge(sid, status_long, by = "AssessmentKey", all.x = TRUE)
        df_status$FisheriesGuild <- tolower(df_status$FisheriesGuild)
        
        return(df_status)
}


format_sag_status_new <- function(df) {
     
        df <- dplyr::mutate(df,status = dplyr::case_when(status == 0 ~ "GREY",
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



format_sag <- function(sag, sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        # sag <- dplyr::mutate(sag, StockKeyLabel=FishStock)
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        # df1 <- left_join(x, y)
        # df1 <- left_join(x, y, by = c("StockKeyLabel", "AssessmentYear"))
        df1 <-as.data.frame(df1)
        
        # df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        # df1 <- subset(df1, select = -c(FishStock))
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        # check <-unique(df1[c("StockKeyLabel", "FisheriesGuild")])
        out <- dplyr::anti_join(df1, check)
}

add_proxyRefPoints <- function(sag_formatted) {
        
        custom_RefPoints <- read.table("data/custom_refpoints_2025.csv",
                sep = ",",
                header = TRUE,
                stringsAsFactors = FALSE
        )

        # 1) One choice per AssessmentKey for each chart (3 = FMSY, 4 = MSYBtrigger)
        cust_choice <- custom_RefPoints %>%
                filter(SAGChartKey %in% c(3, 4)) %>%
                dplyr::group_by(AssessmentKey, SAGChartKey) %>%
                dplyr::summarise(settingValue = dplyr::first(settingValue), .groups = "drop") %>%
                tidyr::pivot_wider(
                        names_from = SAGChartKey,
                        values_from = settingValue,
                        names_prefix = "choice_"
                )
        # -> columns: AssessmentKey, choice_3 (1..4), choice_4 (1..4)
        # 2) Join once, add flags/names, and overwrite values where a proxy is chosen
        sag_final <- sag_formatted %>%
                dplyr::left_join(cust_choice, by = "AssessmentKey") %>%
                # ensure numeric for safety (in case they came as character)
                dplyr::mutate(
                        across(
                                c(FMSY, MSYBtrigger, starts_with("CustomRefPointValue")),
                                ~ suppressWarnings(as.numeric(.x))
                        )
                ) %>%
                dplyr::mutate(
                        # flags + proxy names
                        FMSY_is_proxy = !is.na(choice_3),
                        FMSY_proxy_name = dplyr::case_when(
                                choice_3 == 1 ~ CustomRefPointName1,
                                choice_3 == 2 ~ CustomRefPointName2,
                                choice_3 == 3 ~ CustomRefPointName3,
                                choice_3 == 4 ~ CustomRefPointName4,
                                TRUE ~ NA_character_
                        ),
                        MSYB_is_proxy = !is.na(choice_4),
                        MSYB_proxy_name = dplyr::case_when(
                                choice_4 == 1 ~ CustomRefPointName1,
                                choice_4 == 2 ~ CustomRefPointName2,
                                choice_4 == 3 ~ CustomRefPointName3,
                                choice_4 == 4 ~ CustomRefPointName4,
                                TRUE ~ NA_character_
                        ),

                        # assign chosen custom VALUES; keep official when no choice exists
                        FMSY = dplyr::coalesce(
                                dplyr::case_when(
                                        choice_3 == 1 ~ CustomRefPointValue1,
                                        choice_3 == 2 ~ CustomRefPointValue2,
                                        choice_3 == 3 ~ CustomRefPointValue3,
                                        choice_3 == 4 ~ CustomRefPointValue4,
                                        TRUE ~ NA_real_
                                ),
                                FMSY
                        ),
                        MSYBtrigger = dplyr::coalesce(
                                dplyr::case_when(
                                        choice_4 == 1 ~ CustomRefPointValue1,
                                        choice_4 == 2 ~ CustomRefPointValue2,
                                        choice_4 == 3 ~ CustomRefPointValue3,
                                        choice_4 == 4 ~ CustomRefPointValue4,
                                        TRUE ~ NA_real_
                                ),
                                MSYBtrigger
                        )
                ) %>%
                dplyr::select(-starts_with("choice_"))
        return(sag_final)
}

# stockstatus_CLD_current <- function(x) {
#         browser()
#         df<- dplyr::select(x,Year,
#                            StockKeyLabel,
#                            FisheriesGuild,
#                            FishingPressure,
#                            AssessmentYear,
#                            FMSY,
#                            StockSize,
#                            MSYBtrigger,
#                            Catches,
#                            Landings,
#                            Discards)
#         df$FishingPressure <- as.numeric(df$FishingPressure)
#         df$StockSize <- as.numeric(df$StockSize)
#         df$FMSY <- as.numeric(df$FMSY)
#         df$MSYBtrigger <- as.numeric(df$MSYBtrigger)
#         df$AssessmentYear <- as.numeric(df$AssessmentYear)
#         df$Catches <- as.numeric(df$Catches)
#         df$Landings <- as.numeric(df$Landings)
#         df$Discards <- as.numeric(df$Discards)
#         df$Year <- as.numeric(df$Year)

#         df2 <- dplyr::group_by(df,StockKeyLabel)
#         df2 <- dplyr::filter(df2,Year == AssessmentYear - 1)
#         df2 <- dplyr::mutate(df2,F_FMSY =  ifelse(!is.na(FMSY),
#                                                                 FishingPressure / FMSY,
#                                                                 NA))
#         df2 <- dplyr::select(df2,StockKeyLabel,
#                                                FisheriesGuild,
#                                                F_FMSY,
#                                                Catches,
#                                                Landings,
#                                                Discards,
#                                                FMSY,
#                                                FishingPressure)
#         df3 <- dplyr::group_by(df,StockKeyLabel)
#         df3 <- dplyr::filter(df3, Year %in% c(AssessmentYear, (AssessmentYear - 1)))
#         df3 <- dplyr::mutate(df3, SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
#                                                                         StockSize / MSYBtrigger,
#                                                                         NA))
#         df3 <- dplyr::select(df3, StockKeyLabel,Year,
#                                                FisheriesGuild,
#                                                SSB_MSYBtrigger,
#                                                StockSize,
#                                                MSYBtrigger)
#         check <- unique(df3[c("StockKeyLabel", "Year", "MSYBtrigger")])
#         check <- check[order(-check$Year),]
#         check2 <- check[duplicated(check$StockKeyLabel),]
#         df3 <- dplyr::anti_join(df3,check2)
#         df4 <- dplyr::full_join(df2, df3)
#         df4 <- dplyr::mutate(df4, Status = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
#                                       "GREY",
#                                       if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
#                                               "GREEN",
#                                               "RED",
#                                               "GREY")))
#         df4
# }
stockstatus_CLD_current_proxy <- function(x) {

  # --- Ensure proxy columns exist
  for (nm in c("FMSY_is_proxy","FMSY_proxy_name","MSYB_is_proxy","MSYB_proxy_name")) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  # --- Coerce numerics safely
  num_cols <- c("Year","FishingPressure","StockSize","FMSY","MSYBtrigger",
                "AssessmentYear","Catches","Landings","Discards")
  for (nm in intersect(num_cols, names(x))) {
    x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  }

  # --- Latest assessment year per stock
  x <- x %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::mutate(AY_latest = suppressWarnings(max(AssessmentYear, na.rm = TRUE))) %>%
    dplyr::ungroup()

  # ---------- F side: use Year == AY_latest - 1 ----------
  df_F <- x %>%
    dplyr::filter(Year == AY_latest - 1) %>%
    dplyr::mutate(
      F_FMSY = ifelse(!is.na(FMSY), FishingPressure / FMSY, NA_real_)
    ) %>%
    # if more than one row per stock remains, keep the last (most recent) one
    dplyr::arrange(StockKeyLabel, dplyr::desc(Year)) %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      StockKeyLabel, FisheriesGuild, F_FMSY,
      Catches, Landings, Discards, FMSY, FishingPressure,
      F_proxy = FMSY_is_proxy, F_proxy_name = FMSY_proxy_name
    )

  # ---------- B side: choose latest of AY_latest or AY_latest - 1 with MSYBtrigger ----------
  df_B <- x %>%
    dplyr::filter(Year %in% c(AY_latest, AY_latest - 1)) %>%
    dplyr::arrange(StockKeyLabel, dplyr::desc(Year)) %>%
    dplyr::group_by(StockKeyLabel) %>%
    # keep the latest row that actually has MSYBtrigger
    dplyr::filter(!is.na(MSYBtrigger)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger), StockSize / MSYBtrigger, NA_real_)
    ) %>%
    dplyr::select(
      StockKeyLabel, Year, FisheriesGuild,
      SSB_MSYBtrigger, StockSize, MSYBtrigger,
      B_proxy = MSYB_is_proxy, B_proxy_name = MSYB_proxy_name
    )

  # ---------- Join F and B sides ----------
  df4 <- dplyr::full_join(df_F, df_B, by = c("StockKeyLabel","FisheriesGuild"))

  # ---------- Status classification ----------
  df4 <- df4 %>%
    dplyr::mutate(
      Status = dplyr::case_when(
        is.na(F_FMSY) | is.na(SSB_MSYBtrigger) ~ "GREY",
        F_FMSY < 1 & SSB_MSYBtrigger >= 1       ~ "GREEN",
        TRUE                                    ~ "RED"
      )
    )

  df4
}
### this one works without proxy
# stock_trends <- function(x) {
        
#         x$FishingPressure <- as.numeric(x$FishingPressure)
#         x$StockSize <- as.numeric(x$StockSize)
#         x$FMSY <- as.numeric(x$FMSY)
#         x$MSYBtrigger <- as.numeric(x$MSYBtrigger)
#         x$Year <- as.numeric(x$Year)

#         df <- dplyr::mutate(x,
#                 FMEAN = mean(FishingPressure, na.rm = TRUE),
#                 SSBMEAN = mean(StockSize, na.rm = TRUE),
#                 FMEAN = ifelse(!grepl("F|F(ages 3-6)", FishingPressureDescription), NA, FMEAN),
#                 SSBMEAN = ifelse(!grepl("StockSize", StockSizeDescription), NA, SSBMEAN)
#         )

#         df <- dplyr::mutate(df,
#                 F_FMSY = ifelse(!is.na(FMSY), FishingPressure / FMSY, NA),
#                 SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger), StockSize / MSYBtrigger, NA),
#                 F_FMEAN = ifelse(!is.na(FMEAN), FishingPressure / FMEAN, NA),
#                 SSB_SSBMEAN = ifelse(!is.na(SSBMEAN), StockSize / SSBMEAN, NA)
#         )

#         df <- df %>%
#                 dplyr::select(Year, 
#                                 StockKeyLabel, 
#                                 FisheriesGuild, 
#                                 F_FMSY, 
#                                 SSB_MSYBtrigger, 
#                                 F_FMEAN, 
#                                 SSB_SSBMEAN)

#         df2 <- tidyr::gather(df, Metric, Value, -Year, -StockKeyLabel, -FisheriesGuild) %>%
#                 dplyr::filter(!is.na(Year))

#         df3 <- df2 %>%
#                 dplyr::group_by(StockKeyLabel, FisheriesGuild, Metric, Year) %>%
#                 dplyr::summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
#                 dplyr::filter(!is.na(Value))

#         means <- df2 %>%
#                 dplyr::group_by(FisheriesGuild, Metric, Year) %>%
#                 dplyr::summarize(
#                         non_na_n = sum(!is.na(Value)),
#                         Value = ifelse(non_na_n >= 2, mean(Value, na.rm = TRUE), NA_real_),
#                         StockKeyLabel = "Mean",
#                         .groups = "drop"
#                 ) %>%
#                 dplyr::filter(!is.na(Value)) %>%
#                 dplyr::select(FisheriesGuild, StockKeyLabel, Year, Metric, Value)


#         df4 <- dplyr::bind_rows(df3, means) %>%
#                 dplyr::distinct(.keep_all = TRUE)
#         # check which stocks have max year = 2026

#         return(df4)
# }

#### this works with proxy
stock_trends_proxy <- function(x) {

  # --- Ensure proxy columns exist (in case some sources don't carry them)
  for (nm in c("FMSY_is_proxy", "FMSY_proxy_name", "MSYB_is_proxy", "MSYB_proxy_name")) {
    if (!nm %in% base::names(x)) x[[nm]] <- NA
  }

  # --- Coerce numerics
  x <- dplyr::mutate(
    .data = x,
    FishingPressure = base::as.numeric(FishingPressure),
    StockSize       = base::as.numeric(StockSize),
    FMSY            = base::as.numeric(FMSY),
    MSYBtrigger     = base::as.numeric(MSYBtrigger),
    Year            = base::as.numeric(Year)
  )

  # --- Make per-stock proxy map (constant across years)
  proxy_map <- x %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::summarise(
      F_proxy = base::any(FMSY_is_proxy %in% TRUE, na.rm = TRUE),
      F_name  = dplyr::first(FMSY_proxy_name[!base::is.na(FMSY_proxy_name)],
                             default = NA_character_),
      B_proxy = base::any(MSYB_is_proxy %in% TRUE, na.rm = TRUE),
      B_name  = dplyr::first(MSYB_proxy_name[!base::is.na(MSYB_proxy_name)],
                             default = NA_character_),
      .groups = "drop"
    )

  # --- Means used for normalization (only when descriptors match)
  df <- dplyr::mutate(
    .data = x,
    FMEAN  = base::mean(FishingPressure, na.rm = TRUE),
    SSBMEAN = base::mean(StockSize,       na.rm = TRUE),
    FMEAN  = ifelse(!base::grepl("F|F(ages 3-6)", FishingPressureDescription), NA, FMEAN),
    SSBMEAN = ifelse(!base::grepl("StockSize",     StockSizeDescription),       NA, SSBMEAN)
  )

  # --- Ratios
  df <- dplyr::mutate(
    .data = df,
    F_FMSY           = ifelse(!base::is.na(FMSY),        FishingPressure / FMSY,        NA),
    SSB_MSYBtrigger  = ifelse(!base::is.na(MSYBtrigger), StockSize / MSYBtrigger,       NA),
    F_FMEAN          = ifelse(!base::is.na(FMEAN),       FishingPressure / FMEAN,       NA),
    SSB_SSBMEAN      = ifelse(!base::is.na(SSBMEAN),     StockSize / SSBMEAN,           NA)
  )

  # --- Keep only needed columns
  df <- dplyr::select(
    df, Year, StockKeyLabel, FisheriesGuild,
    F_FMSY, SSB_MSYBtrigger, F_FMEAN, SSB_SSBMEAN
  )

  # --- Long form
  df_long <- tidyr::pivot_longer(
    data = df,
    cols = c("F_FMSY", "SSB_MSYBtrigger", "F_FMEAN", "SSB_SSBMEAN"),
    names_to = "Metric", values_to = "Value"
  ) %>%
    dplyr::filter(!base::is.na(Year))

  # --- Average duplicates at (Stock, Guild, Metric, Year)
  df3 <- df_long %>%
    dplyr::group_by(StockKeyLabel, FisheriesGuild, Metric, Year) %>%
    dplyr::summarise(Value = base::mean(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!base::is.na(Value))

  # --- Guild mean per year (only when >=2 non-NA stocks)
  means <- df_long %>%
    dplyr::group_by(FisheriesGuild, Metric, Year) %>%
    dplyr::summarise(
      non_na_n = base::sum(!base::is.na(Value)),
      Value    = ifelse(non_na_n >= 2, base::mean(Value, na.rm = TRUE), NA_real_),
      StockKeyLabel = "Mean",
      .groups = "drop"
    ) %>%
    dplyr::filter(!base::is.na(Value)) %>%
    dplyr::select(FisheriesGuild, StockKeyLabel, Year, Metric, Value)

  # --- Bind stocks + means
  out <- dplyr::bind_rows(df3, means) %>%
    dplyr::distinct(.keep_all = TRUE)

  # --- Attach stock-level proxy info, then derive row-level proxy columns by metric
  out <- out %>%
  dplyr::left_join(proxy_map, by = "StockKeyLabel") %>%
  dplyr::mutate(
    # explicit row-level (metric-specific) fields
    is_proxy_metric = dplyr::case_when(
      Metric == "F_FMSY"          ~ F_proxy,
      Metric == "SSB_MSYBtrigger" ~ B_proxy,
      TRUE                        ~ FALSE
    ),
    proxy_name_metric = dplyr::case_when(
      Metric == "F_FMSY"          ~ F_name,
      Metric == "SSB_MSYBtrigger" ~ B_name,
      TRUE                        ~ NA_character_
    ),
    # keep your original names so the plotting code keeps working
    Proxy_is_proxy = is_proxy_metric,
    Proxy_name     = proxy_name_metric
  )

  return(out)
}

match_stockcode_to_illustration <- function(StockKeyLabel, df) {
        sapply(StockKeyLabel, function(key) {
                temp <- list.files("inst/app/www/fish", pattern = substr(key, 1, 3))
                if (length(temp) == 0) "fish.png" else temp[1]
        })
}





################### Plotting functions ##########################################################


plot_status_prop_pies <- function(
  df,
  return_data = FALSE,
  width_px = 800,            # pass session$clientData[["output_<id>_width"]]
  min_base = 11,
  max_base = 18
) {
  # --- Responsive sizes
  base_size    <- max(min_base, min(max_base, round(width_px / 50)))
  caption_size <- max(8, base_size - 2)
  label_size   <- max(3, min(6, round(base_size / 3)))

  cap_lab <- ggplot2::labs(
    title = NULL, x = NULL, y = NULL,
    caption = paste0("ICES Stock Assessment Database, ",
                     format(Sys.Date(), "%d-%b-%y"),
                     ". ICES, Copenhagen")
  )

  colList <- c(
    "GREEN" = "#00B26D", "GREY" = "#d3d3d3", "ORANGE" = "#ff7f00",
    "RED" = "#d93b1c", "qual_RED" = "#d93b1c", "qual_GREEN" = "#00B26D",
    "UNDEFINED" = "#006aff"
  )

  # --- Prep
  df_stock <- dplyr::select(
    df, StockKeyLabel, FisheriesGuild, lineDescription, FishingPressure, StockSize
  )
  df_stock <- tidyr::gather(df_stock, Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)

  df2 <- df_stock |>
    dplyr::group_by(FisheriesGuild, lineDescription, Variable, Colour) |>
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop") |>
    tidyr::spread(Colour, COUNT)

  df2[is.na(df2)] <- 0

  # Totals row
  df3 <- df2 |>
    dplyr::select(-FisheriesGuild) |>
    dplyr::group_by(lineDescription, Variable) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), sum), .groups = "drop") |>
    dplyr::mutate(FisheriesGuild = "total")

  df2 <- dplyr::bind_rows(df2, df3)

  # Base R rename + compact regime names
  df2$Variable <- as.character(df2$Variable)
  df2$Variable[df2$Variable == "FishingPressure"] <- "Fishing pressure"
  df2$Variable[df2$Variable == "StockSize"]       <- "Stock size"
  df2$lineDescription <- gsub("Maximum sustainable yield", "MSY", df2$lineDescription)
  df2$lineDescription <- gsub("Precautionary approach",  "PA",  df2$lineDescription)

  # Two-line facet header
  df2$header <- paste0(df2$Variable, "\n", df2$lineDescription)

  # Long format for colours to show
  df2 <- tidyr::gather(df2, colour, value, GREEN:RED, factor_key = TRUE) |>
    dplyr::filter(value > 0)

  # Common radius across columns
  tot <- df2 |>
    dplyr::filter(FisheriesGuild == "total") |>
    dplyr::group_by(header) |>
    dplyr::summarise(tot = sum(value), .groups = "drop")
  overall_max <- max(tot$tot, na.rm = TRUE)

  df2 <- df2 |>
    dplyr::group_by(FisheriesGuild, header) |>
    dplyr::mutate(sum = sum(value), fraction = value * overall_max / sum) |>
    dplyr::ungroup()

  # Facet orders (keep only existing)
  wanted_headers <- c("Fishing pressure\nMSY","Stock size\nMSY",
                      "Fishing pressure\nPA", "Stock size\nPA")
  present_headers <- intersect(wanted_headers, unique(df2$header))
  df2$header <- factor(df2$header, levels = present_headers)

  df2$FisheriesGuild <- factor(
    tolower(df2$FisheriesGuild),
    levels = c("total","benthic","demersal","pelagic","crustacean","elasmobranch")
  )

  # --- Dynamic spacing & margins (based on width and # of columns)
  n_cols <- max(1L, length(unique(df2$header)))
  # ~4% of per-column pixel width, converted to points (≈ px since on-screen),
  # clamped to 10–72 pt
  panel_spacing_x_pt <- max(10, min(72, round((width_px / n_cols) * 0.04)))
  # extra padding around strip text and plot edges
  strip_lr_pad_pt <- max(8, round(base_size * 1.5))
  plot_lr_margin_pt <- max(12, round(base_size * 1.6))

  # --- Plot
  p1 <- ggplot2::ggplot(df2, ggplot2::aes(x = "", y = fraction, fill = colour)) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::geom_text(
      ggplot2::aes(label = value),
      position = ggplot2::position_stack(vjust = 0.5),
      size = label_size
    ) +
    ggplot2::scale_fill_manual(values = colList) +
    ggplot2::coord_polar(theta = "y", direction = 1) +
    ggplot2::facet_grid(FisheriesGuild ~ header) +
    cap_lab +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "none",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 4, r = strip_lr_pad_pt, b = 4, l = strip_lr_pad_pt, unit = "pt"),
        lineheight = 1.05
      ),
      panel.spacing.x = grid::unit(panel_spacing_x_pt, "pt"),
      plot.caption = ggplot2::element_text(size = caption_size, hjust = 0),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(8, plot_lr_margin_pt, 26, plot_lr_margin_pt, unit = "pt")
    )

  if (isTRUE(return_data)) df2 else p1
}


plot_GES_pies <- function(x, y, return_data = FALSE, width_px = 800) {
  # --- Responsive sizes
  base_size        <- max(14, min(20, round(width_px / 50)))
  caption_size     <- max(8, base_size - 2)
  value_label_size <- max(4, min(9, round(base_size / 3.0)))   # a bit larger than before
  total_label_size <- max(3, min(7, round(base_size / 2.9)))

  cap_lab <- ggplot2::labs(
    title = NULL, x = NULL, y = NULL,
    caption = paste0("ICES Stock Assessment Database, ",
                     format(Sys.Date(), "%d-%b-%y"),
                     ". ICES, Copenhagen")
  )

  colList <- c(
    "GREEN" = "#00B26D",
    "GREY" = "#d3d3d3",
    "ORANGE" = "#ff7f00",
    "RED" = "#d93b1c",
    "qual_RED" = "#d93b5c",
    "qual_GREEN" = "#00B28F"
  )

  df_stock <- dplyr::filter(x, lineDescription == "Maximum sustainable yield") |>
    dplyr::select(StockKeyLabel, FishingPressure, StockSize) |>
    tidyr::gather(Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)

  df2 <- df_stock |>
    dplyr::group_by(Variable, Colour) |>
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop") |>
    tidyr::spread(Colour, COUNT)
  df2[is.na(df2)] <- 0

  df3 <- dplyr::filter(y, StockKeyLabel %in% df_stock$StockKeyLabel) |>
    dplyr::mutate(CATCH = ifelse(is.na(Catches) & !is.na(Landings), Landings, Catches)) |>
    dplyr::select(StockKeyLabel, CATCH)

  df4 <- dplyr::left_join(df_stock, df3); df4[is.na(df4)] <- 0
  df4 <- df4 |>
    dplyr::group_by(Variable, Colour) |>
    dplyr::summarise(CATCH = sum(CATCH), .groups = "drop") |>
    tidyr::spread(Colour, CATCH)

  df4 <- tidyr::gather(df4, Color, Catch, GREEN:RED, factor_key = TRUE)
  df2 <- tidyr::gather(df2, Color, Stocks, GREEN:RED, factor_key = TRUE)

  df5 <- merge(df2, df4)
  df5[is.na(df5)] <- 0

  tot    <- sum(df5$Catch)  / 2
  stocks <- sum(df5$Stocks) / 2
  df5    <- tidyr::gather(df5, Metric, Value, Stocks:Catch)
  df5    <- dplyr::group_by(df5, Metric) |>
            dplyr::mutate(sum = sum(Value) / 2)

  # keep only catch for plotting
  df5 <- dplyr::filter(df5, Metric != "Stocks")

  # fraction used by polar
  df5$fraction <- df5$Value

  # nicer labels
  df5$Variable <- plyr::revalue(df5$Variable,
                                c("FishingPressure" = "Fishing Pressure",
                                  "StockSize"       = "Stock Size"))
  df5$Metric   <- plyr::revalue(df5$Metric,
                                c("Stocks" = "Number of stocks",
                                  "Catch"  = "Proportion of catch \n(thousand tonnes)"))

  # Display values (000 t for catch)
  df5$Value2 <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)",
                       df5$Value / 1000, df5$Value)
  df5$sum2   <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)",
                       df5$sum / 1000, df5$sum)

  # --- Percent per pie (within each facet)
  df5 <- df5 |>
    dplyr::group_by(Metric, Variable) |>
    dplyr::mutate(
      facet_sum = sum(Value, na.rm = TRUE),
      pct = ifelse(facet_sum > 0, 100 * Value / facet_sum, NA_real_)
    ) |>
    dplyr::ungroup()

  # tidy up values for display
  df5$Value2 <- as.integer(df5$Value2)
  df5$sum2   <- as.integer(df5$sum2)
  df5 <- dplyr::filter(df5, Value2 > 0)
  df5$pct_lab <- sprintf("%.1f%%", df5$pct)

  p1 <- ggplot2::ggplot(df5, ggplot2::aes(x = "", y = fraction, fill = Color)) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    # Single label: value on first line, percent on second line
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(Value2, "\n", pct_lab)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = value_label_size,
      lineheight = 0.95
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("total = ", sum2), x = 0, y = 0),
      size = total_label_size
    ) +
    ggplot2::scale_fill_manual(values = colList) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::facet_grid(Metric ~ Variable) +
    cap_lab +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "none",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = caption_size, hjust = 0),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(8, 10, 26, 10, unit = "pt")
    )

  if (isTRUE(return_data)) {
    df5 <- subset(df5, select = -c(facet_sum, pct, pct_lab))
    df5
  } else {
    p1
  }
}

# plot_stock_trends <- function(x, guild, return_data = FALSE, ecoregion = NULL) {
#         # --- Filter for selected guild
#         df <- dplyr::filter(x, FisheriesGuild == guild)
#         browser()
#         if (nrow(df) == 0) {
#                 return(
#                         plotly::plot_ly() %>%
#                                 plotly::layout(
#                                         xaxis = list(visible = FALSE),
#                                         yaxis = list(visible = FALSE),
#                                         annotations = list(
#                                                 list(
#                                                         text = paste0("No data available for guild: ", guild),
#                                                         xref = "paper",
#                                                         yref = "paper",
#                                                         x = 0.5,
#                                                         y = 0.5, # center of plot
#                                                         showarrow = FALSE,
#                                                         font = list(size = 20)
#                                                 )
#                                         )
#                                 )
#                 )
#         }

#         # --- Dynamic color palette for all stocks
#         adj_names <- sort(setdiff(unique(df$StockKeyLabel), "Mean"))
#         values <- grDevices::hcl.colors(length(adj_names), palette = "Temps")
#         names(values) <- adj_names
#         values <- c(values, c(MEAN = "black"))
        
       
#         # --- Keep only the two metrics of interest and rename
#         df <- df %>%
#                 dplyr::filter(Metric %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
#                 dplyr::mutate(Metric = dplyr::recode(
#                         Metric,
#                         "F_FMSY"          = "F/F<sub>MSY</sub>",
#                         "SSB_MSYBtrigger" = "SSB/MSY B<sub>trigger</sub>"
#                 ))

#         # --- Split mean vs stocks
#         mean_df <- dplyr::filter(df, StockKeyLabel == "Mean")
        
#         # --- Remove any existing mean for the last year, suggestion of ADGFO 
#         last_year_FMSY <- max(mean_df$Year[mean_df$Metric == "F/F<sub>MSY</sub>"], na.rm = TRUE)
#         last_year_Btrigger <- max(mean_df$Year[mean_df$Metric == "SSB/MSY B<sub>trigger</sub>"], na.rm = TRUE)
#         idx_FMSY <- mean_df$Metric == "F/F<sub>MSY</sub>" & mean_df$Year == last_year_FMSY
#         idx_Btrigger <- mean_df$Metric == "SSB/MSY B<sub>trigger</sub>" & mean_df$Year == last_year_Btrigger        
#         mean_df$Value[idx_FMSY] <- NA_real_
#         mean_df$Value[idx_Btrigger] <- NA_real_

#         df2 <- dplyr::filter(df, StockKeyLabel != "Mean")

#         # unique group ID per render (prevents stale highlights)
#         group_id <- paste0("stocks_", round(as.numeric(Sys.time()) * 1000))


#         # --- SharedData for all stock traces (used across both subplots)
#         sd <- crosstalk::SharedData$new(df2, key = ~StockKeyLabel, group = group_id)

#         # --- Helper to build one panel using a filter transform
#         make_panel <- function(metric_label, yaxis_title, show_legend = TRUE) {
#                 plotly::plot_ly(
#                         data = sd,
#                         x = ~Year,
#                         y = ~Value,
#                         color = ~StockKeyLabel,
#                         colors = values,
#                         type = "scatter",
#                         mode = "lines",
#                         name = ~StockKeyLabel,
#                         legendgroup = ~StockKeyLabel,
#                         ids = ~StockKeyLabel,
#                         transforms = list(list(
#                                 type      = "filter",
#                                 target    = ~Metric,
#                                 operation = "=",
#                                 value     = metric_label
#                         )),
#                         showlegend = show_legend,
#                         line = list(width = 3)
#                 ) %>%
#                         plotly::add_trace(
#                                 data = dplyr::filter(mean_df, Metric == metric_label),
#                                 x = ~Year,
#                                 y = ~Value,
#                                 name = "Mean",
#                                 type = "scatter",
#                                 mode = "lines",
#                                 line = list(color = "black", width = 5),
#                                 showlegend = show_legend,
#                                 inherit = FALSE
#                         ) %>%
#                         plotly::layout(
#                                 yaxis = list(
#                                         title = yaxis_title,
#                                         titlefont = list(size = 16),
#                                         tickfont = list(size = 14),
#                                         zeroline = TRUE,
#                                         zerolinecolor = "black",
#                                         zerolinewidth = 2
#                                 ),
#                                 shapes = list(
#                                         # horizontal reference line at y = 1
#                                         list(
#                                                 type = "line",
#                                                 x0 = safe_min(df$Year, 0),
#                                                 x1 = safe_max(df$Year, 1),
#                                                 y0 = 1,
#                                                 y1 = 1,
#                                                 line = list(color = "#000000", width = 1)
#                                         ),
#                                         # black border around the plot area
#                                         list(
#                                                 type = "rect",
#                                                 xref = "paper",
#                                                 yref = "paper",
#                                                 x0 = 0,
#                                                 x1 = 1,
#                                                 y0 = 0,
#                                                 y1 = 1,
#                                                 line = list(color = "black", width = 1),
#                                                 fillcolor = "rgba(0,0,0,0)"
#                                         )
#                                 )
#                         )
#         }

#         # --- Build both panels
#         plot1 <- make_panel("F/F<sub>MSY</sub>", "F/F<sub>MSY</sub>", show_legend = TRUE)
#         plot2 <- make_panel("SSB/MSY B<sub>trigger</sub>", "SSB/MSY B<sub>trigger</sub>", show_legend = FALSE)

#         # --- Combine panels and enable cross-highlighting
#         final_plot <- plotly::subplot(plot1, plot2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
#                 plotly::layout(
#                         xaxis = list(
#                                 title = "Year",
#                                 titlefont = list(size = 16),
#                                 tickfont = list(size = 14)
#                         ),
#                         margin = list(b = 100, r = 50),
#                         legend = list(
#                                 title = list(text = "Stock name", font = list(size = 16)),
#                                 orientation = "h",
#                                 x = 0.5, y = 1.05, # center above the plot
#                                 xanchor = "center",
#                                 yanchor = "bottom",
#                                 font = list(size = 16)
#                         ),
#                         annotations = list(
#                                 list(
#                                         x = 1, y = -0.15, # relative to plotting area (0–1, left–right / bottom–top)
#                                         xref = "paper",
#                                         yref = "paper",
#                                         text = paste0("ICES Stock Assessment Database, ", format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
#                                         showarrow = FALSE,
#                                         xanchor = "right",
#                                         yanchor = "bottom"
#                                 ),
#                                 list(
#                                         text = paste0("Status trends: ", guild, " (", ecoregion, ")"),
#                                         x = 0.01, y = 0.99, # relative to plotting area (0–1, left–right / bottom–top)
#                                         xref = "paper", yref = "paper",
#                                         showarrow = FALSE,
#                                         xanchor = "left",
#                                         yanchor = "top",
#                                         font = list(size = 18, color = "black")
#                                 )
#                         )
#                 ) %>%
#                 plotly::highlight(
#                         on = "plotly_click",
#                         off = "plotly_doubleclick",
#                         opacityDim = 0.4, # dims non-selected lines in both panels
#                         selected = plotly::attrs_selected(line = list(width = 5))
#                 ) %>% 
#                 plotly::config(
#                         responsive = TRUE,
#                         toImageButtonOptions = list(
#                                 filename = paste0(ecoregion, "_StatusTrends_", guild, "_", format(Sys.Date(), "%d-%b-%y")),
#                                 format   = "png",
#                                 scale    = 3
#                                 # width  = 1600,
#                                 # height = 900
#                         )
#                 )

#         if (return_data) {
#                 return(df)
#         } else {
#                 return(final_plot)
#         }
# }
plot_stock_trends <- function(x, guild, return_data = FALSE, ecoregion = NULL) {
  # --- helpers
  safe_min <- function(v, pad = 0) {
    m <- suppressWarnings(base::min(v, na.rm = TRUE))
    if (is.infinite(m)) 0 else m - pad
  }
  safe_max <- function(v, pad = 0) {
    m <- suppressWarnings(base::max(v, na.rm = TRUE))
    if (is.infinite(m)) 1 else m + pad
  }
  rand_id <- function(prefix = "stockkey_") {
    paste0(prefix, paste(sample(c(letters, LETTERS, 0:9), 12, TRUE), collapse = ""))
  }

  # --- Filter for selected guild
  df <- dplyr::filter(x, FisheriesGuild == guild)
  if (nrow(df) == 0) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = paste0("No data available for guild: ", guild),
            xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            showarrow = FALSE, font = list(size = 20)
          ))
        )
    )
  }

  # --- Colors (exclude Mean)
  adj_names <- base::sort(base::setdiff(base::unique(df$StockKeyLabel), "Mean"))
  values <- grDevices::hcl.colors(length(adj_names), palette = "Temps")
  base::names(values) <- adj_names
  values <- c(values, c(MEAN = "black"))   # not used directly but harmless

  # --- Keep metrics of interest + pretty labels (retain raw for filtering)
  metric_map <- c("F_FMSY" = "F/F<sub>MSY</sub>",
                  "SSB_MSYBtrigger" = "SSB/MSY B<sub>trigger</sub>")
  df <- df %>%
    dplyr::filter(Metric %in% base::names(metric_map)) %>%
    dplyr::mutate(MetricLabel = dplyr::recode(Metric, !!!metric_map))

  # --- Mean rows + null last-year mean
  mean_df <- dplyr::filter(df, StockKeyLabel == "Mean")
  last_year_F <- suppressWarnings(base::max(mean_df$Year[mean_df$Metric == "F_FMSY"], na.rm = TRUE))
  last_year_B <- suppressWarnings(base::max(mean_df$Year[mean_df$Metric == "SSB_MSYBtrigger"], na.rm = TRUE))
  if (is.finite(last_year_F)) {
    mean_df$Value[mean_df$Metric == "F_FMSY" & mean_df$Year == last_year_F] <- NA_real_
  }
  if (is.finite(last_year_B)) {
    mean_df$Value[mean_df$Metric == "SSB_MSYBtrigger" & mean_df$Year == last_year_B] <- NA_real_
  }

  # --- Non-mean rows + hover text (proxy-aware)
  df2 <- dplyr::filter(df, StockKeyLabel != "Mean") %>%
    dplyr::mutate(
      hover = dplyr::if_else(
        Proxy_is_proxy & !base::is.na(Proxy_name),
        paste0("Stock: ", StockKeyLabel,
               "<br>Year: ", Year,
               "<br>", MetricLabel, ": ", base::signif(Value, 4),
               "<br>Proxy: ", Proxy_name),
        paste0("Stock: ", StockKeyLabel,
               "<br>Year: ", Year,
               "<br>", MetricLabel, ": ", base::signif(Value, 4))
      )
    )

  # --- Determine which stocks appear in which panel
  stocks_top    <- unique(dplyr::filter(df2, Metric == "F_FMSY")$StockKeyLabel)
  stocks_bottom <- unique(dplyr::filter(df2, Metric == "SSB_MSYBtrigger")$StockKeyLabel)
  only_bottom   <- base::setdiff(stocks_bottom, stocks_top)

  # --- Fresh Crosstalk group per render (prevents stale linkage)
  ct_group <- rand_id()

  # --- Panel builder (legend shown only for chosen stocks; grouped across panels)
  make_panel <- function(metric_raw, yaxis_title, legend_stocks = character(), show_mean_in_legend = TRUE) {
    panel <- dplyr::filter(df2, Metric == metric_raw)

    # Split into non-proxy vs proxy (constant per stock & metric)
    np <- panel %>%
      dplyr::group_by(StockKeyLabel) %>%
      dplyr::filter(!base::any(Proxy_is_proxy, na.rm = TRUE)) %>%
      dplyr::ungroup()

    pr <- panel %>%
      dplyr::group_by(StockKeyLabel) %>%
      dplyr::filter(base::any(Proxy_is_proxy, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Further split by whether we want legend entries here
    np_on  <- dplyr::filter(np, StockKeyLabel %in% legend_stocks)
    np_off <- dplyr::filter(np, !StockKeyLabel %in% legend_stocks)
    pr_on  <- dplyr::filter(pr, StockKeyLabel %in% legend_stocks)
    pr_off <- dplyr::filter(pr, !StockKeyLabel %in% legend_stocks)

    # Keyed data for cross-panel highlight
    hk_np_on  <- plotly::highlight_key(np_on,  ~StockKeyLabel, group = ct_group)
    hk_np_off <- plotly::highlight_key(np_off, ~StockKeyLabel, group = ct_group)
    hk_pr_on  <- plotly::highlight_key(pr_on,  ~StockKeyLabel, group = ct_group)
    hk_pr_off <- plotly::highlight_key(pr_off, ~StockKeyLabel, group = ct_group)

    p <- plotly::plot_ly() %>%
      # Non-proxy (solid) — legend ON
      plotly::add_lines(
        data = hk_np_on,
        x = ~Year, y = ~Value,
        split = ~StockKeyLabel,
        color = ~StockKeyLabel, colors = values,
        legendgroup = ~StockKeyLabel, name = ~StockKeyLabel,
        line = list(width = 3, dash = "solid"),
        unselected = list(line = list(opacity = 0.3)),
        text = ~hover, hovertemplate = "%{text}<extra></extra>",
        showlegend = TRUE
      ) %>%
      # Non-proxy (solid) — legend OFF
      plotly::add_lines(
        data = hk_np_off,
        x = ~Year, y = ~Value,
        split = ~StockKeyLabel,
        color = ~StockKeyLabel, colors = values,
        legendgroup = ~StockKeyLabel, name = ~StockKeyLabel,
        line = list(width = 3, dash = "solid"),
        unselected = list(line = list(opacity = 0.3)),
        text = ~hover, hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE
      ) %>%
      # Proxy (dotted) — legend ON
      plotly::add_lines(
        data = hk_pr_on,
        x = ~Year, y = ~Value,
        split = ~StockKeyLabel,
        color = ~StockKeyLabel, colors = values,
        legendgroup = ~StockKeyLabel, name = ~StockKeyLabel,
        line = list(width = 3, dash = "dot"),
        unselected = list(line = list(opacity = 0.3)),
        text = ~hover, hovertemplate = "%{text}<extra></extra>",
        showlegend = TRUE
      ) %>%
      # Proxy (dotted) — legend OFF
      plotly::add_lines(
        data = hk_pr_off,
        x = ~Year, y = ~Value,
        split = ~StockKeyLabel,
        color = ~StockKeyLabel, colors = values,
        legendgroup = ~StockKeyLabel, name = ~StockKeyLabel,
        line = list(width = 3, dash = "dot"),
        unselected = list(line = list(opacity = 0.3)),
        text = ~hover, hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE
      ) %>%
      # Mean line (legend only once if desired)
      plotly::add_lines(
        data = dplyr::filter(mean_df, Metric == metric_raw),
        x = ~Year, y = ~Value,
        name = "Mean", legendgroup = "Mean",
        line = list(color = "black", width = 5),
        showlegend = show_mean_in_legend,
        inherit = FALSE
      ) %>%
      plotly::layout(
        yaxis = list(
          title = yaxis_title,
          titlefont = list(size = 16),
          tickfont = list(size = 14),
          zeroline = TRUE, zerolinecolor = "black", zerolinewidth = 2
        ),
        shapes = list(
          list(
            type = "line",
            x0 = safe_min(df$Year, 0),
            x1 = safe_max(df$Year, 1),
            y0 = 1, y1 = 1,
            line = list(color = "#000000", width = 1)
          ),
          list(
            type = "rect", xref = "paper", yref = "paper",
            x0 = 0, x1 = 1, y0 = 0, y1 = 1,
            line = list(color = "black", width = 1),
            fillcolor = "rgba(0,0,0,0)"
          )
        )
      )

    p
  }

  # --- Build both panels
  plot1 <- make_panel(
    "F_FMSY",
    metric_map[["F_FMSY"]],
    legend_stocks = stocks_top,          # legend for top-panel stocks
    show_mean_in_legend = TRUE
  )
  plot2 <- make_panel(
    "SSB_MSYBtrigger",
    metric_map[["SSB_MSYBtrigger"]],
    legend_stocks = only_bottom,         # legend for stocks only in lower panel
    show_mean_in_legend = FALSE          # avoid duplicate 'Mean'
  )

  # --- Combine + highlight without recoloring
  final_plot <- plotly::subplot(plot1, plot2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
    plotly::layout(
      xaxis = list(title = "Year", titlefont = list(size = 16), tickfont = list(size = 14)),
      margin = list(b = 100, r = 50),
      legend = list(
        title = list(text = "Stock name", font = list(size = 16)),
        orientation = "h",
        x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom",
        font = list(size = 16)
      ),
      annotations = list(
        list(
          x = 1, y = -0.15, xref = "paper", yref = "paper",
          text = paste0("ICES Stock Assessment Database, ", base::format(base::Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
          showarrow = FALSE, xanchor = "right", yanchor = "bottom"
        ),
        list(
          text = paste0("Status trends: ", guild, " (", ecoregion, ")"),
          x = 0.01, y = 0.99, xref = "paper", yref = "paper",
          showarrow = FALSE, xanchor = "left", yanchor = "top",
          font = list(size = 18, color = "black")
        )
      )
    ) %>%
    plotly::highlight(
      on = "plotly_click",
      off = "plotly_doubleclick",
      # dynamic = FALSE (default) -> no brush widget
      color = NULL,               # keep original trace color on selection
      opacityDim = 0.3,
      selected = plotly::attrs_selected(
        opacity = 1,
        line = list(width = 6)
      )
    ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = paste0(ecoregion, "_StatusTrends_", guild, "_", base::format(base::Sys.Date(), "%d-%b-%y")),
        format   = "png",
        scale    = 3
      )
    )

  if (return_data) df else final_plot
}






# plot_CLD_bar_app <- function(x, guild, return_data = FALSE) {
#   # --- Filter by guild
#   if (identical(guild, "All")) {
#     df <- x
#   } else {
#     df <- dplyr::filter(x, FisheriesGuild %in% guild)
#   }

#   # --- Build 'total' per stock (max of Catches/Landings across time)
#   df <- df %>%
#     dplyr::group_by(StockKeyLabel) %>%
#     dplyr::mutate(
#       total = ifelse(
#         all(is.na(Catches) & is.na(Landings)),
#         NA,
#         max(Catches, Landings, na.rm = TRUE)
#       )
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(!is.na(total))

#   # Order stocks by total (smallest at bottom after coord_flip)
#   df <- dplyr::mutate(df, StockKeyLabel = forcats::fct_reorder(StockKeyLabel, total))

#   # --- Caption
#   cap_lab <- ggplot2::labs(caption = paste0("ICES Stock Assessment Database, ",
#                      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"))

#   # --- Plot
#   p <- ggplot2::ggplot(df, ggplot2::aes(x = StockKeyLabel, y = Catches / 1000)) +
#     ggplot2::geom_segment(
#       ggplot2::aes(xend = StockKeyLabel, yend = 0, color = Status),
#       size = 2, na.rm = TRUE
#     ) +
#     ggplot2::geom_segment(
#       ggplot2::aes(y = Landings / 1000, xend = StockKeyLabel, yend = 0, color = Status),
#       size = 2, na.rm = TRUE
#     ) +
#     ggplot2::geom_point(
#       ggplot2::aes(y = Catches / 1000, fill = Status),
#       color = "grey50", shape = 24, size = 7, alpha = 0.8, na.rm = TRUE
#     ) +
#     ggplot2::geom_point(
#       ggplot2::aes(y = Landings / 1000, fill = Status),
#       color = "grey50", shape = 21, size = 7, alpha = 0.8, na.rm = TRUE
#     ) +
#     ggplot2::scale_fill_manual(values = c(
#       "GREEN" = "#4daf4a",
#       "RED"   = "#e41a1c",
#       "GREY"  = "#d3d3d3"
#     )) +
#     ggplot2::scale_color_manual(values = c(
#       "GREEN" = "#4daf4a",
#       "RED"   = "#e41a1c",
#       "GREY"  = "#d3d3d3"
#     )) +
#     ggplot2::coord_equal() +
#     ggplot2::coord_flip() +
#     ggplot2::theme_bw(base_size = 20) +
#     ggplot2::labs(x = expression("Stock name"), y = expression("Catch and Landings (thousand tonnes)")) +
#     ggplot2::theme(
#       legend.position      = "none",
#       plot.caption         = ggplot2::element_text(size = 12),
#       panel.grid.minor     = ggplot2::element_blank(),
#       panel.grid.major.y   = ggplot2::element_blank(),
#       panel.grid.major.x   = ggplot2::element_line(size = 0.1, color = "grey80")
#     ) +
#     cap_lab

#   if (isTRUE(return_data)) df else p
# }

plot_CLD_bar_app <- function(x, guild, return_data = FALSE) {
  # --- Filter by guild
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

  # --- Ensure proxy flags exist
  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE

  # --- Build 'total' per stock (max of Catches/Landings across time)
  df <- df %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::mutate(
      total = ifelse(all(is.na(Catches) & is.na(Landings)), NA,
                     max(Catches, Landings, na.rm = TRUE))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(total))

  # Order stocks by total (smallest at bottom after coord_flip)
  df <- dplyr::mutate(df, StockKeyLabel = forcats::fct_reorder(StockKeyLabel, total))

  # Flag if any reference point is proxy
  df <- df %>% dplyr::mutate(ProxyFlag = (F_proxy %in% TRUE) | (B_proxy %in% TRUE))

  # Status palette
  status_pal <- c(GREEN = "#4daf4a", RED = "#e41a1c", GREY = "#d3d3d3")

  # Caption
  cap_lab <- ggplot2::labs(
    caption = paste0("ICES Stock Assessment Database, ",
                     format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen")
  )

  proxy_stroke <- 2.5

  # --- Base plot (segments; color by Status, no legend)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = StockKeyLabel)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = StockKeyLabel, y = 0, yend = Catches/1000, colour = Status),
      size = 2, na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(y = Landings/1000, xend = StockKeyLabel, yend = 0, colour = Status),
      size = 2, na.rm = TRUE, show.legend = FALSE
    )

  # --- Points (NORMAL refpoints: filled; no legend)
  p <- p +
    ggplot2::geom_point(
      data = dplyr::filter(df, !ProxyFlag),
      ggplot2::aes(y = Catches/1000, fill = Status),
      shape = 24, colour = "grey35", size = 7, alpha = 0.85,
      na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, !ProxyFlag),
      ggplot2::aes(y = Landings/1000, fill = Status),
      shape = 21, colour = "grey35", size = 7, alpha = 0.85,
      na.rm = TRUE, show.legend = FALSE
    )

  # --- Points (PROXY refpoints: hollow with Status-colored outline; no legend)
  p <- p +
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag),
      ggplot2::aes(y = Catches/1000, colour = Status),
      shape = 24, fill = NA, size = 7, alpha = 1, stroke = proxy_stroke,
      na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag),
      ggplot2::aes(y = Landings/1000, colour = Status),
      shape = 21, fill = NA, size = 7, alpha = 1, stroke = proxy_stroke,
      na.rm = TRUE, show.legend = FALSE
    )

  # --- Scales (suppress Status legends)
  p <- p +
    ggplot2::scale_fill_manual(values = status_pal, guide = "none") +
    ggplot2::scale_colour_manual(values = status_pal, guide = "none")

  # --- Axes, theme
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(x = "Stock name", y = "Catch and Landings (thousand tonnes)") +
    ggplot2::theme(
      plot.caption       = ggplot2::element_text(size = 14),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(size = 0.1, colour = "grey80")
    ) +
    cap_lab

  # --- Legend (bottom-right): build only entries present in data
  has_land_norm   <- any(!is.na(df$Landings) & !df$ProxyFlag, na.rm = TRUE)
  has_land_proxy  <- any(!is.na(df$Landings) &  df$ProxyFlag,  na.rm = TRUE)
  has_catch_norm  <- any(!is.na(df$Catches)  & !df$ProxyFlag,  na.rm = TRUE)
  has_catch_proxy <- any(!is.na(df$Catches)  &  df$ProxyFlag,   na.rm = TRUE)

  legend_keys <- c(
    "Landings"        = 21,
    "Landings (proxy)"= 21,
    "Catches"         = 24,
    "Catches (proxy)" = 24
  )
  present <- c(has_land_norm, has_land_proxy, has_catch_norm, has_catch_proxy)
  legend_keys <- legend_keys[present]
  legend_labels <- names(legend_keys)

  if (length(legend_keys) > 0) {
    # Dummy layer to host the legend (alpha=0 so it won't plot; legend uses override.aes)
    p <- p +
      ggplot2::geom_point(
        data = data.frame(Legend = factor(legend_labels, levels = legend_labels)),
        ggplot2::aes(x = 0, y = 0, shape = Legend),
        inherit.aes = FALSE, alpha = 0, show.legend = TRUE
      ) +
      ggplot2::scale_shape_manual(
        name   = NULL,
        breaks = legend_labels,
        values = legend_keys,
        labels = legend_labels
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          override.aes = list(
            size   = 6,
            # per-key aesthetics matching 'legend_labels' order:
            fill   = c("Landings"         = "grey60",
                       "Landings (proxy)" = NA,
                       "Catches"          = "grey60",
                       "Catches (proxy)"  = NA)[legend_labels],
            colour = c("Landings"         = "grey25",
                       "Landings (proxy)" = "grey25",
                       "Catches"          = "grey25",
                       "Catches (proxy)"  = "grey25")[legend_labels],
            stroke = c("Landings"         = 1.0,
                       "Landings (proxy)" = 2,
                       "Catches"          = 1.0,
                       "Catches (proxy)"  = 2)[legend_labels],
            alpha  = 1
          ),
          keyheight = ggplot2::unit(18, "pt"),
          keywidth  = ggplot2::unit(26, "pt"),
          byrow = TRUE
        )
      ) +
      ggplot2::theme(
        legend.position      = c(0.98, 0.02),  # bottom-right inside
        legend.justification = c(1, 0),
        legend.background    = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.9),
                                                     colour = "grey85"),
        legend.spacing.y  = ggplot2::unit(10, "pt"),
        legend.key.height    = ggplot2::unit(20, "pt"),
        legend.key.width     = ggplot2::unit(20, "pt")
      )
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  if (isTRUE(return_data)) df else p
}



# plot_kobe_app <- function(x, guild, return_data = FALSE){
#         cap_lab <- ggplot2::labs(caption = paste0("ICES Stock Assessment Database, ",
#                      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"))

#         if(guild == "All"){
#                 df <-x
#         }else(df <- dplyr::filter(x,FisheriesGuild %in% guild))
#         xmax = max(df$F_FMSY, na.rm = TRUE)
#         ifelse(xmax < 3, xmax2 <- 3, xmax2 <- (xmax + 0.5))
#         ymax = max(df$SSB_MSYBtrigger, na.rm = TRUE)
#         ifelse(ymax < 3, ymax2 <- 3, ymax2 <- (ymax + 0.5))
#         kobe <- ggplot2::ggplot(df, ggplot2::aes(x = F_FMSY, y = SSB_MSYBtrigger,
#                                          data_id = StockKeyLabel)) +
#                 ggplot2::coord_cartesian(xlim = c(0, xmax2), ylim = c(0, ymax2))+
#                 ggplot2::geom_point(ggplot2::aes(color = Status), size = 13,
#                            alpha = 0.7, na.rm = TRUE) +
#                 ggplot2::geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
#                 ggplot2::geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
#                 ggrepel::geom_text_repel(ggplot2::aes(label = StockKeyLabel),
#                                          segment.size = .25,
#                                          force = 5,
#                                          size = 5) +
#                 ggplot2::scale_color_manual(values = c("GREEN" = "#4daf4a",
#                                               "RED" = "#e41a1c",
#                                               "GREY" = "#d3d3d3")) +
#                 ggplot2::labs(x = expression(F/F[MSY]),
#                      y = expression(SSB/MSY~B[trigger]),
#                      caption = "") +
#                 ggplot2::theme_bw(base_size = 20) +
#                 ggplot2::theme(legend.position = 'none',
#                       panel.grid.minor = ggplot2::element_blank(),
#                       panel.grid.major = ggplot2::element_blank(),
#                       plot.caption = ggplot2::element_text(size = 10)) +
#                       cap_lab
      
        
#         if(return_data == T){
#                 df
#         }else{
#                 kobe
#         }
# }

plot_kobe_app <- function(x, guild, return_data = FALSE){

  cap_lab <- ggplot2::labs(
    caption = paste0("ICES Stock Assessment Database, ",
                     format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen")
  )

  # Filter by guild
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

  # Be robust if proxy flags aren't present
  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE

  # Flag proxy if either reference point is proxy
  df <- df %>%
    dplyr::mutate(
      ProxyFlag = dplyr::if_else((F_proxy %in% TRUE) | (B_proxy %in% TRUE),
                                 "Proxy refpoint", "Normal refpoint")
    )

  # Axes limits
  xmax  <- suppressWarnings(max(df$F_FMSY, na.rm = TRUE))
  xmax2 <- if (is.finite(xmax) && xmax < 3) 3 else xmax + 0.5
  ymax  <- suppressWarnings(max(df$SSB_MSYBtrigger, na.rm = TRUE))
  ymax2 <- if (is.finite(ymax) && ymax < 3) 3 else ymax + 0.5

  # Symbol sizes
  pt_size   <- 10
  proxy_stroke <- 1.8  # <-- thicker outline for empty circle

  kobe <-
    ggplot2::ggplot(df, ggplot2::aes(x = F_FMSY, y = SSB_MSYBtrigger, data_id = StockKeyLabel)) +
    ggplot2::coord_cartesian(xlim = c(0, xmax2), ylim = c(0, ymax2)) +

    # ---- Normal refpoint (filled circle) ----
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag == "Normal refpoint"),
      ggplot2::aes(color = Status, shape = ProxyFlag),
      size = pt_size, alpha = 0.7, na.rm = TRUE
    ) +

    # ---- Proxy refpoint (empty circle with thicker outline) ----
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag == "Proxy refpoint"),
      ggplot2::aes(color = Status, shape = ProxyFlag),
      size = pt_size, alpha = 0.9, na.rm = TRUE,
      fill = NA, stroke = proxy_stroke
    ) +

    ggplot2::geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +

    ggrepel::geom_text_repel(
      ggplot2::aes(label = StockKeyLabel),
      segment.size = .25, force = 5, size = 5
    ) +

    # Color by status (no color legend)
    ggplot2::scale_color_manual(
      values = c(GREEN = "#4daf4a", RED = "#e41a1c", GREY = "#d3d3d3"),
      guide = "none"
    ) +

    # Shape legend (auto-drops “Proxy refpoint” if not present)
    ggplot2::scale_shape_manual(
      name   = "Reference point",
      values = c("Normal refpoint" = 16,  # filled circle
                 "Proxy refpoint"  = 21), # circle with border (uses stroke)
      drop = TRUE
    ) +

    ggplot2::labs(
      x = expression(F/F[MSY]),
      y = expression(SSB/MSY~B[trigger]),
      caption = ""
    ) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major   = ggplot2::element_blank(),
      plot.caption       = ggplot2::element_text(size = 14),
      legend.position    = c(0.98, 0.98),  # top-right inside plot
      legend.justification = c(1, 1),
      legend.background  = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.85),
                                                 color = "grey85"),
      legend.key.height  = ggplot2::unit(12, "pt"),
      legend.key.width   = ggplot2::unit(18, "pt")
    ) +
    # Make legend symbols neutral (single color) and readable
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        override.aes = list(size = 4, alpha = 1, colour = "grey20", fill = NA, stroke = proxy_stroke)
      )
    ) +
    cap_lab

  if (isTRUE(return_data)) df else kobe
}




# Old version of stock trends function with experimental coverage rule
# stock_trends <- function(x, coverage_thresh = 0.80){
#   x$FishingPressure <- as.numeric(x$FishingPressure)
#   x$StockSize       <- as.numeric(x$StockSize)
#   x$FMSY            <- as.numeric(x$FMSY)
#   x$MSYBtrigger     <- as.numeric(x$MSYBtrigger)
#   x$Year            <- as.numeric(x$Year)

#   df <- dplyr::mutate(x,
#     FMEAN   = mean(FishingPressure, na.rm = TRUE),
#     SSBMEAN = mean(StockSize,      na.rm = TRUE),
#     FMEAN   = ifelse(!grepl("F|F(ages 3-6)", FishingPressureDescription), NA, FMEAN),
#     SSBMEAN = ifelse(!grepl("StockSize",     StockSizeDescription),       NA, SSBMEAN)
#   )

#   df <- dplyr::mutate(df,
#     F_FMSY          = ifelse(!is.na(FMSY),        FishingPressure / FMSY,        NA),
#     SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger), StockSize       / MSYBtrigger, NA),
#     F_FMEAN         = ifelse(!is.na(FMEAN),       FishingPressure / FMEAN,       NA),
#     SSB_SSBMEAN     = ifelse(!is.na(SSBMEAN),     StockSize       / SSBMEAN,     NA)
#   )

#   df <- df %>%
#     dplyr::select(Year, StockKeyLabel, FisheriesGuild, F_FMSY, SSB_MSYBtrigger, F_FMEAN, SSB_SSBMEAN)

#   df2 <- tidyr::gather(df, Metric, Value, -Year, -StockKeyLabel, -FisheriesGuild) %>%
#     dplyr::filter(!is.na(Year))

#   df3 <- df2 %>%
#     dplyr::group_by(StockKeyLabel, FisheriesGuild, Metric, Year) %>%
#     dplyr::summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
#     dplyr::filter(!is.na(Value))

#   # ---- NEW: coverage rule for the across-stock yearly "Mean"
#   # Denominator = distinct stocks that ever have data for this FisheriesGuild × Metric (across all years)
#   denom <- df2 %>%
#     dplyr::filter(!is.na(Value)) %>%
#     dplyr::group_by(FisheriesGuild, Metric) %>%
#     dplyr::summarise(total_stocks = dplyr::n_distinct(StockKeyLabel), .groups = "drop")

#   # For each year: how many of those stocks have data, and what is that year's mean?
#   year_stats <- df2 %>%
#     dplyr::group_by(FisheriesGuild, Metric, Year) %>%
#     dplyr::summarise(
#       n_present = dplyr::n_distinct(StockKeyLabel[!is.na(Value)]),
#       year_mean = mean(Value, na.rm = TRUE),
#       .groups = "drop"
#     )

#   means <- year_stats %>%
#     dplyr::inner_join(denom, by = c("FisheriesGuild","Metric")) %>%
#     dplyr::mutate(
#       coverage = ifelse(total_stocks > 0, n_present / total_stocks, NA_real_),
#       Value    = dplyr::if_else(!is.na(coverage) & coverage >= coverage_thresh, year_mean, NA_real_),
#       StockKeyLabel = "Mean"
#     ) %>%
#     dplyr::filter(!is.na(Value)) %>%
#     dplyr::select(FisheriesGuild, StockKeyLabel, Year, Metric, Value)

#   df4 <- dplyr::bind_rows(df3, means) %>%
#     dplyr::distinct(.keep_all = TRUE)

#   return(df4)
# }