################# Data processing ################################################

#' Fetch SID stock list for a given year and ecoregion
#'
#' Queries the ICES Stock List OData service for the specified
#' \code{year}, filters to the requested ecoregion, and returns a
#' processed stock list data frame. Some assessment keys are
#' hard-coded/expanded for specific stocks, and fisheries guild labels
#' are standardised.
#'
#' @param year Integer or numeric; assessment year to query in the ICES
#'   Stock List service (used in the \code{ActiveYear} filter).
#' @param EcoR Character scalar; ecoregion name to filter on (must match
#'   the \code{EcoRegion} values returned by the service, e.g.
#'   \code{"Greater North Sea"}).
#'
#' @return
#' A data frame (tibble) containing at least the columns:
#' \describe{
#'   \item{StockKeyLabel}{ICES stock identifier.}
#'   \item{EcoRegion}{Ecoregion name (one per row after splitting).}
#'   \item{YearOfLastAssessment}{Year of last assessment.}
#'   \item{AssessmentKey}{Numeric assessment key (non-\code{NA} only).}
#'   \item{StockKeyDescription}{Text description of the stock.}
#'   \item{SpeciesScientificName}{Scientific name of the species.}
#'   \item{SpeciesCommonName}{Common name of the species.}
#'   \item{AdviceCategory}{ICES advice category.}
#'   \item{DataCategory}{ICES data category.}
#'   \item{FisheriesGuild}{Fisheries guild, with \code{"crustacean"}
#'     recoded to \code{"shellfish"}.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Calls the ICES OData endpoint
#'     \code{StockListDWs4} with a filter on \code{ActiveYear == year}
#'     and a restricted set of columns (e.g. \code{StockKeyLabel},
#'     \code{EcoRegion}, \code{AssessmentKey}, \code{FisheriesGuild}, etc.).
#'   \item Parses the JSON response via \code{jsonlite::fromJSON()} and
#'     extracts the \code{$value} array.
#'   \item Ensures \code{EcoRegion} is character and splits multi-valued
#'     ecoregions into separate rows using
#'     \code{tidyr::separate_rows(EcoRegion, sep = ", ")}.
#'   \item Filters rows to the requested \code{EcoR}.
#'   \item For a few specific stocks, calls [add_keys()] to add extra
#'     rows with additional \code{AssessmentKey} values:
#'     \code{"cod.27.46a7d20"} and \code{"cod.21.1.isc"}.
#'   \item Drops any rows with \code{NA} in \code{AssessmentKey}.
#'   \item Recodes \code{FisheriesGuild == "crustacean"} to
#'     \code{"shellfish"}.
#' }
#'
#' This helper is used upstream by the application to fetch the SID
#' (Stock Information Database) for a single ecoregion and year.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @importFrom dplyr mutate filter
#' @importFrom tidyr separate_rows
#' @noRd
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

        ############ Hard coded for some stocks with assessmentComponents
        stock_list_long <- add_keys(stock_list_long, "cod.27.46a7d20", c(19661,19662))
        stock_list_long <- add_keys(stock_list_long, "cod.21.1.isc", c(19605))
        
        stock_list_long <- stock_list_long[!is.na(stock_list_long$AssessmentKey), ]
        
        stock_list_long$FisheriesGuild[stock_list_long$FisheriesGuild == "crustacean"] <- "shellfish"
        return(stock_list_long)
} 

#' Fetch latest SAG data for an ecoregion
#'
#' Queries the ICES SAG (Stock Assessment Graphs) API for the latest
#' stock data corresponding to a given ecoregion.
#'
#' @param Ecoregion Character scalar giving the full ecoregion name
#'   (e.g. \code{"Greater North Sea"}, \code{"Baltic Sea"}). This is
#'   converted to the corresponding ICES ecoregion code via
#'   [get_ecoregion_acronym()].
#'
#' @return
#' A list or data frame (as returned by \code{jsonlite::fromJSON()})
#' containing the latest SAG data for the requested ecoregion.
#' The exact structure is determined by the SAG API response.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts \code{Ecoregion} to its ICES acronym using
#'     [get_ecoregion_acronym()].
#'   \item Calls the SAG API endpoint
#'     \code{https://sag.ices.dk/SAG_API/LatestStocks/Download}
#'     with the \code{ecoregion} query parameter set to that acronym.
#'   \item Parses the JSON response via \code{jsonlite::fromJSON()} and
#'     returns the parsed object directly.
#' }
#'
#' This helper is typically used upstream by the application to obtain
#' the latest assessment data for all stocks in a given ecoregion.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @noRd
getSAG_ecoregion_new <- function(Ecoregion) {
       
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Download?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag)
}

#' Fetch and merge stock status for an ecoregion
#'
#' Queries the ICES SAG status web service for the latest stock status
#' in a given ecoregion, unnests the per-year status, and merges it with
#' a SID stock list (typically from [getSID()]).
#'
#' @param Ecoregion Character scalar giving the full ecoregion name
#'   (e.g. \code{"Greater North Sea"}, \code{"Baltic Sea"}). This is
#'   converted to the ICES ecoregion code via [get_ecoregion_acronym()].
#' @param sid A data frame of stock metadata, usually the output of
#'   [getSID()], containing at least an \code{AssessmentKey} column and
#'   \code{FisheriesGuild}.
#'
#' @return
#' A data frame resulting from a left merge of \code{sid} with the
#' un-nested SAG status data on \code{AssessmentKey}. All columns from
#' \code{sid} are retained, with additional status columns added.
#' \code{FisheriesGuild} is converted to lower case.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts \code{Ecoregion} to its ICES acronym using
#'     [get_ecoregion_acronym()].
#'   \item Calls the SAG API endpoint
#'     \code{https://sag.ices.dk/SAG_API/LatestStocks/Status}
#'     with \code{ecoregion=<acronym>} and parses the JSON response via
#'     \code{jsonlite::fromJSON()}.
#'   \item Assumes the returned object has a list-column \code{YearStatus}
#'     and flattens it with \code{tidyr::unnest(YearStatus)}.
#'   \item Merges the resulting status table with \code{sid} using
#'     \code{merge(..., by = "AssessmentKey", all.x = TRUE)} so that all
#'     SID rows are preserved even if no status is available.
#'   \item Normalises \code{FisheriesGuild} to lower case for
#'     downstream use.
#' }
#'
#' This helper is intended for internal use in preparing stock status
#' tables for the application.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @importFrom tidyr unnest
#' @noRd
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


#' Format and summarise SAG stock status
#'
#' Cleans and reshapes the merged SAG status data into a wide, 
#' stock-level summary table used by the application. It:
#' \itemize{
#'   \item attaches \code{AssessmentComponent} from the raw SAG object,
#'   \item appends it to \code{StockKeyLabel} where present (and strips
#'     the literal "Substock"),
#'   \item maps numeric status codes to colour/status labels,
#'   \item normalises and recodes fishing pressure and stock size
#'     variables (e.g. MSYBt, FQual, SSBQual),
#'   \item de-duplicates by stock / line description / type, keeping the
#'     most recent year, and
#'   \item spreads \code{type} into separate columns and renames them to
#'     \code{FishingPressure} and \code{StockSize}.
#' }
#'
#' @param df Data frame of status information, typically the output from
#'   [getStatusWebService()], containing at least:
#'   \code{AssessmentKey}, \code{StockKeyLabel}, \code{status},
#'   \code{fishingPressure}, \code{stockSize}, \code{type},
#'   \code{lineDescription}, \code{year}, and \code{FisheriesGuild}.
#' @param sag Raw SAG data object as returned by
#'   [getSAG_ecoregion_new()], containing at least
#'   \code{AssessmentKey} and \code{AssessmentComponent}.
#'
#' @return
#' A data frame in wide format with one row per
#' \code{StockKeyLabel}–\code{AssessmentKey}–\code{lineDescription}
#' combination, including:
#' \describe{
#'   \item{StockKeyLabel}{Stock label, with \code{AssessmentComponent}
#'     appended (and "Substock" removed) where applicable.}
#'   \item{AssessmentKey}{Numeric assessment key.}
#'   \item{lineDescription}{Text description of the status line
#'     (harmonised casing for MSY / precautionary lines).}
#'   \item{FisheriesGuild}{Fisheries guild, with \code{"crustacean"}
#'     recoded to \code{"shellfish"}.}
#'   \item{FishingPressure}{Status code (e.g. \code{"GREEN"},
#'     \code{"RED"}, \code{"ORANGE"}, \code{"GREY"}) for fishing
#'     pressure.}
#'   \item{StockSize}{Status code for stock size in the same coding
#'     scheme.}
#' }
#'
#' @details
#' The numeric \code{status} codes are mapped to categorical labels:
#' \code{0 = "GREY"}, \code{1/2 = "GREEN"}, \code{3 = "ORANGE"},
#' \code{4/5 = "RED"}, \code{6 = "GREY"}, and codes \code{7–9} are
#' kept as qualitative trend markers (\code{"qual_UP"},
#' \code{"qual_STEADY"}, \code{"qual_DOWN"}). Qualitative-only lines
#' (e.g. "Management plan", "Qualitative evaluation") are filtered out
#' from the final output.
#'
#' This helper is intended for internal use in the application to
#' prepare a concise status table for plotting and summaries.
#'
#' @importFrom dplyr mutate case_when filter rename
#' @importFrom tidyr spread
#' @noRd
format_sag_status_new <- function(df,sag) {

        df$AssessmentComponent <- sag$AssessmentComponent[ match(df$AssessmentKey, sag$AssessmentKey) ]
        df$StockKeyLabel <- ifelse(is.na(df$AssessmentComponent) |df$AssessmentComponent == "", df$StockKeyLabel, paste0(df$StockKeyLabel, "_", df$AssessmentComponent))
        df$StockKeyLabel <- gsub("\\s*Substock\\b", "", df$StockKeyLabel, ignore.case = TRUE)
        
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
        df<- subset(df, select = c(StockKeyLabel, AssessmentKey,lineDescription, type, status, FisheriesGuild))#, stockComponent,adviceValue
        df$FisheriesGuild[df$FisheriesGuild == "crustacean"] <- "shellfish" 
        df<- tidyr::spread(df,type, status)
        
        df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
        df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
        df <- df %>% dplyr::rename(FishingPressure = `Fishing pressure`,
                            StockSize = `Stock Size`)
      
        df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
        df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
        
        return(df)
}



#' Format annex table with GES summary
#'
#' Prepares a merged annex-style table combining stock status
#' information with SID/SAG metadata. It aligns \code{StockKeyLabel}
#' across sources (including substocks), attaches assessment keys and
#' components, and computes summary columns for Descriptor 3 (D3C1,
#' D3C2) and an overall GES status.
#'
#' @param status A data frame of formatted status information, typically
#'   the output of \code{format_sag_status_new()}, containing at least
#'   \code{StockKeyLabel}, \code{FishingPressure}, \code{StockSize},
#'   \code{StockKeyDescription}, and \code{FisheriesGuild}.
#' @param year Numeric assessment year (currently not used inside the
#'   function, but kept for interface compatibility and potential
#'   future filtering).
#' @param sid A SID stock list data frame, usually from \code{getSID()},
#'   containing \code{StockKeyLabel}, \code{AssessmentKey}, 
#'   \code{FisheriesGuild}, \code{StockKeyDescription}, etc.
#' @param sag A SAG data object, typically from
#'   \code{getSAG_ecoregion_new()}, containing at least
#'   \code{StockKeyLabel}, \code{AssessmentKey}, and
#'   \code{AssessmentComponent}.
#'
#' @return
#' A data frame with one row per stock (or stock component), including:
#' \describe{
#'   \item{StockKeyLabel}{Stock label, with \code{AssessmentComponent}
#'     appended (and “Substock” removed) where applicable.}
#'   \item{AssessmentKey}{Assessment key aligned with the SAG/SID data.}
#'   \item{AssessmentComponent}{Assessment component (or \code{NA} if
#'     none).}
#'   \item{FisheriesGuild}{Fisheries guild from SID/status.}
#'   \item{FishingPressure}{Categorical status (e.g. \code{"GREEN"},
#'     \code{"RED"}, \code{"GREY"}).}
#'   \item{StockSize}{Categorical status for stock size.}
#'   \item{D3C1}{Alias of \code{FishingPressure}.}
#'   \item{D3C2}{Alias of \code{StockSize}.}
#'   \item{GES}{Overall GES summary:
#'     \code{"GREEN"} if both \code{FishingPressure} and \code{StockSize}
#'     are \code{"GREEN"};
#'     \code{"RED"} if either is \code{"RED"};
#'     \code{"GREY"} if either is \code{"GREY"} or if none of the above
#'     apply.}
#'   \item{StockKeyDescription}{Cleaned description with any text in
#'     parentheses removed.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Merges \code{sag} (selecting \code{StockKeyLabel},
#'     \code{AssessmentKey}, \code{AssessmentComponent}) with \code{sid}
#'     by \code{StockKeyLabel}, then de-duplicates and renames
#'     \code{AssessmentKey.x} to \code{AssessmentKey}.
#'   \item Filters \code{sid} to stocks present in \code{status}, using
#'     the base (pre-substock) \code{StockKeyLabel}.
#'   \item Rebuilds \code{StockKeyLabel} including \code{AssessmentComponent}
#'     (where non-empty) and strips any trailing “Substock” text.
#'   \item Joins the resulting \code{sid} back onto \code{status} by
#'     \code{StockKeyLabel}, normalising some column names and dropping
#'     duplicates.
#'   \item Creates D3 columns:
#'     \code{D3C1 = FishingPressure}, \code{D3C2 = StockSize}, and
#'     \code{GES} as described above.
#'   \item Cleans \code{StockKeyDescription} by removing any
#'     parenthesised text.
#' }
#'
#' This helper is designed to produce the annex-style summary table used
#' for reporting and export from the application.
#
#' @importFrom dplyr select distinct rename filter mutate case_when left_join
#' @noRd
format_annex_table <- function(status, year, sid, sag) {
        
        sid <- merge(sag %>% dplyr::select(StockKeyLabel, AssessmentKey, AssessmentComponent), sid, by = "StockKeyLabel")
              
        sid <- sid %>%
                dplyr::distinct() %>%
                dplyr::rename(AssessmentKey = AssessmentKey.x) %>%
                dplyr::select(-AssessmentKey.y) %>%
                dplyr::filter(StockKeyLabel %in% sub("_.*$", "", status$StockKeyLabel))
        
        # need this step to make stocks with substocks match between sid and status
        sid$StockKeyLabel <- ifelse(is.na(sid$AssessmentComponent) |sid$AssessmentComponent == "", sid$StockKeyLabel, paste0(sid$StockKeyLabel, "_", sid$AssessmentComponent))
        sid$StockKeyLabel <- gsub("\\s*Substock\\b", "", sid$StockKeyLabel, ignore.case = TRUE)

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


#' Format SAG data and attach fisheries guilds
#'
#' Cleans and enriches raw SAG data by attaching fisheries guild
#' information from SID, filtering out ambiguous multi-purpose stocks,
#' and harmonising stock labels and guild names.
#'
#' @param sag A data frame (or tibble) with SAG data, typically the
#'   output of \code{getSAG_ecoregion_new()}, containing at least
#'   \code{StockKeyLabel}, \code{AssessmentKey}, \code{Purpose}, and
#'   \code{AssessmentComponent}.
#' @param sid A SID data frame, usually from \code{getSID()}, containing
#'   at least \code{AssessmentKey}, \code{FisheriesGuild}, and
#'   \code{YearOfLastAssessment}.
#'
#' @return
#' A data frame with:
#' \describe{
#'   \item{All SAG columns}{from \code{sag}, merged with \code{sid} on
#'     \code{AssessmentKey}.}
#'   \item{FisheriesGuild}{Lower-case fisheries guild, with
#'     \code{"crustacean"} recoded to \code{"shellfish"}.}
#'   \item{StockKeyLabel}{Potentially modified stock label including
#'     \code{AssessmentComponent} (when present) and with any literal
#'     “Substock” removed.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters \code{sid} to rows with a non-missing
#'     \code{YearOfLastAssessment} and keeps only
#'     \code{AssessmentKey} and \code{FisheriesGuild}.
#'   \item Merges \code{sag} and \code{sid} via
#'     \code{merge(sag, sid, all.x = TRUE, all.y = FALSE)}.
#'   \item Normalises \code{FisheriesGuild} to lower case and recodes
#'     \code{"crustacean"} to \code{"shellfish"}.
#'   \item Identifies stocks with multiple \code{Purpose} entries (same
#'     \code{StockKeyLabel} but multiple purposes) and removes those
#'     from the output via \code{dplyr::anti_join()} to avoid
#'     ambiguous records.
#'   \item Appends \code{AssessmentComponent} to \code{StockKeyLabel}
#'     where non-empty and strips any trailing “Substock” (case
#'     insensitive).
#' }
#'
#' This helper is intended for internal use when preparing SAG data
#' for plotting and summaries within the application.
#'
#' @importFrom dplyr filter select anti_join
#' @noRd
format_sag <- function(sag, sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        # sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        sid <- dplyr::select(sid,AssessmentKey, FisheriesGuild)
        
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        
        df1 <-as.data.frame(df1)
        
        # df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        # replace the fisheries guild == crustacean with shellfish
        df1$FisheriesGuild[df1$FisheriesGuild == "crustacean"] <- "shellfish"
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        
        out <- dplyr::anti_join(df1, check)

        out$StockKeyLabel <- ifelse(is.na(out$AssessmentComponent) | out$AssessmentComponent == "", out$StockKeyLabel, paste0(out$StockKeyLabel, "_", out$AssessmentComponent))
        out$StockKeyLabel <- gsub("\\s*Substock\\b", "", out$StockKeyLabel, ignore.case = TRUE)
        
        return(out)
}


#' Apply custom proxy reference points to SAG data
#'
#' Reads a table of custom reference point selections and applies them
#' to a formatted SAG data set, optionally replacing the official
#' \code{FMSY} and \code{MSYBtrigger} values with user-chosen proxy
#' reference points. Flags and names for the chosen proxies are also
#' added.
#'
#' @param sag_formatted A data frame of formatted SAG data (e.g. from
#'   \code{format_sag()}), containing at least:
#'   \code{AssessmentKey}, \code{FMSY}, \code{MSYBtrigger},
#'   \code{CustomRefPointName1}–\code{CustomRefPointName4} and
#'   \code{CustomRefPointValue1}–\code{CustomRefPointValue4}.
#' @param custom_refpoints_path Character scalar; path to a CSV file
#'   containing custom reference point settings. The file is read with
#'   \code{read.table()} using \code{sep = ","} and must contain at
#'   least:
#'   \describe{
#'     \item{AssessmentKey}{Numeric or character assessment key.}
#'     \item{SAGChartKey}{Integer indicating which chart the setting
#'       applies to (3 = FMSY, 4 = MSYBtrigger).}
#'     \item{settingValue}{Integer 1–4 indicating which custom
#'       reference point (out of 1–4) should be used as proxy.}
#'   }
#'
#' @return
#' A data frame identical to \code{sag_formatted} but with additional
#' columns:
#' \describe{
#'   \item{FMSY_is_proxy}{Logical; \code{TRUE} if a proxy was chosen for
#'     \code{FMSY} (i.e. \code{SAGChartKey = 3} present).}
#'   \item{FMSY_proxy_name}{Name of the chosen \code{FMSY} proxy (from
#'     \code{CustomRefPointName1}–\code{CustomRefPointName4}), or
#'     \code{NA} if none.}
#'   \item{MSYB_is_proxy}{Logical; \code{TRUE} if a proxy was chosen for
#'     \code{MSYBtrigger} (i.e. \code{SAGChartKey = 4} present).}
#'   \item{MSYB_proxy_name}{Name of the chosen \code{MSYBtrigger} proxy,
#'     or \code{NA} if none.}
#' }
#'
#' In addition, when a choice is present:
#' \itemize{
#'   \item \code{FMSY} is overwritten by the selected
#'     \code{CustomRefPointValue1}–\code{CustomRefPointValue4} for
#'     \code{SAGChartKey = 3};
#'   \item \code{MSYBtrigger} is overwritten by the selected custom
#'     value for \code{SAGChartKey = 4}.
#'   \item If no proxy is chosen, the original \code{FMSY} /
#'     \code{MSYBtrigger} values are retained.
#' }
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Reads \code{custom_refpoints_path} as a CSV into
#'     \code{custom_RefPoints}.
#'   \item Filters to \code{SAGChartKey \%in\% c(3, 4)} and, for each
#'     \code{AssessmentKey}–\code{SAGChartKey} combination, keeps the
#'     first \code{settingValue}.
#'   \item Reshapes to a wide table with columns
#'     \code{choice_3} (for FMSY) and \code{choice_4} (for MSYBtrigger).
#'   \item Left-joins these choices onto \code{sag_formatted} and
#'     derives:
#'     \itemize{
#'       \item Boolean flags \code{FMSY_is_proxy}, \code{MSYB_is_proxy};
#'       \item proxy names from the corresponding
#'         \code{CustomRefPointName1}–\code{CustomRefPointName4};
#'       \item replacement values for \code{FMSY} and
#'         \code{MSYBtrigger} using
#'         \code{dplyr::coalesce()} to fall back to the original values
#'         when no proxy is chosen.
#'     }
#'   \item Drops the temporary \code{choice_*} columns before returning.
#' }
#'
#' @importFrom dplyr filter group_by summarise mutate left_join across
#'   case_when coalesce select
#' @importFrom tidyr pivot_wider
#' @noRd
add_proxyRefPoints <- function(sag_formatted, custom_refpoints_path) {
        
        custom_RefPoints <- read.table(custom_refpoints_path,
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


#' Compute current stock status with proxy reference points (CLD view)
#'
#' Derives a current, per-stock status summary for fishing pressure and
#' stock size, taking into account possible proxy reference points for
#' \code{FMSY} and \code{MSYBtrigger}. This is intended for use in the
#' catch–landings–discards (CLD) views of the application.
#'
#' @param x A data frame containing stock time series and reference
#'   points, typically based on formatted SAG data with optional proxy
#'   columns. It must include (where available):
#'   \itemize{
#'     \item \code{StockKeyLabel}, \code{FisheriesGuild}
#'     \item \code{Year}, \code{AssessmentYear}
#'     \item \code{FishingPressure}, \code{StockSize}
#'     \item \code{FMSY}, \code{MSYBtrigger}
#'     \item \code{Catches}, \code{Landings}, \code{Discards}
#'     \item optional proxy columns:
#'       \code{FMSY_is_proxy}, \code{FMSY_proxy_name},
#'       \code{MSYB_is_proxy}, \code{MSYB_proxy_name}
#'   }
#'   Missing proxy columns are created and filled with \code{NA} if
#'   absent.
#'
#' @return
#' A data frame with one row per stock (and fisheries guild), including:
#' \describe{
#'   \item{StockKeyLabel}{Stock identifier.}
#'   \item{FisheriesGuild}{Fisheries guild for the stock.}
#'   \item{F_FMSY}{Ratio of fishing pressure to \code{FMSY} for
#'     \code{Year = AssessmentYear - 1}.}
#'   \item{SSB_MSYBtrigger}{Ratio of stock size to \code{MSYBtrigger}
#'     for the latest year among \code{AssessmentYear} and
#'     \code{AssessmentYear - 1} with a non-missing
#'     \code{MSYBtrigger}.}
#'   \item{Catches, Landings, Discards}{Total catches, landings, and
#'     discards (for the F-year row).}
#'   \item{FMSY}{Reference point used for \code{F_FMSY}, possibly
#'     already updated by proxy logic upstream.}
#'   \item{FishingPressure}{Fishing pressure used in the ratio.}
#'   \item{MSYBtrigger}{Reference point used for
#'     \code{SSB_MSYBtrigger}.}
#'   \item{F_proxy}{Logical flag indicating that a proxy FMSY was used
#'     (\code{FMSY_is_proxy}).}
#'   \item{F_proxy_name}{Name of the chosen FMSY proxy.}
#'   \item{B_proxy}{Logical flag indicating that a proxy MSYBtrigger was
#'     used (\code{MSYB_is_proxy}).}
#'   \item{B_proxy_name}{Name of the chosen MSYBtrigger proxy.}
#'   \item{Status}{Categorical status:
#'     \code{"GREEN"} if \code{F_FMSY < 1} and
#'     \code{SSB_MSYBtrigger >= 1};
#'     \code{"RED"} otherwise, when both ratios are available;
#'     \code{"GREY"} if either ratio is missing.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Ensures that proxy flag/name columns exist; if missing, they
#'     are created and filled with \code{NA}.
#'   \item Coerces a set of numeric columns (years, reference points,
#'     CLD values) to numeric, suppressing conversion warnings.
#'   \item Computes, per stock, the latest \code{AssessmentYear}
#'     (\code{AY_latest}).
#'   \item For the F side:
#'     \item Filters to \code{Year = AY_latest - 1},
#'     \item Computes \code{F_FMSY = FishingPressure / FMSY},
#'     \item Keeps at most one row per stock (latest year).
#'   \item For the B side:
#'     \item Filters to \code{Year \%in\% \{AY_latest, AY_latest - 1\}},
#'     \item For each stock, keeps the latest row with non-missing
#'       \code{MSYBtrigger},
#'     \item Computes \code{SSB_MSYBtrigger = StockSize / MSYBtrigger}.
#'   \item Full-joins F and B summaries by
#'     \code{StockKeyLabel, FisheriesGuild} and assigns a status:
#'     \item \code{"GREY"} if either ratio is missing,
#'     \item \code{"GREEN"} if \code{F_FMSY < 1} and
#'       \code{SSB_MSYBtrigger >= 1},
#'     \item \code{"RED"} otherwise.
#' }
#'
#' Proxy logic (which reference point is used) is assumed to be handled
#' upstream (e.g. via [add_proxyRefPoints()]); this function only
#' records the proxy flags/names and uses the already chosen reference
#' point values in the ratios.
#'
#' @importFrom dplyr group_by ungroup mutate filter arrange slice_head
#'   select full_join case_when
#' @noRd
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



#' Compute stock-level and guild-mean trends with proxy reference points
#'
#' Builds a long-format time series of relative status metrics for each
#' stock and fisheries guild, including optional indicators for whether
#' proxy reference points were used. The output is suitable for plotting
#' stock trends (per stock and guild means) using F and SSB relative to
#' reference points and historical means.
#'
#' @param x A data frame containing time series and reference point
#'   information for multiple stocks. It must include at least:
#'   \itemize{
#'     \item \code{StockKeyLabel}, \code{FisheriesGuild}
#'     \item \code{Year}
#'     \item \code{FishingPressure}, \code{StockSize}
#'     \item \code{FMSY}, \code{MSYBtrigger}
#'     \item optionally:
#'       \code{FishingPressureDescription},
#'       \code{StockSizeDescription} (used to decide when to compute
#'       means), and
#'       \code{FMSY_is_proxy}, \code{FMSY_proxy_name},
#'       \code{MSYB_is_proxy}, \code{MSYB_proxy_name} (proxy flags /
#'       names).
#'   }
#'   If the proxy columns are missing, they are created and filled with
#'   \code{NA}.
#'
#' @return
#' A data frame in long format with columns:
#' \describe{
#'   \item{FisheriesGuild}{Fisheries guild.}
#'   \item{StockKeyLabel}{Stock label; the synthetic row
#'     \code{"Mean"} represents the guild mean for that year and
#'     metric.}
#'   \item{Year}{Year (numeric).}
#'   \item{Metric}{One of:
#'     \code{"F_FMSY"}, \code{"SSB_MSYBtrigger"},
#'     \code{"F_FMEAN"}, \code{"SSB_SSBMEAN"}.}
#'   \item{Value}{Numeric value of the metric.}
#'   \item{F_proxy}{Logical; \code{TRUE} if any FMSY proxy was used for
#'     this stock (same across all years/metrics for that stock).}
#'   \item{F_name}{Name of the FMSY proxy (if any) for this stock.}
#'   \item{B_proxy}{Logical; \code{TRUE} if any MSYBtrigger proxy was
#'     used for this stock.}
#'   \item{B_name}{Name of the MSYBtrigger proxy (if any).}
#'   \item{is_proxy_metric}{Logical; \code{TRUE} when the metric is
#'     directly linked to a proxy-based reference point
#'     (\code{F_FMSY} or \code{SSB_MSYBtrigger}).}
#'   \item{proxy_name_metric}{Name of the metric-specific proxy (F or
#'     B), or \code{NA} for metrics not based on proxies.}
#'   \item{Proxy_is_proxy}{Alias of \code{is_proxy_metric}, for
#'     backwards compatibility with plotting code.}
#'   \item{Proxy_name}{Alias of \code{proxy_name_metric}.}
#' }
#'
#' @details
#' The function proceeds as follows:
#' \enumerate{
#'   \item Ensures proxy columns
#'     (\code{FMSY_is_proxy}, \code{FMSY_proxy_name}, 
#'     \code{MSYB_is_proxy}, \code{MSYB_proxy_name}) exist; if missing,
#'     they are added and filled with \code{NA}.
#'   \item Coerces \code{FishingPressure}, \code{StockSize}, \code{FMSY},
#'     \code{MSYBtrigger}, and \code{Year} to numeric.
#'   \item Builds a per-stock proxy map (\code{proxy_map}) summarising:
#'     \itemize{
#'       \item whether an FMSY proxy (\code{F_proxy}) or MSYBtrigger
#'         proxy (\code{B_proxy}) was ever used for that stock, and
#'       \item the first non-\code{NA} proxy name for each type
#'         (\code{F_name}, \code{B_name}).
#'     }
#'   \item Computes global (over all years and rows) means
#'     \code{FMEAN} and \code{SSBMEAN} per row, then sets them to
#'     \code{NA} where the descriptor columns
#'     (\code{FishingPressureDescription}, \code{StockSizeDescription})
#'     do not match the expected patterns. These means are used for
#'     normalisation:
#'     \code{F_FMEAN = FishingPressure / FMEAN} and
#'     \code{SSB_SSBMEAN = StockSize / SSBMEAN}.
#'   \item Computes ratios:
#'     \item \code{F_FMSY          = FishingPressure / FMSY}
#'     \item \code{SSB_MSYBtrigger = StockSize / MSYBtrigger}
#'     \item \code{F_FMEAN         = FishingPressure / FMEAN}
#'     \item \code{SSB_SSBMEAN     = StockSize / SSBMEAN}
#'   \item Keeps only the columns needed for trend plotting and pivots
#'     them to long format (\code{Metric}, \code{Value}).
#'   \item Averages duplicates at the
#'     \code{(StockKeyLabel, FisheriesGuild, Metric, Year)} level.
#'   \item Computes guild means per year and metric (only where at least
#'     two stocks with non-\code{NA} values are available), stored as
#'     rows with \code{StockKeyLabel == "Mean"}.
#'   \item Binds stock-level and guild-mean rows, removes duplicates,
#'     joins \code{proxy_map}, and derives metric-specific proxy flags
#'     and names:
#'     \item \code{is_proxy_metric} is \code{TRUE} for \code{F_FMSY} and
#'       \code{SSB_MSYBtrigger} when the corresponding proxy is flagged.
#'     \item \code{proxy_name_metric} carries the F or B proxy name for
#'       those metrics.
#'   \item For backwards compatibility, copies these to
#'     \code{Proxy_is_proxy} and \code{Proxy_name}.
#' }
#'
#' @importFrom dplyr group_by summarise mutate select filter distinct bind_rows
#'   left_join ungroup
#' @importFrom tidyr pivot_longer
#' @noRd
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


#' Match stock codes to fish illustration files
#'
#' For each stock in \code{StockKeyLabel}, finds a corresponding image
#' file in the \code{inst/app/www/fish} directory based on the first
#' three characters of the stock code (typically the FAO 3-letter
#' species code). If no matching file is found, a default
#' \code{"fish.png"} is returned.
#'
#' @param StockKeyLabel Character vector of stock labels (e.g.
#'   \code{"cod.27.46a7d20"}). Only the first three characters are used
#'   for matching to illustration file names.
#' @param df Unused argument; included for interface compatibility with
#'   other code paths. It is ignored inside the function.
#'
#' @return
#' A character vector of the same length as \code{StockKeyLabel},
#' containing file names (not full paths) for the matched fish
#' illustrations. If no illustration is found for a given stock, the
#' default \code{"fish.png"} is returned.
#'
#' @details
#' The function searches the \code{inst/app/www/fish} folder for files
#' whose names contain the three-letter prefix
#' \code{substr(StockKeyLabel, 1, 3)}. If multiple files match, the
#' first one returned by \code{list.files()} is used. This is intended
#' for internal use by the Shiny app to display an appropriate fish
#' illustration for a stock.
#'
#' @importFrom base sapply substr
#' @noRd
match_stockcode_to_illustration <- function(StockKeyLabel, df) {
        sapply(StockKeyLabel, function(key) {
                temp <- list.files("inst/app/www/fish", pattern = substr(key, 1, 3))
                if (length(temp) == 0) "fish.png" else temp[1]
        })
}





################### Plotting functions ##########################################################

#' Plot proportional stock-status pies by guild and reference line
#'
#' Creates a grid of pie charts showing the distribution of stock
#' status categories (e.g. GREEN, RED, GREY, ORANGE) across fisheries
#' guilds and reference lines (MSY / PA), for both fishing pressure and
#' stock size. The plot is sized responsively based on the available
#' width.
#'
#' @param df A data frame of stock-level status information, typically
#'   derived from \code{format_sag_status_new()}, containing at least:
#'   \itemize{
#'     \item \code{StockKeyLabel}
#'     \item \code{FisheriesGuild}
#'     \item \code{lineDescription} (e.g. "Maximum sustainable yield",
#'       "Precautionary approach")
#'     \item \code{FishingPressure} (status code: GREEN, RED, GREY, etc.)
#'     \item \code{StockSize} (status code: GREEN, RED, GREY, etc.)
#'   }
#' @param return_data Logical; if \code{TRUE}, returns the processed
#'   data frame used for plotting (one row per guild–line–variable–
#'   colour slice) instead of the \code{ggplot} object. Default is
#'   \code{FALSE}.
#' @param width_px Numeric; approximate width of the plot in pixels
#'   (e.g. \code{session$clientData[["output_<id>_width"]]} in a Shiny
#'   context). Used to scale font sizes and spacing. Default is 800.
#' @param min_base,max_base Numeric; lower and upper bounds on the
#'   base font size used for the \code{theme_bw()} base size. Defaults:
#'   \code{min_base = 11}, \code{max_base = 18}.
#'
#' @return
#' If \code{return_data = FALSE} (default), a \code{ggplot} object with
#' faceted pie charts:
#' \itemize{
#'   \item Columns: combinations of indicator and line (e.g.
#'     "Fishing pressure\nMSY", "Stock size\nPA").
#'   \item Rows: fisheries guilds plus a \code{"total"} row combining
#'     all guilds.
#' }
#' If \code{return_data = TRUE}, a data frame containing the processed
#' counts and derived \code{fraction} used for the pie slices.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Selects \code{StockKeyLabel}, \code{FisheriesGuild},
#'     \code{lineDescription}, \code{FishingPressure}, and
#'     \code{StockSize}, then pivots to long format with a
#'     \code{Variable} column distinguishing fishing pressure vs stock
#'     size.
#'   \item Counts stocks by \code{FisheriesGuild}, \code{lineDescription},
#'     \code{Variable}, and status colour (\code{Colour}).
#'   \item Adds a "total" guild row by summing over guilds.
#'   \item Renames variables and line descriptions to compact forms
#'     (e.g. "Fishing pressure", "Stock size", "MSY", "PA"), and
#'     combines them into a two-line facet header.
#'   \item Filters to colours with positive counts and computes
#'     per-facet totals; uses a common maximum to scale slice radii so
#'     pies are comparable across facets.
#'   \item Orders facets in a fixed structure (MSY/PA × pressure/stock)
#'     when present, and orders guilds as
#'     \code{total, benthic, demersal, pelagic, shellfish, elasmobranch}.
#'   \item Draws pies with \code{geom_bar()} and
#'     \code{coord_polar(theta = "y")}, annotating each slice with the
#'     count of stocks, and applies a manual fill scale for the status
#'     colours.
#'   \item Scales font sizes, panel spacing, strip padding, and plot
#'     margins based on \code{width_px} and the number of facet
#'     columns, and adds a caption with the current date and ICES
#'     reference.
#' }
#'
#' This helper is intended for internal use within the Shiny app to
#' show a compact overview of the proportion of stocks in each status
#' category by guild and management line.
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_fill_manual
#'   coord_polar facet_grid labs theme_bw theme element_blank
#'   element_text margin
#' @importFrom grid unit
#' @importFrom dplyr select group_by summarise mutate filter bind_rows
#'   across everything
#' @importFrom tidyr gather spread
#' @noRd
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
  df2$Variable[df2$Variable == "FishingPressure"] <- "Fishing\npressure"
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
  wanted_headers <- c("Fishing\npressure\nMSY","Stock size\nMSY",
                      "Fishing\npressure\nPA", "Stock size\nPA")
  present_headers <- intersect(wanted_headers, unique(df2$header))
  df2$header <- factor(df2$header, levels = present_headers)

  df2$FisheriesGuild <- factor(
    tolower(df2$FisheriesGuild),
    levels = c("total","benthic","demersal","pelagic","shellfish","elasmobranch")
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

#' Plot GES pie charts for MSY status and catch proportions
#'
#' Produces a grid of pie charts summarising GES-related status under
#' the Maximum Sustainable Yield (MSY) framework, showing the
#' distribution of status categories (GREEN, RED, GREY, etc.) by
#' indicator (fishing pressure / stock size) and weighted by catch.
#'
#' The top row shows the proportion of catch (thousand tonnes) in each
#' status category, and labels each slice with the absolute catch and
#' percentage within that pie. A total catch label is shown in the
#' centre of each pie.
#'
#' @param x A data frame of stock-level status information under
#'   MSY, typically derived from \code{format_sag_status_new()}, and
#'   containing at least:
#'   \itemize{
#'     \item \code{StockKeyLabel}
#'     \item \code{lineDescription} (e.g. "Maximum sustainable yield")
#'     \item \code{FishingPressure} (status code: GREEN/RED/GREY/...)
#'     \item \code{StockSize} (status code: GREEN/RED/GREY/...)
#'   }
#'   Only rows with \code{lineDescription == "Maximum sustainable yield"}
#'   are used.
#' @param y A data frame of stock-level catch information for the same
#'   stocks, containing at least:
#'   \itemize{
#'     \item \code{StockKeyLabel}
#'     \item \code{Catches}
#'     \item \code{Landings}
#'   }
#'   The function derives \code{CATCH} per stock as
#'   \code{Catches} if available, otherwise \code{Landings}.
#' @param return_data Logical; if \code{TRUE}, returns the processed
#'   data (one row per status–indicator combination) instead of the
#'   plot. Defaults to \code{FALSE}.
#' @param width_px Numeric; approximate width of the plot in pixels
#'   (e.g. \code{session$clientData[["output_<id>_width"]]}) used to
#'   scale font sizes and labels. Default is 800.
#'
#' @return
#' If \code{return_data = FALSE} (default), returns a \code{ggplot}
#' object showing:
#' \itemize{
#'   \item Columns: indicator type (fishing pressure vs stock size).
#'   \item Rows: metric type (currently the catch-weighted pies:
#'     "Proportion of catch (thousand tonnes)").
#'   \item Slice fill: status colour (GREEN, RED, GREY, ORANGE, etc.).
#'   \item Slice labels: absolute catch (thousand tonnes) and
#'     percentage within each pie; a central label shows the total
#'     catch in that facet.
#' }
#'
#' If \code{return_data = TRUE}, returns the underlying data frame used
#' for plotting (one row per Metric–Variable–Colour combination) with:
#' \describe{
#'   \item{Variable}{Indicator: "Fishing Pressure" or "Stock Size".}
#'   \item{Color}{Status category (e.g. GREEN/RED/GREY/...).}
#'   \item{Metric}{"Proportion of catch \n(thousand tonnes)".}
#'   \item{Value}{Raw value used for the pie radius (catch).}
#'   \item{sum}{Facet-level total used for scaling.}
#'   \item{fraction}{Value mapped to the polar radius.}
#'   \item{Value2}{Catch in thousand tonnes (integer).}
#'   \item{sum2}{Total catch in thousand tonnes (integer).}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters \code{x} to MSY lineDescription, pivots
#'     \code{FishingPressure} and \code{StockSize} to a long
#'     \code{Variable}/\code{Colour} format.
#'   \item Counts the number of stocks per \code{Variable} and status
#'     colour, and also sums \code{CATCH} per status colour using
#'     \code{y}.
#'   \item Merges the stock counts and catch totals; the stock counts
#'     are used only for intermediate calculations, while the pies are
#'     drawn using catch as the radius.
#'   \item Computes total catch per metric, derives the share of each
#'     status within each indicator, and converts catch to thousand
#'     tonnes for labelling.
#'   \item Builds a polar bar plot with one pie per
#'     (Metric × Variable) facet and hides axes, showing only colour
#'     and labels.
#'   \item Font sizes and label sizes are scaled based on
#'     \code{width_px} for better responsiveness in Shiny.
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_fill_manual
#'   coord_polar facet_grid labs theme_bw theme element_blank
#'   element_text margin
#' @importFrom dplyr filter select group_by summarise mutate
#' @importFrom tidyr gather spread
#' @importFrom plyr revalue
#' @noRd
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



#' Plot stock status trends by guild with proxy highlighting
#'
#' Creates an interactive two-panel Plotly time-series figure showing
#' stock-level status trajectories for a single fisheries guild.
#' The top panel displays exploitation status (\code{F/F_MS Y}), and
#' the bottom panel displays stock status
#' (\code{SSB/MSY B_trigger}) over time.
#'
#' Lines corresponding to stocks that use proxy reference points are
#' drawn with a dotted linetype, and the hover text explicitly
#' indicates when a proxy is used. A thick black line shows the
#' fisheries-guild mean in each panel (with the last year truncated
#' so it is visually distinct from individual-stock series).
#'
#' Clicking a stock line in either panel highlights the selected stock
#' in both panels and dims all other stocks; double-click clears the
#' selection.
#'
#' @param x A data frame of stock-level metrics, typically the output
#'   of \code{stock_trends_proxy()}, containing at least:
#'   \itemize{
#'     \item \code{StockKeyLabel} – stock name or label.
#'     \item \code{FisheriesGuild} – guild classification.
#'     \item \code{Metric} – one of \code{"F_FMSY"} or
#'       \code{"SSB_MSYBtrigger"} (other metrics are ignored).
#'     \item \code{Year} – assessment year (numeric).
#'     \item \code{Value} – numeric value of the metric.
#'     \item \code{Proxy_is_proxy} – logical flag indicating whether
#'       the metric is based on a proxy reference point for that row.
#'     \item \code{Proxy_name} – optional description of the proxy
#'       reference point (character, may be \code{NA}).
#'   }
#'   Rows with \code{StockKeyLabel == "Mean"} are interpreted as
#'   guild-level means.
#'
#' @param guild Character scalar; the fisheries guild to display
#'   (e.g. \code{"demersal"}, \code{"pelagic"}, etc.). Only rows with
#'   \code{FisheriesGuild == guild} are plotted.
#' @param return_data Logical; if \code{TRUE}, returns the filtered
#*   data frame for the requested guild (with the added
#'   \code{MetricLabel} column) instead of the Plotly object.
#'   Default is \code{FALSE}.
#' @param ecoregion Optional character scalar used to annotate the
#'   title and the export filename (e.g. \code{"Greater North Sea"}).
#'
#' @return
#' If \code{return_data = FALSE} (default), an interactive Plotly
#' htmlwidget consisting of two vertically stacked panels:
#' \itemize{
#'   \item Top panel: \code{F/F_MS Y} time series by stock, with a
#'     horizontal reference line at 1 and a thick black mean line.
#'   \item Bottom panel: \code{SSB/MSY B_trigger} time series by stock,
#'     similarly annotated.
#' }
#'
#' If there are no rows for the requested \code{guild}, a blank Plotly
#' object is returned with a central annotation indicating that no data
#' are available.
#'
#' If \code{return_data = TRUE}, the function returns the subset of
#' \code{x} used for plotting (for the selected guild and metrics),
#' including the derived \code{MetricLabel} column.
#'
#' @details
#' Colours are assigned per \code{StockKeyLabel} using
#' \code{hcl.colors(..., palette = "Temps")}, excluding the guild
#' mean row (\code{"Mean"}). Stocks that rely on proxy reference
#' points (\code{Proxy_is_proxy == TRUE}) are drawn as dotted lines;
#' other stocks are drawn as solid lines.
#'
#' Hover text shows the stock name, year, metric value, and—when
#' applicable—the proxy reference point name. A horizontal reference
#' line at 1 is added in both panels, along with a rectangular border
#' around each subplot.
#'
#' A random Crosstalk group is created per call to ensure linked
#' highlighting across both panels without interference between
#' multiple instances of the plot in a Shiny app. The export filename
#' for the built-in "Download as PNG" button includes the ecoregion,
#' guild name, and current date.
#'
#' @importFrom dplyr filter mutate recode group_by ungroup arrange
#'   slice_head select
#' @importFrom grDevices hcl.colors
#' @importFrom plotly plot_ly add_lines layout subplot highlight
#'   highlight_key config attrs_selected
#' @noRd
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
        title = list(text = "<b>Stock code:</b>", font = list(size = 16)),
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




#' Plot catches and landings by stock with proxy reference-point markers
#'
#' Produces a catch–landings "dumbbell" style plot (per stock) for a
#' given fisheries guild, highlighting whether reference points are
#' based on proxies. Each stock appears as a vertical pair of points
#' for catches and landings (in thousand tonnes) connected to zero by
#' line segments, with colours indicating overall status
#' (e.g. GREEN/RED/GREY).
#'
#' Stocks using proxy reference points are drawn with hollow markers
#' (status-coloured outlines), while stocks using official reference
#' points are drawn with filled markers. A small legend explains the
#' shape/fill convention for landings vs catches and proxy vs
#' non-proxy.
#'
#' @param x A data frame containing stock-level data for one or more
#'   fisheries guilds, with at least the following columns:
#'   \itemize{
#'     \item \code{StockKeyLabel} – stock identifier (character).
#'     \item \code{FisheriesGuild} – guild label(s) (character or factor).
#'     \item \code{Catches} – catches (typically in tonnes; plotted as
#'       \code{Catches/1000}).
#'     \item \code{Landings} – landings (typically in tonnes; plotted as
#'       \code{Landings/1000}).
#'     \item \code{Status} – categorical status code per stock (e.g.
#'       \code{"GREEN"}, \code{"RED"}, \code{"GREY"}), used for colours.
#'     \item \code{F_proxy} – logical (or coercible to logical) flag
#'       indicating that the F-related reference point is a proxy
#'       (may contain \code{NA}).
#'     \item \code{B_proxy} – logical (or coercible to logical) flag
#'       indicating that the B-related reference point is a proxy
#'       (may contain \code{NA}).
#'   }
#'   Other columns are ignored.
#'
#' @param guild Either a single character value or a character vector
#'   of fisheries guilds to include (matched against
#'   \code{FisheriesGuild}). If \code{guild} is exactly \code{"All"},
#'   no filtering by guild is applied and all rows in \code{x} are
#'   used.
#'
#' @param return_data Logical; if \code{TRUE}, the function returns the
#'   processed data frame used for plotting instead of the ggplot
#'   object. The returned data includes:
#'   \itemize{
#'     \item the original columns used in the plot;
#'     \item \code{total} – per-stock maximum of catches or landings
#'       across time (used for ordering);
#'     \item \code{ProxyFlag} – logical flag indicating if either
#'       \code{F_proxy} or \code{B_proxy} is \code{TRUE} for that row.
#'   }
#'   Default is \code{FALSE}.
#'
#' @return
#' If \code{return_data = FALSE} (default), a \code{ggplot} object is
#' returned. The plot shows:
#' \itemize{
#'   \item one horizontal "bar" per stock (after \code{coord_flip()}),
#'     ordered by total volume (smaller stocks at the bottom);
#'   \item coloured segments from 0 to catches/landings (thousand
#'     tonnes) per stock, coloured by \code{Status};
#'   \item filled markers for non-proxy reference points (triangles for
#'     catches, circles for landings);
#'   \item hollow markers with coloured outlines for proxy reference
#'     points;
#'   \item an inset legend describing landings vs catches and proxy vs
#'     non-proxy markers;
#'   \item a caption of the form
#'     \dQuote{ICES Stock Assessment Database, dd-Mmm-yy. ICES, Copenhagen}.
#' }
#'
#' If \code{return_data = TRUE}, the processed data frame (after
#' guild filtering, ordering, and \code{total}/\code{ProxyFlag}
#' computation) is returned instead.
#'
#' @details
#' For each stock, \code{total} is calculated as:
#' \deqn{\max(\text{Catches}, \text{Landings}, \mathrm{na.rm} = TRUE)}
#' across all years in \code{x}. Stocks with missing catches and
#' landings throughout are removed from the plot. The factor ordering
#' of \code{StockKeyLabel} is then set according to \code{total},
#' so that smaller total-volume stocks appear lower in the plot
#' after \code{coord_flip()}.
#'
#' The function expects the columns \code{F_proxy} and \code{B_proxy}
#' to be present. If either is missing, a warning is issued, as this
#' usually indicates an upstream processing problem. The combined
#' \code{ProxyFlag} column is set to \code{TRUE} if either
#' \code{F_proxy} or \code{B_proxy} is \code{TRUE}, and is used to
#' switch between filled vs hollow markers.
#'
#' @importFrom dplyr filter group_by mutate ungroup
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_segment geom_point scale_fill_manual
#'   scale_colour_manual coord_equal coord_flip theme_bw labs theme
#'   element_text element_blank unit guide_legend guides scale_shape_manual
#'   element_rect
#' @noRd
plot_CLD_bar_app <- function(x, guild, return_data = FALSE) {
  # --- Filter by guild
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

   # --- Ensure proxy flags exist  
  if (!"F_proxy" %in% names(df)) warning("Missing 'F_proxy' column in input data. This may indicate an upstream data issue.")  
  if (!"B_proxy" %in% names(df)) warning("Missing 'B_proxy' column in input data. This may indicate an upstream data issue.")  


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
    ggplot2::labs(x = "Stock code", y = "Catch and Landings (thousand tonnes)") +
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
    "Landings \n(Proxy ref. point)"= 21,
    "Catches"         = 24,
    "Catches \n(Proxy ref. point)" = 24
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
                       "Landings \n(Proxy ref. point)" = NA,
                       "Catches"          = "grey60",
                       "Catches \n(Proxy ref. point)"  = NA)[legend_labels],
            colour = c("Landings"         = "grey25",
                       "Landings \n(Proxy ref. point)" = "grey25",
                       "Catches"          = "grey25",
                       "Catches \n(Proxy ref. point)"  = "grey25")[legend_labels],
            stroke = c("Landings"         = 1.0,
                       "Landings \n(Proxy ref. point)" = 2,
                       "Catches"          = 1.0,
                       "Catches \n(Proxy ref. point)"  = 2)[legend_labels],
            alpha  = 1
          ),
          keyheight = ggplot2::unit(30, "pt"),
          keywidth  = ggplot2::unit(30, "pt"),
          byrow = TRUE
        )
      ) +
      ggplot2::theme(
        legend.position      = c(0.98, 0.02),  # bottom-right inside
        legend.justification = c(1, 0),
        legend.background    = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.9),
                                                     colour = "grey85"),
        legend.spacing.y  = ggplot2::unit(10, "pt"),
        legend.key.height    = ggplot2::unit(30, "pt"),
        legend.key.width     = ggplot2::unit(30, "pt")
      )
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  if (isTRUE(return_data)) df else p
}




#' Plot a Kobe-style status diagram with proxy reference-point markers
#'
#' Produces a Kobe-style plot of exploitation status (\eqn{F/F_{MSY}})
#' versus stock status (\eqn{SSB/MSY\,B_{trigger}}) for the latest
#' assessment values of each stock, optionally filtered by fisheries
#' guild. Points are coloured by overall status and their shapes
#' indicate whether the underlying reference points are official or
#' proxy values.
#'
#' Stocks using only official reference points are drawn as filled
#' circles ("Normal refpoint"). Stocks for which either the F-based
#' or B-based reference point is a proxy are drawn as hollow circles
#' with a thicker outline ("Proxy refpoint"). Stock labels are added
#' with repelled text to reduce overlap.
#'
#' @param x A data frame containing stock-level indicators with at
#'   least the following columns:
#'   \itemize{
#'     \item \code{StockKeyLabel} – stock identifier (character).
#'     \item \code{FisheriesGuild} – fisheries guild (character/factor).
#'     \item \code{F_FMSY} – ratio of fishing pressure to \eqn{F_{MSY}}.
#'     \item \code{SSB_MSYBtrigger} – ratio of spawning stock biomass
#'       to \eqn{MSY\,B_{trigger}}.
#'     \item \code{Status} – categorical status code, typically
#'       \code{"GREEN"}, \code{"RED"}, or \code{"GREY"}, used for
#'       point colouring.
#'     \item \code{F_proxy} – logical flag indicating whether the
#'       F-related reference point is a proxy (may contain \code{NA}).
#'     \item \code{B_proxy} – logical flag indicating whether the
#'       B-related reference point is a proxy (may contain \code{NA}).
#'   }
#'   If \code{F_proxy} or \code{B_proxy} are missing, they are created
#'   and set to \code{FALSE} (i.e. treated as normal reference points).
#'
#' @param guild Either a single character value or a character vector
#'   of fisheries guilds to include (matched against
#'   \code{FisheriesGuild}). If \code{guild} is exactly \code{"All"},
#'   no guild-based filtering is applied and all rows in \code{x} are
#'   used.
#'
#' @param return_data Logical; if \code{TRUE}, the processed data
#'   frame used to build the plot (after guild filtering and
#'   \code{ProxyFlag} construction) is returned instead of the
#'   ggplot object. Default is \code{FALSE}.
#'
#' @return
#' If \code{return_data = FALSE} (default), returns a \code{ggplot}
#' object showing stocks in Kobe space with:
#' \itemize{
#'   \item x-axis: \eqn{F/F_{MSY}}.
#'   \item y-axis: \eqn{SSB/MSY\,B_{trigger}}.
#'   \item point colours given by \code{Status}.
#'   \item point shapes indicating "Normal refpoint" vs
#'     "Proxy refpoint", with a legend explaining the distinction.
#'   \item dashed reference lines at 1 on both axes.
#'   \item stock labels plotted with \code{ggrepel::geom_text_repel()}.
#'   \item a standard caption of the form
#'     \dQuote{ICES Stock Assessment Database, dd-Mmm-yy. ICES, Copenhagen}.
#' }
#'
#' If \code{return_data = TRUE}, the function returns the filtered and
#' augmented data frame (including the \code{ProxyFlag} column) instead
#' of plotting.
#'
#' @details
#' The plotting window is automatically expanded to at least \code{[0, 3]}
#' on each axis, or slightly beyond the maximum observed values in
#' \code{F_FMSY} and \code{SSB_MSYBtrigger}. The legend for the
#' reference-point shape drops levels that are not present in the data
#' (e.g. if no proxies are present, only "Normal refpoint" is shown).
#'
#' @importFrom dplyr filter mutate if_else
#' @importFrom ggplot2 ggplot aes coord_cartesian geom_point geom_hline
#'   geom_vline scale_color_manual scale_shape_manual labs theme_bw theme
#'   element_blank element_text element_rect guides guide_legend unit
#' @importFrom ggrepel geom_text_repel
#' @noRd
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
                                 "Proxy reference point", "Reference point")
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
      data = dplyr::filter(df, ProxyFlag == "Reference point"),
      ggplot2::aes(color = Status, shape = ProxyFlag),
      size = pt_size, alpha = 0.7, na.rm = TRUE
    ) +

    # ---- Proxy refpoint (empty circle with thicker outline) ----
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag == "Proxy reference point"),
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
      name   = "ICES reference point",
      values = c("Reference point" = 16,  # filled circle
                 "Proxy reference point"  = 21), # circle with border (uses stroke)
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
      legend.key.height  = ggplot2::unit(30, "pt"),
      legend.key.width   = ggplot2::unit(30, "pt")
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

