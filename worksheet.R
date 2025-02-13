

library(dplyr)
library(future)
library(jsonlite)
library(icesSAG)
library(icesASD)
library(promises)
library(data.table)
library(memoise)
library(future.apply)
library(tidyr)

load_sag_status_new <- function(sag) {
        stocks <- unique(sag[c("AssessmentKey","FishStock")])
        status <- icesSAG::getStockStatusValues(stocks$AssessmentKey)
        status <- do.call(rbind.data.frame, status)
        # stocks$AssessmentKey <- as.character(stocks$AssessmentKey)
        status <- dplyr::left_join(status, stocks)
        status <- dplyr::mutate(status, StockKeyLabel= FishStock)
        status <- subset(status, select = -c(FishStock))
        status <- dplyr::relocate(status, StockKeyLabel, .before = lineNumber)
        status
}

stocks <- icesSAG::getListStocks(year = 2024)
stocks <- unique(stocks[c("AssessmentKey","StockKeyLabel")])
test1 <- icesSAG::getStockStatusValues(stocks$AssessmentKey)
test2 <- do.call(rbind.data.frame, test1)


getSID <- function(year, EcoR) {
    message("Downloading SID data for year: ", year)
    stock_list_long <- jsonlite::fromJSON(
        URLencode(
            sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel, EcoRegion, YearOfLastAssessment, AssessmentKey, FisheriesGuild", year)
        )
    )$value

    stock_list_long %>%
        mutate(EcoRegion = as.character(EcoRegion)) %>%
        separate_rows(EcoRegion, sep = ", ")
    stock_list_long <- stock_list_long %>%
        filter(EcoRegion == EcoR)
    # setDT(stock_list_long)

    # # Get unique valid years (excluding NA and 0)
    # valid_years <- unique(stock_list_long$YearOfLastAssessment)
    # valid_years <- valid_years[!is.na(valid_years) & valid_years != 0]


    # # Parallelized API calls for ASD records
    # ASDList <- rbindlist(future_lapply(valid_years, function(y) {
    #     message("Fetching ASD advice records for year: ", y)
    #     as.data.table(icesASD::getAdviceViewRecord(year = y))
    # }), fill = TRUE)

    # ASDList <- ASDList %>% group_by(stockCode) %>% filter(assessmentYear == max(assessmentYear, na.rm = TRUE, finite = TRUE)) %>% ungroup()
    # ASDList <- ASDList %>% select(stockCode, assessmentKey, adviceComponent, adviceStatus)

    # # Ensure ASDList is a valid data frame
    # if (is.null(ASDList) || identical(ASDList, list()) || nrow(ASDList) == 0) {
    #     ASDList <- data.frame(
    #         StockKeyLabel = character(),
    #         AssessmentKey = character(),
    #         AssessmentComponent = character(),
    #         stringsAsFactors = FALSE
    #     )
    # } else {
    #     ASDList <- ASDList %>%
    #         mutate(adviceComponent = na_if(adviceComponent, "N.A.")) %>%
    #         rename(
    #             StockKeyLabel = stockCode,
    #             AssessmentKey = assessmentKey,
    #             AssessmentComponent = adviceComponent
    #         ) %>%
    #         filter(adviceStatus == "Advice")
    # }
    # setDT(ASDList)

    # message("Merging SID and ASD records...")
    # # Efficient merge using data.table
    # stock_list_long <- ASDList[stock_list_long, on = "StockKeyLabel"]

    # Find missing AssessmentKeys using YearOfLastAssessment
    # browser()
    missing_keys <- which(is.na(stock_list_long$AssessmentKey) &
        !is.na(stock_list_long$YearOfLastAssessment) &
        stock_list_long$YearOfLastAssessment != 0)

    if (length(missing_keys) > 0) {
        message("Finding missing assessment keys...")

        # Retrieve assessment keys (returns list)
        assessment_keys <- lapply(missing_keys, function(i) {
            keys <- icesSAG::findAssessmentKey(stock_list_long$StockKeyLabel[i],
                year = stock_list_long$YearOfLastAssessment[i]
            )
            if (length(keys) > 0) keys[1] else NA # Take only the first key or return NA
        })

        # Convert list to vector and assign
        stock_list_long$AssessmentKey[missing_keys] <- unlist(assessment_keys)
    }

    # Drop rows where AssessmentKey is still NA
    # stock_list_long <- stock_list_long[!is.na(AssessmentKey)]
    stock_list_long <- stock_list_long[!is.na(stock_list_long$AssessmentKey), ]


    message("Data processing complete.")
    return(stock_list_long)
}

df <- getSID(year = 2024, EcoR = "Celtic Seas")
# status <- icesSAG::getStockStatusValues(df$AssessmentKey)
# Set up parallel execution (adjust workers based on system capability)
plan(multisession, workers = parallel::detectCores() - 1)

# Ensure AssessmentKey is unique to avoid redundant API calls
unique_keys <- unique(df$AssessmentKey)

# Fetch stock status values in parallel
status_list <- future_lapply(unique_keys, function(key) {
    tryCatch(
        icesSAG::getStockStatusValues(key),
        error = function(e) {
            message(sprintf("Error fetching data for AssessmentKey: %s", key))
            return(NULL)
        }
    )
})

status <- do.call(rbind, status_list)
# Combine results into a single dataframe
status <- do.call(rbind.data.frame, status)
# Merge stock status values with SID data
df_merged <- merge(df, status, by = "AssessmentKey", all.x = TRUE)
df_merged$FisheriesGuild <- tolower(df_merged$FisheriesGuild)


# format_sag_status_new <- function(x) {
#         df <- x
#         # df <- dplyr::filter(df,(grepl(pattern = ecoregion, Ecoregion)))
#         df <- dplyr::mutate(df,status = case_when(status == 0 ~ "UNDEFINED",
#                                                   status == 1 ~ "GREEN",
#                                                   status == 2 ~ "qual_GREEN", #qualitative green
#                                                   status == 3 ~ "ORANGE",
#                                                   status == 4 ~ "RED",
#                                                   status == 5 ~ "qual_RED", #qualitative red
#                                                   status == 6 ~ "GREY",
#                                                   status == 7 ~ "qual_UP",
#                                                   status == 8 ~ "qual_STEADY",
#                                                   status == 9 ~ "qual_DOWN",
#                                                   TRUE ~ "OTHER"),
#                             fishingPressure = case_when(fishingPressure == "-" &
#                                                                 type == "Fishing pressure" ~ "FQual",
#                                                         TRUE ~ fishingPressure),
#                             stockSize = case_when(stockSize == "-" &
#                                                           type == "Stock Size" ~ "SSBQual",
#                                                   TRUE ~ stockSize),
#                             stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
#                             variable = case_when(type == "Fishing pressure" ~ fishingPressure,
#                                                  type == "Stock Size" ~ stockSize,
#                                                  TRUE ~ type),
#                             variable = case_when(lineDescription == "Management plan" &
#                                                          type == "Fishing pressure" ~ "FMGT",
#                                                  lineDescription == "Management plan" &
#                                                          type == "Stock Size" ~ "SSBMGT",
#                                                  TRUE ~ variable),
#                             variable = case_when(
#                                     grepl("Fpa", variable) ~ "FPA",
#                                     grepl("Bpa", variable) ~ "BPA",
#                                     grepl("^Qual*", variable) ~ "SSBQual",
#                                     grepl("-", variable) ~ "FQual",
#                                     grepl("^BMGT", variable) ~ "SSBMGT",
#                                     grepl("MSYBtrigger", variable) ~ "BMSY",
#                                     grepl("FMSY", variable) ~ "FMSY",
#                                     TRUE ~ variable
#                             )) 
#         df <- dplyr::filter(df,variable != "-")
        
#         df <- dplyr::filter(df, lineDescription != "Management plan")
#         df <- dplyr::filter(df, lineDescription != "Qualitative evaluation")
#         df <- dplyr::mutate(df,key = paste(StockKeyLabel, lineDescription, type))
#         df<- df[order(-df$year),]
#         df <- df[!duplicated(df$key), ]
#         df<- subset(df, select = -key)
#         df<- subset(df, select = c(StockKeyLabel, AssessmentKey,lineDescription, type, status, FisheriesGuild))
#         df<- tidyr::spread(df,type, status)
        
#         df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
#         df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
#         colnames(df2) <- c("StockKeyLabel","AssessmentKey","lineDescription","FishingPressure","StockSize" )
#         df2 <-dplyr::mutate(df2, SBL = case_when(FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
#                                                  FishingPressure == "RED" | StockSize == "RED" ~ "RED",
#                                                  FishingPressure == "ORANGE"  |  StockSize == "ORANGE" ~ "RED",
#                                                  TRUE ~ "GREY"))
#         df2<- subset(df2, select = c(StockKeyLabel, SBL))
#         df <- dplyr::left_join(df, df2)
#         df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
#         df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
#         # colnames(df) <- c("StockKeyLabel","AssessmentYear","AdviceCategory","lineDescription","FishingPressure","StockSize", "SBL" )
#         # sid <- dplyr::select(y,StockKeyLabel,
#         #                      FisheriesGuild)
#         # sid$FisheriesGuild <- tolower(sid$FisheriesGuild)
#         # colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "Ecoregion", "FisheriesGuild")
#         # df <- merge(df, sid, all = FALSE)
#         df
# }

library(dplyr)
library(tidyr)

format_sag_status_new <- function(x) {
    # Ensure column names are valid
    names(x) <- make.names(names(x), unique = TRUE)
    
    df <- x %>%
        mutate(
            status = case_when(
                status == 0 ~ "UNDEFINED",
                status == 1 ~ "GREEN",
                status == 2 ~ "qual_GREEN",  # qualitative green
                status == 3 ~ "ORANGE",
                status == 4 ~ "RED",
                status == 5 ~ "qual_RED",    # qualitative red
                status == 6 ~ "GREY",
                status == 7 ~ "qual_UP",
                status == 8 ~ "qual_STEADY",
                status == 9 ~ "qual_DOWN",
                TRUE ~ "OTHER"
            ),
            fishingPressure = ifelse(fishingPressure == "-" & type == "Fishing pressure", "FQual", fishingPressure),
            stockSize = ifelse(stockSize == "-" & type == "Stock Size", "SSBQual", stockSize),
            stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
            variable = case_when(
                type == "Fishing pressure" ~ fishingPressure,
                type == "Stock Size" ~ stockSize,
                TRUE ~ type
            ),
            variable = case_when(
                lineDescription == "Management plan" & type == "Fishing pressure" ~ "FMGT",
                lineDescription == "Management plan" & type == "Stock Size" ~ "SSBMGT",
                TRUE ~ variable
            ),
            variable = case_when(
                grepl("Fpa", variable) ~ "FPA",
                grepl("Bpa", variable) ~ "BPA",
                grepl("^Qual*", variable) ~ "SSBQual",
                grepl("-", variable) ~ "FQual",
                grepl("^BMGT", variable) ~ "SSBMGT",
                grepl("MSYBtrigger", variable) ~ "BMSY",
                grepl("FMSY", variable) ~ "FMSY",
                TRUE ~ variable
            )
        ) %>%
        filter(variable != "-", 
               !lineDescription %in% c("Management plan", "Qualitative evaluation")) %>%
        mutate(
            key = paste(StockKeyLabel, lineDescription, type)
        ) %>%
        arrange(desc(year)) %>%
        distinct(key, .keep_all = TRUE) %>%
        select(-key) %>%
        pivot_wider(names_from = type, values_from = status)

    # Create and merge SBL status
    df2 <- df %>%
        filter(!lineDescription %in% c("Maximum Sustainable Yield", "Maximum sustainable yield")) %>%
        rename(FishingPressure = `Fishing pressure`, StockSize = `Stock Size`) %>%
        mutate(
            SBL = case_when(
                FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                FishingPressure == "ORANGE" | StockSize == "ORANGE" ~ "RED",
                TRUE ~ "GREY"
            )
        ) %>%
        select(StockKeyLabel, SBL)

    df <- left_join(df, df2, by = "StockKeyLabel") %>%
        mutate(
            lineDescription = gsub("Maximum Sustainable Yield", "Maximum sustainable yield", lineDescription),
            lineDescription = gsub("Precautionary Approach", "Precautionary approach", lineDescription)
        )

    return(df)
}

clean_status <- format_sag_status_new(df_merged)
str(clean_status)
plot_status_prop_pies <- function(x, cap_month = "November",
                         cap_year = "2018",
                         return_data = FALSE) {
        df <- df_merged
        cap_lab <- ggplot2::labs(title = "", x = "", y = "",
                        caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen",
                                          cap_month,
                                          cap_year))
        colList <- c("GREEN" = "#00B26D",
                     "GREY" = "#d3d3d3",
                     "ORANGE" = "#ff7f00",
                     "RED" = "#d93b1c",
                     "qual_RED" = "#d93b1c",
                     "qual_GREEN" = "#00B26D")


        df_stock <- dplyr::select(df,StockKeyLabel,
                       FisheriesGuild,
                       lineDescription,
                       fishingPressure,
                       stockSize)
                    #    SBL)
        df_stock <- tidyr::gather(df_stock,Variable, Colour, fishingPressure:stockSize, factor_key = TRUE)
        df2 <- dplyr::group_by(df_stock, FisheriesGuild, lineDescription, Variable, Colour)
        df2 <- dplyr::summarize(df2, COUNT = dplyr::n())
        df2 <- tidyr::spread(df2, Colour, COUNT)
        df2[is.na(df2)] <- 0
        df3 <- subset(df2,select =-c(FisheriesGuild))
        df3 <- dplyr::group_by(df3,lineDescription, Variable)
        df3 <- dplyr::summarise_each(df3,dplyr::funs(sum))
        df3$FisheriesGuild <- "total"
        df2 <- rbind(df2,df3)

        df4 <- dplyr::filter(df2,Variable == "SBL")
        df4$lineDescription <- ""
        df4 <- unique(df4)
        df2 <- dplyr::filter(df2,Variable != "SBL")
        df2 <- rbind(df2,df4)
        df2$lineDescription <- gsub("Maximum sustainable yield","MSY", df2$lineDescription)
        df2$lineDescription <- gsub("Precautionary approach", "PA", df2$lineDescription)
        df2$header <- paste0(df2$Variable, "\n" , df2$lineDescription)

        df2 <- tidyr::gather(df2,colour, value,GREEN:RED, factor_key = TRUE)
        df2 <- dplyr::filter(df2,value > 0)


        tot <- dplyr::filter(df2,FisheriesGuild == "total")
        tot <- dplyr::group_by(tot,header)
        tot <- dplyr::mutate(tot, tot = sum(value))
        max <- unique(tot$tot)
        df2 <- dplyr::group_by(df2, FisheriesGuild, header)
        df2 <- dplyr::mutate(df2,sum = sum(value))
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
                          size = 3) +
                ggplot2::scale_fill_manual(values = colList) +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(panel.grid = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      legend.position="none") +
                ggplot2::theme(axis.text=ggplot2::element_blank(),
                      axis.ticks=ggplot2::element_blank(),
                      strip.background = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 6)) +
                cap_lab +
                ggplot2::coord_polar(theta = "y", direction = 1) +
                ggplot2::facet_grid(FisheriesGuild ~ header)

        if(return_data == T){
                df2
        }else{
                p1
        }
}
library(dplyr)
library(tidyr)
library(ggplot2)

plot_status_prop_pies <- function(x, cap_month = "November", cap_year = "2018", return_data = FALSE) {
    cap_lab <- labs(
        title = "", x = "", y = "",
        caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen", cap_month, cap_year)
    )

    colList <- c(
        "GREEN" = "#00B26D", "GREY" = "#d3d3d3", "ORANGE" = "#ff7f00",
        "RED" = "#d93b1c", "qual_RED" = "#d93b1c", "qual_GREEN" = "#00B26D"
    )

    df2 <- x %>%
        select(StockKeyLabel, FisheriesGuild, lineDescription, fishingPressure, stockSize, SBL) %>%
        pivot_longer(cols = fishingPressure:stockSize, names_to = "Variable", values_to = "Colour") %>%
        group_by(FisheriesGuild, lineDescription, Variable, Colour) %>%
        summarise(COUNT = n(), .groups = "drop") %>%
        pivot_wider(names_from = Colour, values_from = COUNT, values_fill = 0) %>%
        ungroup()

    df3 <- df2 %>%
        select(-FisheriesGuild) %>%
        group_by(lineDescription, Variable) %>%
        summarise(across(everything(), sum), .groups = "drop") %>%
        mutate(FisheriesGuild = "total")

    df2 <- bind_rows(df2, df3)

    df4 <- df2 %>%
        filter(Variable == "SBL") %>%
        mutate(lineDescription = "") %>%
        distinct()

    df2 <- df2 %>%
        filter(Variable != "SBL") %>%
        bind_rows(df4) %>%
        mutate(
            lineDescription = recode(lineDescription, "Maximum sustainable yield" = "MSY", "Precautionary approach" = "PA"),
            header = paste0(Variable, "\n", lineDescription)
        ) %>%
        pivot_longer(cols = GREEN:RED, names_to = "colour", values_to = "value") %>%
        filter(value > 0)

    tot <- df2 %>%
        filter(FisheriesGuild == "total") %>%
        group_by(header) %>%
        mutate(tot = sum(value)) %>%
        ungroup()

    max_tot <- unique(tot$tot)

    df2 <- df2 %>%
        group_by(FisheriesGuild, header) %>%
        mutate(
            sum = sum(value),
            fraction = value * max_tot / sum
        ) %>%
        ungroup() %>%
        mutate(
            header = factor(header, levels = c("fishingPressure\nMSY", "stockSize\nMSY", "fishingPressure\nPA", "stockSize\nPA", "SBL\n")),
            FisheriesGuild = factor(tolower(FisheriesGuild), levels = c("total", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))
        )

    p1 <- ggplot(df2, aes(x = "", y = fraction, fill = colour)) +
        geom_bar(stat = "identity", width = 1) +
        geom_text(aes(label = value), position = position_stack(vjust = 0.5), size = 3) +
        scale_fill_manual(values = colList) +
        theme_bw(base_size = 9) +
        theme(
            panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
            legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(),
            strip.background = element_blank(), plot.caption = element_text(size = 6)
        ) +
        cap_lab +
        coord_polar(theta = "y", direction = 1) +
        facet_grid(FisheriesGuild ~ header)

    if (return_data) {
        return(df2)
    } else {
        return(p1)
    }
}
str(clean_status)
plot_status_prop_pies(clean_status)


plot_status_prop_pies <- function(x, cap_month = "November", cap_year = "2018", return_data = FALSE) {
    cap_lab <- labs(
        title = "", x = "", y = "",
        caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen", cap_month, cap_year)
    )

    colList <- c(
        "GREEN" = "#00B26D", "GREY" = "#d3d3d3", "ORANGE" = "#ff7f00",
        "RED" = "#d93b1c", "qual_RED" = "#d93b1c", "qual_GREEN" = "#00B26D"
    )

    df2 <- x %>%
        select(StockKeyLabel, FisheriesGuild, lineDescription, fishingPressure, stockSize, SBL) %>%
        pivot_longer(cols = c(fishingPressure, stockSize), names_to = "Variable", values_to = "Colour") %>%
        group_by(FisheriesGuild, lineDescription, Variable, Colour) %>%
        summarise(COUNT = n(), .groups = "drop") %>%
        pivot_wider(names_from = Colour, values_from = COUNT, values_fill = list(COUNT = 0)) %>%
        ungroup()

    # Ensure all expected color columns exist
    expected_cols <- c("GREEN", "GREY", "ORANGE", "RED", "qual_RED", "qual_GREEN")
    for (col in expected_cols) {
        if (!(col %in% colnames(df2))) {
            df2[[col]] <- 0
        }
    }

    df3 <- df2 %>%
        select(-FisheriesGuild) %>%
        group_by(lineDescription, Variable) %>%
        summarise(across(all_of(expected_cols), sum), .groups = "drop") %>%
        mutate(FisheriesGuild = "total")

    df2 <- bind_rows(df2, df3)

    df4 <- df2 %>%
        filter(Variable == "SBL") %>%
        mutate(lineDescription = "") %>%
        distinct()

    df2 <- df2 %>%
        filter(Variable != "SBL") %>%
        bind_rows(df4) %>%
        mutate(
            lineDescription = recode(lineDescription, "Maximum sustainable yield" = "MSY", "Precautionary approach" = "PA"),
            header = paste0(Variable, "\n", lineDescription)
        ) %>%
        pivot_longer(cols = all_of(expected_cols), names_to = "colour", values_to = "value") %>%
        filter(value > 0)

    tot <- df2 %>%
        filter(FisheriesGuild == "total") %>%
        group_by(header) %>%
        mutate(tot = sum(value)) %>%
        ungroup()

    max_tot <- unique(tot$tot)

    df2 <- df2 %>%
        group_by(FisheriesGuild, header) %>%
        mutate(
            sum = sum(value),
            fraction = ifelse(sum == 0, 0, value * max_tot / sum)  # Avoid division by zero
        ) %>%
        ungroup() %>%
        mutate(
            header = factor(header, levels = c("fishingPressure\nMSY", "stockSize\nMSY", "fishingPressure\nPA", "stockSize\nPA", "SBL\n")),
            FisheriesGuild = factor(tolower(FisheriesGuild), levels = c("total", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))
        )

    p1 <- ggplot(df2, aes(x = "", y = fraction, fill = colour)) +
        geom_bar(stat = "identity", width = 1) +
        geom_text(aes(label = value), position = position_stack(vjust = 0.5), size = 3) +
        scale_fill_manual(values = colList) +
        theme_bw(base_size = 9) +
        theme(
            panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
            legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(),
            strip.background = element_blank(), plot.caption = element_text(size = 6)
        ) +
        cap_lab +
        coord_polar(theta = "y", direction = 1) +
        facet_grid(FisheriesGuild ~ header)

    if (return_data) {
        return(df2)
    } else {
        return(p1)
    }
}
