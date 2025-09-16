

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
library(jsonlite)

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
            sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel, EcoRegion, YearOfLastAssessment, AssessmentKey, AdviceCategory, FisheriesGuild", year)
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

df <- getSID(year = 2024, EcoR = "Greater North Sea")
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

# read rda file
load("data/clean_status.rda")
str(clean_status)
names(clean_status)
names(df_merged)
format_sag_status_new <- function(x) {
        df <- x
        # df <- dplyr::filter(df,(grepl(pattern = ecoregion, Ecoregion)))
        df <- dplyr::mutate(df,status = case_when(status == 0 ~ "UNDEFINED",
                                                  status == 1 ~ "GREEN",
                                                  status == 2 ~ "qual_GREEN", #qualitative green
                                                  status == 3 ~ "ORANGE",
                                                  status == 4 ~ "RED",
                                                  status == 5 ~ "qual_RED", #qualitative red
                                                  status == 6 ~ "GREY",
                                                  status == 7 ~ "qual_UP",
                                                  status == 8 ~ "qual_STEADY",
                                                  status == 9 ~ "qual_DOWN",
                                                  TRUE ~ "OTHER"),
                            fishingPressure = case_when(fishingPressure == "-" &
                                                                type == "Fishing pressure" ~ "FQual",
                                                        TRUE ~ fishingPressure),
                            stockSize = case_when(stockSize == "-" &
                                                          type == "Stock Size" ~ "SSBQual",
                                                  TRUE ~ stockSize),
                            stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
                            variable = case_when(type == "Fishing pressure" ~ fishingPressure,
                                                 type == "Stock Size" ~ stockSize,
                                                 TRUE ~ type),
                            variable = case_when(lineDescription == "Management plan" &
                                                         type == "Fishing pressure" ~ "FMGT",
                                                 lineDescription == "Management plan" &
                                                         type == "Stock Size" ~ "SSBMGT",
                                                 TRUE ~ variable),
                            variable = case_when(
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
        df<- df[order(-df$year),]
        df <- df[!duplicated(df$key), ]
        df<- subset(df, select = -key)
        df<- subset(df, select = c(StockKeyLabel, AssessmentKey,lineDescription, type, status, FisheriesGuild))
        df<- tidyr::spread(df,type, status)
        
        df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
        df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
        colnames(df2) <- c("StockKeyLabel","AssessmentKey","lineDescription","FisheriesGuild","FishingPressure","StockSize" )
        df2 <-dplyr::mutate(df2, SBL = case_when(FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                                                 FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                                                 FishingPressure == "ORANGE"  |  StockSize == "ORANGE" ~ "RED",
                                                 TRUE ~ "GREY"))
        df2<- subset(df2, select = c(StockKeyLabel, SBL))
        df <- dplyr::left_join(df, df2)
        df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
        df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
        # colnames(df) <- c("StockKeyLabel","AssessmentYear","AdviceCategory","lineDescription","FishingPressure","StockSize", "SBL" )
        # sid <- dplyr::select(y,StockKeyLabel,
        #                      FisheriesGuild)
        # sid$FisheriesGuild <- tolower(sid$FisheriesGuild)
        # colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "Ecoregion", "FisheriesGuild")
        # df <- merge(df, sid, all = FALSE)
        return(df)
}

# library(dplyr)
# library(tidyr)

# format_sag_status_new <- function(x) {
#     # Ensure column names are valid
#     names(x) <- make.names(names(x), unique = TRUE)
    
#     df <- x %>%
#         mutate(
#             status = case_when(
#                 status == 0 ~ "UNDEFINED",
#                 status == 1 ~ "GREEN",
#                 status == 2 ~ "qual_GREEN",  # qualitative green
#                 status == 3 ~ "ORANGE",
#                 status == 4 ~ "RED",
#                 status == 5 ~ "qual_RED",    # qualitative red
#                 status == 6 ~ "GREY",
#                 status == 7 ~ "qual_UP",
#                 status == 8 ~ "qual_STEADY",
#                 status == 9 ~ "qual_DOWN",
#                 TRUE ~ "OTHER"
#             ),
#             fishingPressure = ifelse(fishingPressure == "-" & type == "Fishing pressure", "FQual", fishingPressure),
#             stockSize = ifelse(stockSize == "-" & type == "Stock Size", "SSBQual", stockSize),
#             stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
#             variable = case_when(
#                 type == "Fishing pressure" ~ fishingPressure,
#                 type == "Stock Size" ~ stockSize,
#                 TRUE ~ type
#             ),
#             variable = case_when(
#                 lineDescription == "Management plan" & type == "Fishing pressure" ~ "FMGT",
#                 lineDescription == "Management plan" & type == "Stock Size" ~ "SSBMGT",
#                 TRUE ~ variable
#             ),
#             variable = case_when(
#                 grepl("Fpa", variable) ~ "FPA",
#                 grepl("Bpa", variable) ~ "BPA",
#                 grepl("^Qual*", variable) ~ "SSBQual",
#                 grepl("-", variable) ~ "FQual",
#                 grepl("^BMGT", variable) ~ "SSBMGT",
#                 grepl("MSYBtrigger", variable) ~ "BMSY",
#                 grepl("FMSY", variable) ~ "FMSY",
#                 TRUE ~ variable
#             )
#         ) %>%
#         filter(variable != "-", 
#                !lineDescription %in% c("Management plan", "Qualitative evaluation")) %>%
#         mutate(
#             key = paste(StockKeyLabel, lineDescription, type)
#         ) %>%
#         arrange(desc(year)) %>%
#         distinct(key, .keep_all = TRUE) %>%
#         select(-key) %>%
#         pivot_wider(names_from = type, values_from = status)

#     # Create and merge SBL status
#     df2 <- df %>%
#         filter(!lineDescription %in% c("Maximum Sustainable Yield", "Maximum sustainable yield")) %>%
#         rename(FishingPressure = `Fishing pressure`, StockSize = `Stock Size`) %>%
#         mutate(
#             SBL = case_when(
#                 FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
#                 FishingPressure == "RED" | StockSize == "RED" ~ "RED",
#                 FishingPressure == "ORANGE" | StockSize == "ORANGE" ~ "RED",
#                 TRUE ~ "GREY"
#             )
#         ) %>%
#         select(StockKeyLabel, SBL)

#     df <- left_join(df, df2, by = "StockKeyLabel") %>%
#         mutate(
#             lineDescription = gsub("Maximum Sustainable Yield", "Maximum sustainable yield", lineDescription),
#             lineDescription = gsub("Precautionary Approach", "Precautionary approach", lineDescription)
#         )

#     return(df)
# }

cleanStatus <- format_sag_status_new(df_merged)
str(cleanStatus)
str(clean_status)
names(cleanStatus)
colnames(cleanStatus) <- c("StockKeyLabel","AssessmentKey","lineDescription","FisheriesGuild","FishingPressure","StockSize" , "SBL")

plot_status_prop_pies <- function(x, cap_month = "November",
                         cap_year = "2018",
                         return_data = FALSE) {
        df <- x
        colnames(df) <- c("StockKeyLabel","AssessmentKey","lineDescription","FisheriesGuild","FishingPressure","StockSize" , "SBL")
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
                       FishingPressure,
                       StockSize,
                       SBL)
        df_stock <- tidyr::gather(df_stock,Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)
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
                # cap_lab +
                ggplot2::coord_polar(theta = "y", direction = 1) +
                ggplot2::facet_grid(FisheriesGuild ~ header)
                
        # ggplotly(p1)     
        # browser()
        # Create a list of separate plots for each header when interactive mode is enabled
        
        # browser()
        # plot_list <- df2 %>%
        # split(list(.$FisheriesGuild, .$header)) %>%
        # lapply(function(data) {
        #     if (nrow(data) > 0) {
        #         plot_ly(data, labels = ~colour, values = ~fraction, type = 'pie',
        #                 textinfo = 'label+percent', marker = list(colors = colList)) %>%
        #             layout(title = paste(data$FisheriesGuild[1], "-", data$header[1]))
        #     } else {
        #         NULL
        #     }
        # }) %>%
        # purrr::compact() 

        # # Ensure no NA values are passed to layout
        # plot_list <- lapply(plot_list, function(p) {
        #     if (!is.null(p)) {
        #         p$x$layout <- Filter(Negate(is.na), p$x$layout)
        #     }
        #     p
        # })

        # # Determine the number of rows and columns for the subplot
        # n_plots <- length(plot_list)
        # n_cols <- 5
        # n_rows <- 6#ceiling(n_plots / n_cols)
        
        # return(subplot(plot_list, nrows = n_rows, ncols = n_cols, shareX = TRUE, shareY = TRUE))
        if(return_data == T){
                df2
        }else{
                p1
        }
}

plot_status_prop_pies(cleanStatus, cap_month = "November", cap_year = "2018", return_data = FALSE) 


plot_status_prop_pies <- function(x, cap_month = "November", cap_year = "2018", return_data = FALSE) {
    df <- x %>%
        setNames(c("StockKeyLabel", "AssessmentKey", "lineDescription", "FisheriesGuild", "FishingPressure", "StockSize", "SBL"))
    
    colList <- c("GREEN" = "#00B26D", "GREY" = "#d3d3d3", "ORANGE" = "#ff7f00", "RED" = "#d93b1c",
                 "qual_RED" = "#d93b1c", "qual_GREEN" = "#00B26D")
    
    df_stock <- df %>%
        select(StockKeyLabel, FisheriesGuild, lineDescription, FishingPressure, StockSize, SBL) %>%
        pivot_longer(cols = FishingPressure:StockSize, names_to = "Variable", values_to = "Colour")
    
    df2 <- df_stock %>%
        count(FisheriesGuild, lineDescription, Variable, Colour) %>%
        pivot_wider(names_from = Colour, values_from = n, values_fill = list(n = 0))
    
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
    
    df2 <- df2 %>% filter(Variable != "SBL") %>% bind_rows(df4)
    
    df2 <- df2 %>%
        mutate(lineDescription = recode(lineDescription, "Maximum sustainable yield" = "MSY", "Precautionary approach" = "PA"),
               header = paste(Variable, "\n", lineDescription)) %>%
        pivot_longer(cols = GREEN:RED, names_to = "colour", values_to = "value") %>%
        filter(value > 0)
    
    tot <- df2 %>%
        filter(FisheriesGuild == "total") %>%
        group_by(header) %>%
        summarise(tot = sum(value), .groups = "drop")
    
    df2 <- df2 %>%
        left_join(tot, by = "header") %>%
        group_by(FisheriesGuild, header) %>%
        mutate(sum = sum(value), fraction = ifelse(sum > 0, value * tot / sum, 0)) %>%
        ungroup() %>%
        mutate(
            header = factor(header, levels = c("FishingPressure\nMSY", "StockSize\nMSY", "FishingPressure\nPA", "StockSize\nPA", "SBL\n")),
            FisheriesGuild = factor(tolower(FisheriesGuild), levels = c("total", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))
        )
    
    if (return_data) return(df2)
    
    # Generate interactive pie charts for each combination of FisheriesGuild and header
    plot_list <- df2 %>%
        split(list(.$FisheriesGuild, .$header)) %>%
        lapply(function(data) {
            if (nrow(data) > 0) {
                plot_ly(data, labels = ~colour, values = ~fraction, type = 'pie',
                        textinfo = 'label+percent', marker = list(colors = colList)) %>%
                    layout(title = paste(data$FisheriesGuild[1], "-", data$header[1]))
            } else {
                NULL
            }
        }) %>%
        purrr::compact() # Remove NULL elements
    
    if (length(plot_list) == 0) {
        stop("No valid data to plot.")
    }
    
    return(subplot(plot_list, nrows = 6, shareX = TRUE, shareY = TRUE))
}


clean_status <- format_sag_status_new(getStatus(year = 2024, EcoR = "Greater North Sea"))

format_annex_table <- function(status, year) {
  # sid <- getSD(year)
#   sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
#   sid <- dplyr::select(sid,
#                        StockKeyLabel,
#                        StockKeyDescription,
#                        SpeciesScientificName,
#                        SpeciesCommonName,
#                        # FisheriesGuild,
#                        DataCategory)
#   sid <- sid %>% filter(StockKeyLabel %in% df$StockKeyLabel)
#   df <- dplyr::left_join(df, sid, by = "StockKeyLabel")
  # df <- df[c(1,10,11,12,13,14,2,3,4,5,8,6,7)]
  year <- 2024
  status <- clean_status
  sid <- jsonlite::fromJSON(
                URLencode(
                        sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel, 
                        EcoRegion, 
                        YearOfLastAssessment, 
                        AssessmentKey,
                        StockKeyDescription,
                        SpeciesScientificName,
                        SpeciesCommonName, 
                        AdviceCategory, 
                        FisheriesGuild", year)
                )
        )$value
    sid <- sid %>% filter(StockKeyLabel %in% status$StockKeyLabel)
  df <- dplyr::left_join(status, sid, by = "StockKeyLabel")
    # status <- test
  df <- dplyr::mutate(df,
                      D3C1 = FishingPressure,
                      D3C2 = StockSize,
                      GES = dplyr::case_when(
                          FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                          FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                          FishingPressure == "GREY" | StockSize == "GREY" ~ "GREY",
                          TRUE ~ "GREY"))


  df$StockKeyDescription <- gsub("\\s*\\([^\\)]+\\)","",df$StockKeyDescription, perl = TRUE)

  df
}


getSAG_ecoregion <- function(year, ecoregion, sid){
        years <- ((year-4):year)
        ecoreg <- gsub(" ", "%20", ecoregion, fixed = TRUE)
        # sid <- icesSD::getSD(NULL,year)
        out <- data.frame()
        res <- data.frame()
        for(n in 1:5){
                x <- years[n]
                url <- paste0("https://sag.ices.dk/SAG_API/api/SAGDownload?year=", x, "&EcoRegion=", ecoreg)
                tmpSAG <- tempfile(fileext = ".zip")
                download.file(url, destfile = tmpSAG, mode = "wb", quiet = FALSE)
                names <-unzip(tmpSAG, list = TRUE)
                res <- read.csv(unz(tmpSAG, names$Name[1]),
                                stringsAsFactors = FALSE,
                                header = TRUE,
                                fill = TRUE)
                res<- unique(res)
                out <- rbind(out, res)
        }
        out <- dplyr::filter(out, Purpose == "Advice")
        out <- data.table::as.data.table(out) 
        # out <- out[out[, .I[AssessmentKey == max(AssessmentKey)], by=FishStock]$V1]
        out <- out[out[, .I[AssessmentYear == max(AssessmentYear)], by=FishStock]$V1]
        out <- as.data.frame(out)
        out <- dplyr::filter(out,out$FishStock %in% sid$StockKeyLabel)
}

format_sag <- function(sag,sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        sag <- dplyr::mutate(sag, StockKeyLabel=FishStock)
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        # df1 <- left_join(x, y)
        # df1 <- left_join(x, y, by = c("StockKeyLabel", "AssessmentYear"))
        df1 <-as.data.frame(df1)
        
        df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        df1 <- subset(df1, select = -c(FishStock))
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        # check <-unique(df1[c("StockKeyLabel", "FisheriesGuild")])
        out <- dplyr::anti_join(df1, check)
}
sag_complete_frmt <- format_sag(sag, sid)

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

sag_catch_current <- stockstatus_CLD_current(sag_complete_frmt)


# function to download data from github
download_github_data <- function(repo_owner, repo_name, file_path) {
    # Fetch file metadata from GitHub API
    response <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}", 
                   owner = repo_owner, 
                   repo = repo_name, 
                   path = file_path)
    
    # Extract raw file URL
    download_url <- response$download_url
    
    # Download and read the file
    df <- read.csv(download_url)
    
    return(df)
}

NrS_catchScenarioStk <- download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")
NrS_catchRange <- download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice","shiny/Figure1_HeadlinePlot_advice.csv")
save(NrS_catchScenarioStk, file = "D:/GitHub_2023/fisheriesXplorer/data/NrS_catchScenarioStk.rda")
save(NrS_catchRange, file = "D:/GitHub_2023/fisheriesXplorer/data/NrS_catchRange.rda")


devtools::load_all(); run_app()




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
#       
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
library(dplyr)
sid <- getSID(2025, "Baltic Sea")
status <- getStatusWebService("Baltic Sea", sid)
clean_status <- format_sag_status_new(status)

df <- status





library(dplyr)  # for %>%, group_by(), mutate(), slice()
library(plotly)

data(iris)
iris1 <- iris %>%
  group_by(Species) %>%
  mutate(PL = mean(Petal.Length), PW = mean(Petal.Width)) %>%
  highlight_key(~Species) 

fig1 <- plot_ly(
  x = ~Petal.Length, 
  y = ~Petal.Width, 
  type  = "scatter",
  mode  = "markers",
  color = ~Species,
  data  = iris1)

fig2 <- plot_ly(data = iris1) %>%  # initiate plot with same data frame
  slice(1) %>%                     # use dplyr verb on plotly object
  add_markers(
    x     = ~PL,
    y     = ~PW,
    color = ~Species)

subplot(fig1, fig2)






library(shiny)
library(ggplot2)
library(rmarkdown)

# --- Module 1: Plot ---
mod_plot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

mod_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({ data.frame(x = 1:10, y = (1:10)^2) })
    plt <- reactive({
      ggplot(data(), aes(x, y)) + geom_line(color = "blue") + theme_minimal()
    })
    output$plot <- renderPlot({ plt() })
    
    reactive({
      list(
        name = "Plot Module",
        text = "This plot shows y = x² for x from 1 to 10.",
        tables = list(),
        plots = list(plt())
      )
    })
  })
}

# --- Module 2: Table ---
mod_table_ui <- function(id) {
  ns <- NS(id)
  tableOutput(ns("table"))
}

mod_table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({ head(mtcars, 5) })
    output$table <- renderTable({ data() })
    
    reactive({
      list(
        name = "Table Module",
        text = "This table shows the first 5 rows of the mtcars dataset.",
        tables = list(data()),
        plots = list()
      )
    })
  })
}

# --- App UI ---
ui <- fluidPage(
  titlePanel("Multi-module Shiny app with Snapshot Report"),
  fluidRow(
    column(6, mod_plot_ui("plotModule")),
    column(6, mod_table_ui("tableModule"))
  ),
  hr(),
  downloadButton("downloadReport", "Download Snapshot PDF")
)

# --- App Server ---
server <- function(input, output, session) {
  module1 <- mod_plot_server("plotModule")
  module2 <- mod_table_server("tableModule")
  
  all_modules <- reactive({
    list(
      module1(),
      module2()
    )
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("snapshot-", Sys.Date(), ".html")
    },
    content = function(file) {
      rmarkdown::render("report.Rmd",
                        output_file = file,
                        params = list(modules = all_modules()),
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)






# app.R
# Packages ----
library(shiny)
library(pins)
library(dplyr)

# -------------------------------------------------------------------
# CONFIG — set these in your real app
CODE_DOI <- "10.5281/zenodo.CODE_DOI_PLACEHOLDER"
DATA_DOI <- "10.5281/zenodo.DATA_DOI_PLACEHOLDER"
APP_TITLE <- "fisheriesXplorer (demo: versioned data & citation)"
APP_AUTHORS <- "Santos da Costa, P., et al."
APP_URL_BASE <- "http://localhost:xxxx"  # replaced by deployed URL on shinyapps.io
# -------------------------------------------------------------------

# Board setup ----
# For production on shinyapps.io, replace with an external, persistent, versioned board, e.g.:
# board <- pins::board_s3(
#   bucket  = Sys.getenv("S3_BUCKET"),
#   prefix  = "fisheriesxplorer",
#   versioned = TRUE
# )
# Or Posit Connect: board_connect(server = ..., key = ..., versioned = TRUE)
# For this demo we use a temporary (ephemeral) board and seed two months of data:
board <- pins::board_temp(versioned = TRUE)

# Seed demo datasets (two versions with different snapshot_dates) ----
seed_demo_pins <- function(board) {
  if (!("landings" %in% pins::pin_list(board))) {
    # Landings (monthly updates)
    land_aug <- data.frame(stock = c("COD", "HAD", "POK"),
                           landings_t  = c(1200, 800, 450))
    attr(land_aug, "snapshot_date") <- as.Date("2025-08-01")
    pins::pin_write(board, land_aug, name = "landings", type = "rds", versioned = TRUE)

    land_sep <- data.frame(stock = c("COD", "HAD", "POK"),
                           landings_t  = c(1100, 870, 520))
    attr(land_sep, "snapshot_date") <- as.Date("2025-09-01")
    pins::pin_write(board, land_sep, name = "landings", type = "rds", versioned = TRUE)

    # Stock status (annual updates)
    ss_2024 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Healthy", "Cautious", "Healthy"))
    attr(ss_2024, "snapshot_date") <- as.Date("2024-10-01")
    pins::pin_write(board, ss_2024, name = "stock_status", type = "rds", versioned = TRUE)

    ss_2025 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Cautious", "Cautious", "Healthy"))
    attr(ss_2025, "snapshot_date") <- as.Date("2025-09-01")
    pins::pin_write(board, ss_2025, name = "stock_status", type = "rds", versioned = TRUE)
  }
}
seed_demo_pins(board)

# Helpers: discover available snapshot dates by reading each version's embedded date ----
available_snapshot_dates <- function(board, names) {
  dates <- c()
  for (nm in names) {
    vers <- pins::pin_versions(board, nm)
    for (v in vers$version) {
      x <- pins::pin_read(board, nm, version = v)
      sd <- attr(x, "snapshot_date")
      if (!is.null(sd)) dates <- c(dates, sd)
    }
  }
  sort(unique(as.Date(dates)))
}

# choose version by the embedded snapshot_date (<= as_of) ----
choose_version_by_snapshot <- function(board, name, as_of_date) {
  vers <- pins::pin_versions(board, name)
  if (!nrow(vers)) stop("No versions found for pin: ", name)
  # Inspect each version's embedded snapshot_date
  candidates <- lapply(vers$version, function(v) {
    x <- pins::pin_read(board, name, version = v)
    sd <- attr(x, "snapshot_date")
    if (is.null(sd)) return(NULL)
    if (as.Date(sd) <= as.Date(as_of_date)) {
      list(version = v, snapshot_date = as.Date(sd))
    } else NULL
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No ", name, " snapshot available on/before ", as_of_date)
  # Pick the most recent snapshot_date
  o <- order(sapply(candidates, function(z) z$snapshot_date))
  candidates[[tail(o, 1)]]
}

# read pin "as of" and return both data and version metadata ----
read_pin_as_of <- function(board, name, as_of_date) {
  sel <- choose_version_by_snapshot(board, name, as_of_date)
  data <- pins::pin_read(board, name, version = sel$version)
  list(data = data, version = sel$version, snapshot_date = sel$snapshot_date)
}

# UI ----
ui <- function(request) {
  shiny::navbarPage(
    title = APP_TITLE,
    id = "nav",
    header = NULL,
    footer = NULL,
    inverse = FALSE,

    # ---- Data view tab
    tabPanel("Explore",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            wellPanel(
              h4("View data as of"),
              # Populate choices from available snapshots
              uiOutput("as_of_ui"),
              br(),
              bookmarkButton(id = "share_btn", label = "Share this view"),
              helpText("Creates a URL that reproduces this exact view.")
            ),
            wellPanel(
              h4("Version & Citation"),
              uiOutput("provenance_ui"),
              tags$hr(),
              h5("Suggested citation"),
              verbatimTextOutput("citation_text", placeholder = TRUE),
              h5("BibTeX"),
              verbatimTextOutput("bibtex_text", placeholder = TRUE)
            )
          ),
          column(
            width = 9,
            tabsetPanel(
              tabPanel("Landings",
                tableOutput("landings_tbl")
              ),
              tabPanel("Stock status",
                tableOutput("status_tbl")
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
server <- function(input, output, session) {

  # All snapshot dates available across pins in this app
  all_dates <- available_snapshot_dates(board, c("landings", "stock_status"))

  # Build "as of" selector (restrict to known snapshots to make behavior clear)
  output$as_of_ui <- renderUI({
    # honor ?as_of=YYYY-MM-DD in query string
    qs <- shiny::parseQueryString(session$clientData$url_search)
    default_date <- if (!is.null(qs$as_of)) as.Date(qs$as_of) else tail(all_dates, 1)
    selectInput("as_of", "Snapshot date:", choices = all_dates, selected = default_date)
  })

  # Reactively read each dataset as of the chosen date
  landings_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "landings", as.Date(input$as_of))
  })

  status_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "stock_status", as.Date(input$as_of))
  })

  # Tables
  output$landings_tbl <- renderTable({
    landings_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$status_tbl <- renderTable({
    status_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Provenance / Version & Citation panel
  output$provenance_ui <- renderUI({
    req(input$as_of)
    l <- landings_asof()
    s <- status_asof()
    tagList(
      p(HTML(sprintf("<b>Code DOI:</b> %s<br/><b>Data DOI / manifest:</b> %s",
                     CODE_DOI, DATA_DOI))),
      p(HTML(sprintf("<b>View date:</b> %s", as.character(as.Date(input$as_of))))),
      tags$details(
        tags$summary("Pin versions used"),
        tags$ul(
          tags$li(HTML(sprintf("<b>landings</b>: version <code>%s</code> (snapshot %s)",
                               l$version, as.character(l$snapshot_date)))),
          tags$li(HTML(sprintf("<b>stock_status</b>: version <code>%s</code> (snapshot %s)",
                               s$version, as.character(s$snapshot_date))))
        )
      ),
      p(HTML("<i>These version IDs allow exact re-loading of the same data bytes.</i>"))
    )
  })

  # Citation strings
  build_citation <- function(as_of, url) {
    sprintf("%s (Snapshot %s). *%s* [Shiny app]. Code: %s; Data: %s. %s",
            APP_AUTHORS, as.character(as_of), APP_TITLE, CODE_DOI, DATA_DOI, url)
  }

  build_bibtex <- function(as_of, url) {
    yy <- format(as_of, "%Y"); mm <- as.integer(format(as_of, "%m"))
    sprintf("@software{fisheriesxplorer_%s_%02d,\n  author  = {%s},\n  title   = {%s},\n  year    = {%s},\n  month   = {%d},\n  version = {%s},\n  doi     = {%s},\n  url     = {%s}\n}",
            yy, mm, APP_AUTHORS, APP_TITLE, yy, mm,
            paste0(format(as_of, "%Y"), ".", format(as_of, "%m")),
            DATA_DOI, url)
  }

  # Produce a bookmark URL that also includes ?as_of=...
  observeEvent(input$share_btn, {
    # Ensure the as_of param is in the query string before bookmarking
    qs <- paste0("?as_of=", as.character(as.Date(input$as_of)))
    shiny::updateQueryString(qs, mode = "replace", session = session)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    showModal(modalDialog(
      title = "Shareable link",
      easyClose = TRUE,
      footer = NULL,
      p("Anyone opening this link will see exactly this view:"),
      tags$code(url),
      tags$hr(),
      p("Copy the citation below into your manuscript or email:"),
      tags$pre(build_citation(as.Date(input$as_of), url))
    ))
  })

  output$citation_text <- renderText({
    # Build a live URL reflecting current ?as_of param (when local, fall back to query string)
    qs <- paste0("?as_of=", as.character(as.Date(input$as_of)))
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_citation(as.Date(input$as_of), curr_url)
  })

  output$bibtex_text <- renderText({
    qs <- paste0("?as_of=", as.character(as.Date(input$as_of)))
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_bibtex(as.Date(input$as_of), curr_url)
  })

}

# Bookmarking (URL) ----
enableBookmarking("url")

# Run ----
shinyApp(ui, server)


















library(shiny)
library(pins)
library(dplyr)

# -------------------------------------------------------------------
# CONFIG — set these in your real app
CODE_DOI <- "10.5281/zenodo.CODE_DOI_PLACEHOLDER"
DATA_DOI <- "10.5281/zenodo.DATA_DOI_PLACEHOLDER"
APP_TITLE <- "fisheriesXplorer (demo: versioned data & citation)"
APP_AUTHORS <- "Santos da Costa, P., et al."
APP_URL_BASE <- "http://localhost:xxxx"  # replaced by deployed URL on shinyapps.io
# -------------------------------------------------------------------

# (helpers just for pretty labels in the citation)
tab_label <- function(val) switch(val,
  landings = "Landings",
  stock_status = "Stock status",
  val
)
subtab_label <- function(val) switch(val,
  landings_overview = "Overview",
  landings_details  = "Details",
  status_overview   = "Overview",
  status_details    = "Details",
  val
)

# Board setup ----
board <- pins::board_temp(versioned = TRUE)

# Seed demo datasets (two versions with different snapshot_dates) ----
seed_demo_pins <- function(board) {
  if (!("landings" %in% pins::pin_list(board))) {
    # Landings (monthly updates)
    land_aug <- data.frame(stock = c("COD", "HAD", "POK"),
                           landings_t  = c(1200, 800, 450))
    attr(land_aug, "snapshot_date") <- as.Date("2025-08-01")
    pins::pin_write(board, land_aug, name = "landings", type = "rds", versioned = TRUE)

    land_sep <- data.frame(stock = c("COD", "HAD", "POK"),
                           landings_t  = c(1100, 870, 520))
    attr(land_sep, "snapshot_date") <- as.Date("2025-09-01")
    pins::pin_write(board, land_sep, name = "landings", type = "rds", versioned = TRUE)

    # Stock status (annual updates)
    ss_2024 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Healthy", "Cautious", "Healthy"))
    attr(ss_2024, "snapshot_date") <- as.Date("2024-10-01")
    pins::pin_write(board, ss_2024, name = "stock_status", type = "rds", versioned = TRUE)

    ss_2025 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Cautious", "Cautious", "Healthy"))
    attr(ss_2025, "snapshot_date") <- as.Date("2025-09-01")
    pins::pin_write(board, ss_2025, name = "stock_status", type = "rds", versioned = TRUE)
  }
}
seed_demo_pins(board)

# Helpers: discover available snapshot dates by reading each version's embedded date ----
available_snapshot_dates <- function(board, names) {
  dates <- c()
  for (nm in names) {
    vers <- pins::pin_versions(board, nm)
    for (v in vers$version) {
      x <- pins::pin_read(board, nm, version = v)
      sd <- attr(x, "snapshot_date")
      if (!is.null(sd)) dates <- c(dates, sd)
    }
  }
  sort(unique(as.Date(dates)))
}

# choose version by the embedded snapshot_date (<= as_of) ----
choose_version_by_snapshot <- function(board, name, as_of_date) {
  vers <- pins::pin_versions(board, name)
  if (!nrow(vers)) stop("No versions found for pin: ", name)
  # Inspect each version's embedded snapshot_date
  candidates <- lapply(vers$version, function(v) {
    x <- pins::pin_read(board, name, version = v)
    sd <- attr(x, "snapshot_date")
    if (is.null(sd)) return(NULL)
    if (as.Date(sd) <= as.Date(as_of_date)) {
      list(version = v, snapshot_date = as.Date(sd))
    } else NULL
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No ", name, " snapshot available on/before ", as_of_date)
  # Pick the most recent snapshot_date
  o <- order(sapply(candidates, function(z) z$snapshot_date))
  candidates[[tail(o, 1)]]
}

# read pin "as of" and return both data and version metadata ----
read_pin_as_of <- function(board, name, as_of_date) {
  sel <- choose_version_by_snapshot(board, name, as_of_date)
  data <- pins::pin_read(board, name, version = sel$version)
  list(data = data, version = sel$version, snapshot_date = sel$snapshot_date)
}

# UI ----
ui <- function(request) {
  shiny::navbarPage(
    title = APP_TITLE,
    id = "nav",

    # ---- Data view tab
    tabPanel("Explore",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            wellPanel(
              h4("View data as of"),
              # Populate choices from available snapshots
              uiOutput("as_of_ui"),
              br(),
              bookmarkButton(id = "share_btn", label = "Share this view"),
              helpText("Creates a URL that reproduces this exact view.")
            ),
            wellPanel(
              h4("Version & Citation"),
              uiOutput("provenance_ui"),
              tags$hr(),
              h5("Suggested citation"),
              verbatimTextOutput("citation_text", placeholder = TRUE),
              h5("BibTeX"),
              verbatimTextOutput("bibtex_text", placeholder = TRUE)
            )
          ),
          column(
            width = 9,
            # Give the tabset an id and stable values so we can permalink the active tab
            tabsetPanel(id = "tabs",
              tabPanel(title = "Landings", value = "landings",
                # (optional) a sub-tabset so we can also permalink a sub-view
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "landings_overview",
                           tableOutput("landings_tbl")),
                  tabPanel(title = "Details",  value = "landings_details",
                           tableOutput("landings_tbl_details"))
                )
              ),
              tabPanel(title = "Stock status", value = "stock_status",
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "status_overview",
                           tableOutput("status_overview_tbl")),
                  tabPanel(title = "Details",  value = "status_details",
                           tableOutput("status_tbl"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
server <- function(input, output, session) {

  # All snapshot dates available across pins in this app
  all_dates <- available_snapshot_dates(board, c("landings", "stock_status"))

  # Build "as of" selector (restrict to known snapshots to make behavior clear)
  output$as_of_ui <- renderUI({
    # honor ?as_of=YYYY-MM-DD in the query string
    qs <- shiny::parseQueryString(session$clientData$url_search)
    default_date <- if (!is.null(qs$as_of)) as.Date(qs$as_of) else tail(all_dates, 1)

    # also honor ?tab= and ?subtab= on first load
    if (!is.null(qs$tab))    updateTabsetPanel(session, "tabs",    selected = qs$tab)
    if (!is.null(qs$subtab)) updateTabsetPanel(session, "subtabs", selected = qs$subtab)

    selectInput("as_of", "Snapshot date:", choices = all_dates, selected = default_date)
  })

  # Reactively read each dataset as of the chosen date
  landings_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "landings", as.Date(input$as_of))
  })
  status_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "stock_status", as.Date(input$as_of))
  })

  # Tables
  output$landings_tbl <- renderTable({
    landings_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # (same data in the Details subtab, just to show the mechanism)
  output$landings_tbl_details <- renderTable({
    landings_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$status_tbl <- renderTable({
    status_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Simple overview table for status (count by status)
  output$status_overview_tbl <- renderTable({
    status_asof()$data %>% count(status, name = "n")
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Provenance / Version & Citation panel
  output$provenance_ui <- renderUI({
    req(input$as_of)
    l <- landings_asof()
    s <- status_asof()
    tagList(
      p(HTML(sprintf("<b>Code DOI:</b> %s<br/><b>Data DOI / manifest:</b> %s",
                     CODE_DOI, DATA_DOI))),
      p(HTML(sprintf("<b>View date:</b> %s", as.character(as.Date(input$as_of))))),
      p(HTML(sprintf("<b>View:</b> %s — %s",
                     tab_label(input$tabs), subtab_label(input$subtabs)))),
      tags$details(
        tags$summary("Pin versions used"),
        tags$ul(
          tags$li(HTML(sprintf("<b>landings</b>: version <code>%s</code> (snapshot %s)",
                               l$version, as.character(l$snapshot_date)))),
          tags$li(HTML(sprintf("<b>stock_status</b>: version <code>%s</code> (snapshot %s)",
                               s$version, as.character(s$snapshot_date))))
        )
      ),
      p(HTML("<i>These version IDs allow exact re-loading of the same data bytes.</i>"))
    )
  })

  # Citation strings (now include tab + subtab in the URL)
  build_citation <- function(as_of, url, tabs, subtabs) {
    sprintf("%s (Snapshot %s). *%s* [Shiny app]. Code: %s; Data: %s. View: %s — %s. %s",
            APP_AUTHORS, as.character(as_of), APP_TITLE, CODE_DOI, DATA_DOI,
            tab_label(tabs), subtab_label(subtabs), url)
  }

  build_bibtex <- function(as_of, url, tabs, subtabs) {
    yy <- format(as_of, "%Y"); mm <- as.integer(format(as_of, "%m"))
    note <- paste0("View: ", tab_label(tabs), " — ", subtab_label(subtabs))
    sprintf("@software{fisheriesxplorer_%s_%02d,\n  author  = {%s},\n  title   = {%s},\n  year    = {%s},\n  month   = {%d},\n  version = {%s},\n  doi     = {%s},\n  url     = {%s},\n  note    = {%s}\n}",
            yy, mm, APP_AUTHORS, APP_TITLE, yy, mm,
            paste0(format(as_of, "%Y"), ".", format(as_of, "%m")),
            DATA_DOI, url, note)
  }

  # Produce a bookmark URL that also includes ?as_of=...&tab=...&subtab=...
  observeEvent(input$share_btn, {
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    shiny::updateQueryString(qs, mode = "replace", session = session)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    showModal(modalDialog(
      title = "Shareable link",
      easyClose = TRUE,
      footer = NULL,
      p("Anyone opening this link will see exactly this view:"),
      tags$code(url),
      tags$hr(),
      p("Copy the citation below into your manuscript or email:"),
      tags$pre(build_citation(as.Date(input$as_of), url, input$tabs, input$subtabs))
    ))
  })

  output$citation_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_citation(as.Date(input$as_of), curr_url, input$tabs, input$subtabs)
  })

  output$bibtex_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_bibtex(as.Date(input$as_of), curr_url, input$tabs, input$subtabs)
  })

}

# Bookmarking (URL) ----
enableBookmarking("url")

# Run ----
shinyApp(ui, server)

###############azure v
library(shiny)
library(pins)
library(dplyr)
library(AzureStor)   # <-- NEW

# -------------------------------------------------------------------
# CONFIG — set these in your real app
CODE_DOI <- "10.5281/zenodo.CODE_DOI_PLACEHOLDER"
DATA_DOI <- "10.5281/zenodo.DATA_DOI_PLACEHOLDER"
APP_TITLE <- "fisheriesXplorer (demo: versioned data & citation)"
APP_AUTHORS <- "Santos da Costa, P., et al."
APP_URL_BASE <- "http://localhost:xxxx"  # replaced by deployed URL on shinyapps.io
PINS_PATH <- "fisheriesxplorer/pins"     # <-- Azure “prefix” inside the container
# -------------------------------------------------------------------

# (helpers just for pretty labels in the citation)
tab_label <- function(val) switch(val,
  landings = "Landings",
  stock_status = "Stock status",
  val
)
subtab_label <- function(val) switch(val,
  landings_overview = "Overview",
  landings_details  = "Details",
  status_overview   = "Overview",
  status_details    = "Details",
  val
)

# -----------------------
# Board setup (Azure or fallback)
# -----------------------
use_azure <- nzchar(Sys.getenv("AZURE_CONTAINER_URL")) && nzchar(Sys.getenv("AZURE_SAS_RO"))

if (use_azure) {
  # Azure read-only connection (SAS)
  container <- AzureStor::storage_container(
    endpoint = Sys.getenv("AZURE_CONTAINER_URL"),  # e.g. https://acct.dfs.core.windows.net/container
    sas      = Sys.getenv("AZURE_SAS_RO")          # SAS with sp=r
  )
  board <- pins::board_azure(container = container, path = PINS_PATH, versioned = TRUE)
  SEED_DEMO <- FALSE
} else {
  # Fallback: local temp board (seed demo data so the app runs out of the box)
  board <- pins::board_temp(versioned = TRUE)
  SEED_DEMO <- TRUE
}

# Seed demo datasets (only used in fallback mode) ----
seed_demo_pins <- function(board) {
  if (!("landings" %in% pins::pin_list(board))) {
    # Landings (monthly updates)
    land_aug <- data.frame(stock = c("COD", "HAD", "POK"),
                           landings_t  = c(1200, 800, 450))
    attr(land_aug, "snapshot_date") <- as.Date("2025-08-01")
    pins::pin_write(board, land_aug, name = "landings", type = "rds", versioned = TRUE)

    land_sep <- data.frame(stock = c("COD", "HAD", "POK"),
                           landings_t  = c(1100, 870, 520))
    attr(land_sep, "snapshot_date") <- as.Date("2025-09-01")
    pins::pin_write(board, land_sep, name = "landings", type = "rds", versioned = TRUE)

    # Stock status (annual updates)
    ss_2024 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Healthy", "Cautious", "Healthy"))
    attr(ss_2024, "snapshot_date") <- as.Date("2024-10-01")
    pins::pin_write(board, ss_2024, name = "stock_status", type = "rds", versioned = TRUE)

    ss_2025 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Cautious", "Cautious", "Healthy"))
    attr(ss_2025, "snapshot_date") <- as.Date("2025-09-01")
    pins::pin_write(board, ss_2025, name = "stock_status", type = "rds", versioned = TRUE)
  }
}
if (SEED_DEMO) seed_demo_pins(board)

# Helpers: discover available snapshot dates by reading each version's embedded date ----
available_snapshot_dates <- function(board, names) {
  dates <- c()
  for (nm in names) {
    vers <- pins::pin_versions(board, nm)
    for (v in vers$version) {
      x <- pins::pin_read(board, nm, version = v)
      sd <- attr(x, "snapshot_date")
      if (!is.null(sd)) dates <- c(dates, sd)
    }
  }
  sort(unique(as.Date(dates)))
}

# choose version by the embedded snapshot_date (<= as_of) ----
choose_version_by_snapshot <- function(board, name, as_of_date) {
  vers <- pins::pin_versions(board, name)
  if (!nrow(vers)) stop("No versions found for pin: ", name)
  # Inspect each version's embedded snapshot_date
  candidates <- lapply(vers$version, function(v) {
    x <- pins::pin_read(board, name, version = v)
    sd <- attr(x, "snapshot_date")
    if (is.null(sd)) return(NULL)
    if (as.Date(sd) <= as.Date(as_of_date)) {
      list(version = v, snapshot_date = as.Date(sd))
    } else NULL
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No ", name, " snapshot available on/before ", as_of_date)
  # Pick the most recent snapshot_date
  o <- order(sapply(candidates, function(z) z$snapshot_date))
  candidates[[tail(o, 1)]]
}

# read pin "as of" and return both data and version metadata ----
read_pin_as_of <- function(board, name, as_of_date) {
  sel <- choose_version_by_snapshot(board, name, as_of_date)
  data <- pins::pin_read(board, name, version = sel$version)
  list(data = data, version = sel$version, snapshot_date = sel$snapshot_date)
}

# UI ----
ui <- function(request) {
  shiny::navbarPage(
    title = APP_TITLE,
    id = "nav",

    # ---- Data view tab
    tabPanel("Explore",
      fluidPage(
        fluidRow(
          column(
            width = 3,
            wellPanel(
              h4("View data as of"),
              # Populate choices from available snapshots
              uiOutput("as_of_ui"),
              br(),
              bookmarkButton(id = "share_btn", label = "Share this view"),
              helpText("Creates a URL that reproduces this exact view.")
            ),
            wellPanel(
              h4("Version & Citation"),
              uiOutput("provenance_ui"),
              tags$hr(),
              h5("Suggested citation"),
              verbatimTextOutput("citation_text", placeholder = TRUE),
              h5("BibTeX"),
              verbatimTextOutput("bibtex_text", placeholder = TRUE)
            )
          ),
          column(
            width = 9,
            # Give the tabset an id and stable values so we can permalink the active tab
            tabsetPanel(id = "tabs",
              tabPanel(title = "Landings", value = "landings",
                # (optional) a sub-tabset so we can also permalink a sub-view
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "landings_overview",
                           tableOutput("landings_tbl")),
                  tabPanel(title = "Details",  value = "landings_details",
                           tableOutput("landings_tbl_details"))
                )
              ),
              tabPanel(title = "Stock status", value = "stock_status",
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "status_overview",
                           tableOutput("status_overview_tbl")),
                  tabPanel(title = "Details",  value = "status_details",
                           tableOutput("status_tbl"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
server <- function(input, output, session) {

  # All snapshot dates available across pins in this app
  all_dates <- available_snapshot_dates(board, c("landings", "stock_status"))

  # Build "as of" selector (restrict to known snapshots to make behavior clear)
  output$as_of_ui <- renderUI({
    # honor ?as_of=YYYY-MM-DD in the query string
    qs <- shiny::parseQueryString(session$clientData$url_search)
    default_date <- if (!is.null(qs$as_of)) as.Date(qs$as_of) else tail(all_dates, 1)

    # also honor ?tab= and ?subtab= on first load
    if (!is.null(qs$tab))    updateTabsetPanel(session, "tabs",    selected = qs$tab)
    if (!is.null(qs$subtab)) updateTabsetPanel(session, "subtabs", selected = qs$subtab)

    selectInput("as_of", "Snapshot date:", choices = all_dates, selected = default_date)
  })

  # Reactively read each dataset as of the chosen date
  landings_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "landings", as.Date(input$as_of))
  })
  status_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "stock_status", as.Date(input$as_of))
  })

  # Tables
  output$landings_tbl <- renderTable({
    landings_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # (same data in the Details subtab, just to show the mechanism)
  output$landings_tbl_details <- renderTable({
    landings_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$status_tbl <- renderTable({
    status_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Simple overview table for status (count by status)
  output$status_overview_tbl <- renderTable({
    status_asof()$data %>% count(status, name = "n")
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Provenance / Version & Citation panel
  output$provenance_ui <- renderUI({
    req(input$as_of)
    l <- landings_asof()
    s <- status_asof()
    storage_str <- if (use_azure) {
      paste0("<b>Storage:</b> Azure container (", htmltools::htmlEscape(Sys.getenv("AZURE_CONTAINER_URL")),
             "), path <code>", PINS_PATH, "</code>")
    } else {
      "<b>Storage:</b> local temporary board (demo fallback)"
    }
    tagList(
      p(HTML(storage_str)),
      p(HTML(sprintf("<b>Code DOI:</b> %s<br/><b>Data DOI / manifest:</b> %s",
                     CODE_DOI, DATA_DOI))),
      p(HTML(sprintf("<b>View date:</b> %s", as.character(as.Date(input$as_of))))),
      p(HTML(sprintf("<b>View:</b> %s — %s",
                     tab_label(input$tabs), subtab_label(input$subtabs)))),
      tags$details(
        tags$summary("Pin versions used"),
        tags$ul(
          tags$li(HTML(sprintf("<b>landings</b>: version <code>%s</code> (snapshot %s)",
                               l$version, as.character(l$snapshot_date)))),
          tags$li(HTML(sprintf("<b>stock_status</b>: version <code>%s</code> (snapshot %s)",
                               s$version, as.character(s$snapshot_date))))
        )
      ),
      p(HTML("<i>These version IDs allow exact re-loading of the same data bytes.</i>"))
    )
  })

  # Citation strings (include tab + subtab in the URL)
  build_citation <- function(as_of, url, tabs, subtabs) {
    sprintf("%s (Snapshot %s). *%s* [Shiny app]. Code: %s; Data: %s. View: %s — %s. %s",
            APP_AUTHORS, as.character(as_of), APP_TITLE, CODE_DOI, DATA_DOI,
            tab_label(tabs), subtab_label(subtabs), url)
  }

  build_bibtex <- function(as_of, url, tabs, subtabs) {
    yy <- format(as_of, "%Y"); mm <- as.integer(format(as_of, "%m"))
    note <- paste0("View: ", tab_label(tabs), " — ", subtab_label(subtabs))
    sprintf("@software{fisheriesxplorer_%s_%02d,\n  author  = {%s},\n  title   = {%s},\n  year    = {%s},\n  month   = {%d},\n  version = {%s},\n  doi     = {%s},\n  url     = {%s},\n  note    = {%s}\n}",
            yy, mm, APP_AUTHORS, APP_TITLE, yy, mm,
            paste0(format(as_of, "%Y"), ".", format(as_of, "%m")),
            DATA_DOI, url, note)
  }

  # Produce a bookmark URL that also includes ?as_of=...&tab=...&subtab=...
  observeEvent(input$share_btn, {
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of))),
      # add active tab + subtab
    qs <- paste0(
      qs,
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    shiny::updateQueryString(qs, mode = "replace", session = session)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    showModal(modalDialog(
      title = "Shareable link",
      easyClose = TRUE,
      footer = NULL,
      p("Anyone opening this link will see exactly this view:"),
      tags$code(url),
      tags$hr(),
      p("Copy the citation below into your manuscript or email:"),
      tags$pre(build_citation(as.Date(input$as_of), url, input$tabs, input$subtabs))
    ))
  })

  output$citation_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_citation(as.Date(input$as_of), curr_url, input$tabs, input$subtabs)
  })

  output$bibtex_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_bibtex(as.Date(input$as_of), curr_url, input$tabs, input$subtabs)
  })

}

# Bookmarking (URL) ----
enableBookmarking("url")

# Run ----
shinyApp(ui, server)
# -------------------------------------------------------------------






########### asyncronous data updating
library(shiny)
library(pins)
library(dplyr)
library(AzureStor)   # Azure support

# -------------------------------------------------------------------
# CONFIG — set these in your real app
CODE_DOI <- "10.5281/zenodo.CODE_DOI_PLACEHOLDER"
DATA_DOI <- "10.5281/zenodo.DATA_DOI_PLACEHOLDER"
APP_TITLE <- "fisheriesXplorer (demo: versioned data & citation)"
APP_AUTHORS <- "Santos da Costa, P., et al."
APP_URL_BASE <- "http://localhost:xxxx"  # replaced by deployed URL on shinyapps.io
PINS_PATH <- "fisheriesxplorer/pins"     # Azure “prefix” inside the container
# -------------------------------------------------------------------

# Cadence registry (used for UI + citation)
DATASETS <- list(
  landings     = list(label = "Landings",     cadence = "annual"),
  stock_status = list(label = "Stock status", cadence = "daily")
)

# (helpers just for pretty labels in the citation)
tab_label <- function(val) switch(val,
  landings = "Landings",
  stock_status = "Stock status",
  val
)
subtab_label <- function(val) switch(val,
  landings_overview = "Overview",
  landings_details  = "Details",
  status_overview   = "Overview",
  status_details    = "Details",
  val
)

# -----------------------
# Board setup (Azure or fallback)
# -----------------------
use_azure <- nzchar(Sys.getenv("AZURE_CONTAINER_URL")) && nzchar(Sys.getenv("AZURE_SAS_RO"))

if (use_azure) {
  container <- AzureStor::storage_container(
    endpoint = Sys.getenv("AZURE_CONTAINER_URL"),  # e.g. https://acct.dfs.core.windows.net/container
    sas      = Sys.getenv("AZURE_SAS_RO")          # SAS with sp=r
  )
  board <- pins::board_azure(container = container, path = PINS_PATH, versioned = TRUE)
  SEED_DEMO <- FALSE
} else {
  # Fallback: local temp board (seed demo data so the app runs out of the box)
  board <- pins::board_temp(versioned = TRUE)
  SEED_DEMO <- TRUE
}

# Seed demo datasets (only used in fallback mode) ----
# IMPORTANT: reflect cadences
# - status => daily snapshots (two different days)
# - landings => annual snapshots (Jan 01 for each year)
seed_demo_pins <- function(board) {
  if (!("landings" %in% pins::pin_list(board))) {
    # Landings (ANNUAL updates → snapshot_date on Jan 01)
    land_2024 <- data.frame(stock = c("COD", "HAD", "POK"),
                            landings_t  = c(1250, 820, 480))
    attr(land_2024, "snapshot_date") <- as.Date("2024-01-01")
    pins::pin_write(board, land_2024, name = "landings", type = "rds", versioned = TRUE)

    land_2025 <- data.frame(stock = c("COD", "HAD", "POK"),
                            landings_t  = c(1100, 900, 530))
    attr(land_2025, "snapshot_date") <- as.Date("2025-01-01")
    pins::pin_write(board, land_2025, name = "landings", type = "rds", versioned = TRUE)

    # Stock status (DAILY updates → snapshot_date for specific days)
    ss_0914 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Healthy", "Cautious", "Healthy"))
    attr(ss_0914, "snapshot_date") <- as.Date("2025-09-14")
    pins::pin_write(board, ss_0914, name = "stock_status", type = "rds", versioned = TRUE)

    ss_0915 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Cautious", "Cautious", "Healthy"))
    attr(ss_0915, "snapshot_date") <- as.Date("2025-09-15")
    pins::pin_write(board, ss_0915, name = "stock_status", type = "rds", versioned = TRUE)
  }
}
if (SEED_DEMO) seed_demo_pins(board)

# Helpers: discover available snapshot dates by reading each version's embedded date ----
available_snapshot_dates <- function(board, names) {
  dates <- c()
  for (nm in names) {
    vers <- pins::pin_versions(board, nm)
    for (v in vers$version) {
      x <- pins::pin_read(board, nm, version = v)
      sd <- attr(x, "snapshot_date")
      if (!is.null(sd)) dates <- c(dates, sd)
    }
  }
  sort(unique(as.Date(dates)))
}

# choose version by the embedded snapshot_date (<= as_of) ----
choose_version_by_snapshot <- function(board, name, as_of_date) {
  vers <- pins::pin_versions(board, name)
  if (!nrow(vers)) stop("No versions found for pin: ", name)
  # Inspect each version's embedded snapshot_date
  candidates <- lapply(vers$version, function(v) {
    x <- pins::pin_read(board, name, version = v)
    sd <- attr(x, "snapshot_date")
    if (is.null(sd)) return(NULL)
    if (as.Date(sd) <= as.Date(as_of_date)) {
      list(version = v, snapshot_date = as.Date(sd))
    } else NULL
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No ", name, " snapshot available on/before ", as_of_date)
  # Pick the most recent snapshot_date
  o <- order(sapply(candidates, function(z) z$snapshot_date))
  candidates[[tail(o, 1)]]
}

# read pin "as of" and return both data and version metadata ----
read_pin_as_of <- function(board, name, as_of_date) {
  sel <- choose_version_by_snapshot(board, name, as_of_date)
  data <- pins::pin_read(board, name, version = sel$version)
  list(data = data, version = sel$version, snapshot_date = sel$snapshot_date)
}

# UI ----
ui <- function(request) {
  shiny::navbarPage(
    title = APP_TITLE,
    id = "nav",

    # ---- Data view tab
    tabPanel("Explore",
      fluidPage(
        fluidRow(
          column(
            width = 8,
            wellPanel(
              h4("View data as of"),
              # Populate choices from available snapshots
              uiOutput("as_of_ui"),
              br(),
              bookmarkButton(id = "share_btn", label = "Share this view"),
              helpText("Creates a URL that reproduces this exact view.")
            ),
            wellPanel(
              h4("Version & Citation"),
              uiOutput("provenance_ui"),
              tags$hr(),
              h5("Suggested citation"),
              verbatimTextOutput("citation_text", placeholder = TRUE),
              h5("BibTeX"),
              verbatimTextOutput("bibtex_text", placeholder = TRUE)
            )
          ),
          column(
            width = 4,
            # Give the tabset an id and stable values so we can permalink the active tab
            tabsetPanel(id = "tabs",
              tabPanel(title = "Landings", value = "landings",
                # (optional) a sub-tabset so we can also permalink a sub-view
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "landings_overview",
                           tableOutput("landings_tbl")),
                  tabPanel(title = "Details",  value = "landings_details",
                           tableOutput("landings_tbl_details"))
                )
              ),
              tabPanel(title = "Stock status", value = "stock_status",
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "status_overview",
                           tableOutput("status_overview_tbl")),
                  tabPanel(title = "Details",  value = "status_details",
                           tableOutput("status_tbl"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
server <- function(input, output, session) {

  # All snapshot dates available across pins in this app
  all_dates <- available_snapshot_dates(board, c("landings", "stock_status"))

  # Build "as of" selector (restrict to known snapshots to make behavior clear)
  output$as_of_ui <- renderUI({
    # honor ?as_of=YYYY-MM-DD in the query string
    qs <- shiny::parseQueryString(session$clientData$url_search)
    default_date <- if (!is.null(qs$as_of)) as.Date(qs$as_of) else tail(all_dates, 1)

    # also honor ?tab= and ?subtab= on first load
    if (!is.null(qs$tab))    updateTabsetPanel(session, "tabs",    selected = qs$tab)
    if (!is.null(qs$subtab)) updateTabsetPanel(session, "subtabs", selected = qs$subtab)

    selectInput("as_of", "Snapshot date:", choices = all_dates, selected = default_date)
  })

  # Reactively read each dataset as of the chosen date
  landings_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "landings", as.Date(input$as_of))
  })
  status_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "stock_status", as.Date(input$as_of))
  })

  # Tables
  output$landings_tbl <- renderTable({
    landings_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # (same data in the Details subtab, just to show the mechanism)
  output$landings_tbl_details <- renderTable({
    landings_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$status_tbl <- renderTable({
    status_asof()$data
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Simple overview table for status (count by status)
  output$status_overview_tbl <- renderTable({
    status_asof()$data %>% count(status, name = "n")
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Provenance / Version & Citation panel (now shows cadence per dataset)
  output$provenance_ui <- renderUI({
    req(input$as_of)
    l <- landings_asof()
    s <- status_asof()
    storage_str <- if (use_azure) {
      paste0("<b>Storage:</b> Azure container (", htmltools::htmlEscape(Sys.getenv("AZURE_CONTAINER_URL")),
             "), path <code>", PINS_PATH, "</code>")
    } else {
      "<b>Storage:</b> local temporary board (demo fallback)"
    }
    tagList(
      p(HTML(storage_str)),
      p(HTML(sprintf("<b>Code DOI:</b> %s<br/><b>Data DOI / manifest:</b> %s",
                     CODE_DOI, DATA_DOI))),
      p(HTML(sprintf("<b>View date:</b> %s", as.character(as.Date(input$as_of))))),
      p(HTML(sprintf("<b>View:</b> %s — %s",
                     tab_label(input$tabs), subtab_label(input$subtabs)))),
      tags$details(
        tags$summary("Data currency by dataset"),
        tags$ul(
          tags$li(HTML(sprintf("<b>%s</b> — cadence: %s; snapshot: %s; pin version: <code>%s</code>",
                               DATASETS$landings$label, DATASETS$landings$cadence,
                               as.character(l$snapshot_date), l$version))),
          tags$li(HTML(sprintf("<b>%s</b> — cadence: %s; snapshot: %s; pin version: <code>%s</code>",
                               DATASETS$stock_status$label, DATASETS$stock_status$cadence,
                               as.character(s$snapshot_date), s$version)))
        )
      ),
      p(HTML("<i>These version IDs allow exact re-loading of the same data bytes.</i>"))
    )
  })

  # Citation strings (include tab + subtab + per-dataset cadence & snapshot)
  build_citation <- function(as_of, url, tabs, subtabs, land_obj, status_obj) {
    sprintf(
      paste0("%s (view as of %s). *%s* [Shiny app]. ",
             "Code: %s; Data: %s. View: %s — %s. ",
             "Snapshots — %s (%s): %s [pin %s]; %s (%s): %s [pin %s]. %s"),
      APP_AUTHORS, as.character(as.Date(as_of)), APP_TITLE,
      CODE_DOI, DATA_DOI, tab_label(tabs), subtab_label(subtabs),
      DATASETS$landings$label, DATASETS$landings$cadence,
      as.character(land_obj$snapshot_date), land_obj$version,
      DATASETS$stock_status$label, DATASETS$stock_status$cadence,
      as.character(status_obj$snapshot_date), status_obj$version,
      url
    )
  }

  build_bibtex <- function(as_of, url, tabs, subtabs, land_obj, status_obj) {
    yy <- format(as_of, "%Y"); mm <- as.integer(format(as_of, "%m"))
    note <- paste0("View: ", tab_label(tabs), " — ", subtab_label(subtabs),
                   "; Snapshots — ",
                   DATASETS$landings$label, " (", DATASETS$landings$cadence, "): ",
                   as.character(land_obj$snapshot_date), " [pin ", land_obj$version, "]; ",
                   DATASETS$stock_status$label, " (", DATASETS$stock_status$cadence, "): ",
                   as.character(status_obj$snapshot_date), " [pin ", status_obj$version, "]")
    sprintf("@software{fisheriesxplorer_%s_%02d,\n  author  = {%s},\n  title   = {%s},\n  year    = {%s},\n  month   = {%d},\n  version = {%s},\n  doi     = {%s},\n  url     = {%s},\n  note    = {%s}\n}",
            yy, mm, APP_AUTHORS, APP_TITLE, yy, mm,
            paste0(format(as_of, "%Y"), ".", format(as_of, "%m")),
            DATA_DOI, url, note)
  }

  # Produce a bookmark URL that also includes ?as_of=...&tab=...&subtab=...
  observeEvent(input$share_btn, {
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    shiny::updateQueryString(qs, mode = "replace", session = session)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    showModal(modalDialog(
      title = "Shareable link",
      easyClose = TRUE,
      footer = NULL,
      p("Anyone opening this link will see exactly this view:"),
      tags$code(url),
      tags$hr(),
      p("Copy the citation below into your manuscript or email:"),
      tags$pre(build_citation(as.Date(input$as_of), url, input$tabs, input$subtabs,
                              landings_asof(), status_asof()))
    ))
  })

  output$citation_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_citation(as.Date(input$as_of), curr_url, input$tabs, input$subtabs,
                   landings_asof(), status_asof())
  })

  output$bibtex_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_bibtex(as.Date(input$as_of), curr_url, input$tabs, input$subtabs,
                 landings_asof(), status_asof())
  })

}

# Bookmarking (URL) ----
enableBookmarking("url")

# Run ----
shinyApp(ui, server)







######################################################
# app.R — fisheriesXplorer demo: Azure + versioned pins + time-travel + Figshare DOIs
# Run locally: shiny::runApp()
# Deploy: set AZURE_CONTAINER_URL, AZURE_SAS_RO (read-only SAS)
#         and (optional) CODE_DOI_BASE, CODE_DOI_VER, DATA_DOI_BASE, DATA_DOI_VER

library(shiny)
library(pins)
library(dplyr)
library(AzureStor)   # install.packages("AzureStor")
library(htmltools)

# -------------------------------------------------------------------
# APP CONFIG
APP_TITLE    <- "fisheriesXplorer (demo: versioned data & citation)"
APP_AUTHORS  <- "ICES (International Council for the Exploration of the Sea)"
APP_URL_BASE <- "http://localhost:xxxx"     # replaced by deployed URL on shinyapps.io
PINS_PATH    <- "fisheriesxplorer/pins"     # Azure “prefix” (like a folder)
# Back-compat defaults (used if Figshare env vars are not set)
CODE_DOI     <- "10.5281/zenodo.CODE_DOI_PLACEHOLDER"
DATA_DOI     <- "10.5281/zenodo.DATA_DOI_PLACEHOLDER"
# -------------------------------------------------------------------

# ---- Figshare DOI support (env-driven; falls back to defaults) ----
# Set these in deployment if you use Figshare:
#   CODE_DOI_BASE=10.6084/m9.figshare.1234567
#   CODE_DOI_VER=7
#   DATA_DOI_BASE=10.6084/m9.figshare.7654321
#   DATA_DOI_VER=52
CODE_DOI_BASE <- Sys.getenv("CODE_DOI_BASE", unset = CODE_DOI)
CODE_DOI_VER  <- Sys.getenv("CODE_DOI_VER",  unset = "")
DATA_DOI_BASE <- Sys.getenv("DATA_DOI_BASE", unset = DATA_DOI)
DATA_DOI_VER  <- Sys.getenv("DATA_DOI_VER",  unset = "")

compose_versioned_doi <- function(base, ver) {
  if (!nzchar(base) || !nzchar(ver)) return(NA_character_)
  ver <- sub("^v", "", ver)  # accept "7" or "v7"
  paste0(base, ".v", ver)
}
link_doi <- function(doi) {
  if (!nzchar(doi)) return("")
  sprintf("<a href='https://doi.org/%s' target='_blank' rel='noopener'>%s</a>", doi, doi)
}

CODE_DOI_VERSIONED <- compose_versioned_doi(CODE_DOI_BASE, CODE_DOI_VER)
DATA_DOI_VERSIONED <- compose_versioned_doi(DATA_DOI_BASE, DATA_DOI_VER)

# Prefer versioned DOIs in citations when available
CODE_DOI_CITE <- if (!is.na(CODE_DOI_VERSIONED)) CODE_DOI_VERSIONED else CODE_DOI_BASE
DATA_DOI_CITE <- if (!is.na(DATA_DOI_VERSIONED)) DATA_DOI_VERSIONED else DATA_DOI_BASE

# ---- Dataset cadences (shown in UI & citations) ----
DATASETS <- list(
  landings     = list(label = "Landings",     cadence = "annual"),
  stock_status = list(label = "Stock status", cadence = "daily")
)

# (helpers for pretty labels in the citation)
tab_label <- function(val) switch(val,
  landings = "Landings",
  stock_status = "Stock status",
  val
)
subtab_label <- function(val) switch(val,
  landings_overview = "Overview",
  landings_details  = "Details",
  status_overview   = "Overview",
  status_details    = "Details",
  val
)

# -----------------------
# Board setup (Azure or fallback)
# -----------------------
use_azure <- nzchar(Sys.getenv("AZURE_CONTAINER_URL")) && nzchar(Sys.getenv("AZURE_SAS_RO"))

if (use_azure) {
  container <- AzureStor::storage_container(
    endpoint = Sys.getenv("AZURE_CONTAINER_URL"),  # e.g. https://acct.dfs.core.windows.net/container
    sas      = Sys.getenv("AZURE_SAS_RO")          # SAS with sp=r
  )
  board <- pins::board_azure(container = container, path = PINS_PATH, versioned = TRUE)
  SEED_DEMO <- FALSE
} else {
  # Fallback: local temp board (seed demo data so the app runs out of the box)
  board <- pins::board_temp(versioned = TRUE)
  SEED_DEMO <- TRUE
}

# Seed demo datasets (only used in fallback mode) ----
# Cadences reflected in snapshot_date values:
# - landings => annual snapshots on Jan 01
# - stock_status => daily snapshots on specific dates
seed_demo_pins <- function(board) {
  if (!("landings" %in% pins::pin_list(board))) {
    # Landings (ANNUAL)
    land_2024 <- data.frame(stock = c("COD", "HAD", "POK"),
                            landings_t  = c(1250, 820, 480))
    attr(land_2024, "snapshot_date") <- as.Date("2024-01-01")
    pins::pin_write(board, land_2024, name = "landings", type = "rds", versioned = TRUE)

    land_2025 <- data.frame(stock = c("COD", "HAD", "POK"),
                            landings_t  = c(1100, 900, 530))
    attr(land_2025, "snapshot_date") <- as.Date("2025-01-01")
    pins::pin_write(board, land_2025, name = "landings", type = "rds", versioned = TRUE)

    # Stock status (DAILY)
    ss_0914 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Healthy", "Cautious", "Healthy"))
    attr(ss_0914, "snapshot_date") <- as.Date("2025-09-14")
    pins::pin_write(board, ss_0914, name = "stock_status", type = "rds", versioned = TRUE)

    ss_0915 <- data.frame(stock = c("COD", "HAD", "POK"),
                          status = c("Cautious", "Cautious", "Healthy"))
    attr(ss_0915, "snapshot_date") <- as.Date("2025-09-15")
    pins::pin_write(board, ss_0915, name = "stock_status", type = "rds", versioned = TRUE)
  }
}
if (SEED_DEMO) seed_demo_pins(board)

# -----------------------
# Helpers: discover & resolve snapshots
# -----------------------
available_snapshot_dates <- function(board, names) {
  dates <- c()
  for (nm in names) {
    vers <- pins::pin_versions(board, nm)
    for (v in vers$version) {
      x <- pins::pin_read(board, nm, version = v)
      sd <- attr(x, "snapshot_date")
      if (!is.null(sd)) dates <- c(dates, sd)
    }
  }
  sort(unique(as.Date(dates)))
}

choose_version_by_snapshot <- function(board, name, as_of_date) {
  vers <- pins::pin_versions(board, name)
  if (!nrow(vers)) stop("No versions found for pin: ", name)
  candidates <- lapply(vers$version, function(v) {
    x <- pins::pin_read(board, name, version = v)
    sd <- attr(x, "snapshot_date")
    if (is.null(sd)) return(NULL)
    if (as.Date(sd) <= as.Date(as_of_date)) {
      list(version = v, snapshot_date = as.Date(sd))
    } else NULL
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No ", name, " snapshot available on/before ", as_of_date)
  o <- order(sapply(candidates, function(z) z$snapshot_date))
  candidates[[tail(o, 1)]]
}

read_pin_as_of <- function(board, name, as_of_date) {
  sel  <- choose_version_by_snapshot(board, name, as_of_date)
  data <- pins::pin_read(board, name, version = sel$version)
  list(data = data, version = sel$version, snapshot_date = sel$snapshot_date)
}

# -----------------------
# UI
# -----------------------
ui <- function(request) {
  shiny::navbarPage(
    title = APP_TITLE,
    id = "nav",

    tabPanel("Explore",
      fluidPage(
        fluidRow(
          column(
            width = 8,
            wellPanel(
              h4("View data as of"),
              uiOutput("as_of_ui"),
              br(),
              bookmarkButton(id = "share_btn", label = "Share this view"),
              helpText("Creates a URL that reproduces this exact view.")
            ),
            wellPanel(
              h4("Version & Citation"),
              uiOutput("provenance_ui"),
              tags$hr(),
              h5("Suggested citation"),
              verbatimTextOutput("citation_text", placeholder = TRUE),
              h5("BibTeX"),
              verbatimTextOutput("bibtex_text", placeholder = TRUE)
            )
          ),
          column(
            width = 4,
            # tabs & subtabs so we can permalink an exact view
            tabsetPanel(id = "tabs",
              tabPanel(title = "Landings", value = "landings",
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "landings_overview",
                           tableOutput("landings_tbl")),
                  tabPanel(title = "Details",  value = "landings_details",
                           tableOutput("landings_tbl_details"))
                )
              ),
              tabPanel(title = "Stock status", value = "stock_status",
                tabsetPanel(id = "subtabs",
                  tabPanel(title = "Overview", value = "status_overview",
                           tableOutput("status_overview_tbl")),
                  tabPanel(title = "Details",  value = "status_details",
                           tableOutput("status_tbl"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# -----------------------
# Server
# -----------------------
server <- function(input, output, session) {

  # Collect all snapshot dates across pins
  all_dates <- available_snapshot_dates(board, c("landings", "stock_status"))

  # As-of selector + honor ?tab= & ?subtab= on first load
  output$as_of_ui <- renderUI({
    qs <- shiny::parseQueryString(session$clientData$url_search)
    default_date <- if (!is.null(qs$as_of)) as.Date(qs$as_of) else tail(all_dates, 1)
    if (!is.null(qs$tab))    updateTabsetPanel(session, "tabs",    selected = qs$tab)
    if (!is.null(qs$subtab)) updateTabsetPanel(session, "subtabs", selected = qs$subtab)
    selectInput("as_of", "Snapshot date:", choices = all_dates, selected = default_date)
  })

  # Dataset reads (as of selected date)
  landings_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "landings", as.Date(input$as_of))
  })
  status_asof <- reactive({
    req(input$as_of)
    read_pin_as_of(board, "stock_status", as.Date(input$as_of))
  })

  # Tables
  output$landings_tbl <- renderTable({ landings_asof()$data }, striped = TRUE, bordered = TRUE, spacing = "s")
  output$landings_tbl_details <- renderTable({ landings_asof()$data }, striped = TRUE, bordered = TRUE, spacing = "s")
  output$status_tbl   <- renderTable({ status_asof()$data   }, striped = TRUE, bordered = TRUE, spacing = "s")
  output$status_overview_tbl <- renderTable({
    status_asof()$data %>% count(status, name = "n")
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # Provenance / Version & Citation panel (with Azure + Figshare DOIs + cadences)
  output$provenance_ui <- renderUI({
    req(input$as_of)
    l <- landings_asof()
    s <- status_asof()
    storage_str <- if (use_azure) {
      paste0("<b>Storage:</b> Azure container (", htmltools::htmlEscape(Sys.getenv("AZURE_CONTAINER_URL")),
             "), path <code>", PINS_PATH, "</code>")
    } else {
      "<b>Storage:</b> local temporary board (demo fallback)"
    }
    tagList(
      p(HTML(storage_str)),
      p(HTML(sprintf(
        paste0(
          "<b>Code DOI (base):</b> %s",
          if (!is.na(CODE_DOI_VERSIONED)) paste0("<br/><b>Code DOI (this version):</b> ", link_doi(CODE_DOI_VERSIONED)) else "",
          "<br/><b>Data DOI (base):</b> %s",
          if (!is.na(DATA_DOI_VERSIONED)) paste0("<br/><b>Data DOI (this version):</b> ", link_doi(DATA_DOI_VERSIONED)) else ""
        ),
        link_doi(CODE_DOI_BASE),
        link_doi(DATA_DOI_BASE)
      ))),
      p(HTML(sprintf("<b>View date:</b> %s", as.character(as.Date(input$as_of))))),
      p(HTML(sprintf("<b>View:</b> %s — %s", tab_label(input$tabs), subtab_label(input$subtabs)))),
      tags$details(
        tags$summary("Data currency by dataset"),
        tags$ul(
          tags$li(HTML(sprintf("<b>%s</b> — cadence: %s; snapshot: %s; pin version: <code>%s</code>",
                               DATASETS$landings$label, DATASETS$landings$cadence,
                               as.character(l$snapshot_date), l$version))),
          tags$li(HTML(sprintf("<b>%s</b> — cadence: %s; snapshot: %s; pin version: <code>%s</code>",
                               DATASETS$stock_status$label, DATASETS$stock_status$cadence,
                               as.character(s$snapshot_date), s$version)))
        )
      ),
      p(HTML("<i>These version IDs allow exact re-loading of the same data bytes.</i>"))
    )
  })

  # Citation & BibTeX (prefer versioned DOIs if provided via env)
  build_citation <- function(as_of, url, tabs, subtabs, land_obj, status_obj) {
    sprintf(
      paste0("%s (view as of %s). *%s* [Shiny app]. ",
             "Code DOI: %s; Data DOI: %s. View: %s — %s. ",
             "Snapshots — Landings (annual): %s [pin %s]; Stock status (daily): %s [pin %s]. %s"),
      APP_AUTHORS, as.character(as.Date(as_of)), APP_TITLE,
      CODE_DOI_CITE, DATA_DOI_CITE, tab_label(tabs), subtab_label(subtabs),
      as.character(land_obj$snapshot_date), land_obj$version,
      as.character(status_obj$snapshot_date), status_obj$version,
      url
    )
  }

  build_bibtex <- function(as_of, url, tabs, subtabs, land_obj, status_obj) {
    yy <- format(as_of, "%Y"); mm <- as.integer(format(as_of, "%m"))
    note <- paste0("View: ", tab_label(tabs), " — ", subtab_label(subtabs),
                   "; Snapshots — Landings (annual): ", as.character(land_obj$snapshot_date),
                   " [pin ", land_obj$version, "]; Stock status (daily): ",
                   as.character(status_obj$snapshot_date), " [pin ", status_obj$version, "]")
    sprintf("@software{fisheriesxplorer_%s_%02d,\n  author  = {%s},\n  title   = {%s},\n  year    = {%s},\n  month   = {%d},\n  version = {%s},\n  doi     = {%s},\n  url     = {%s},\n  note    = {%s}\n}",
            yy, mm, APP_AUTHORS, APP_TITLE, yy, mm,
            paste0(format(as_of, "%Y"), ".", format(as_of, "%m")),
            DATA_DOI_CITE, url, note)
  }

  # Bookmark URL includes ?as_of=&tab=&subtab=
  observeEvent(input$share_btn, {
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    shiny::updateQueryString(qs, mode = "replace", session = session)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    showModal(modalDialog(
      title = "Shareable link",
      easyClose = TRUE,
      footer = NULL,
      p("Anyone opening this link will see exactly this view:"),
      tags$code(url),
      tags$hr(),
      p("Copy the citation below into your manuscript or email:"),
      tags$pre(build_citation(as.Date(input$as_of), url, input$tabs, input$subtabs,
                              landings_asof(), status_asof()))
    ))
  })

  # Live citation + BibTeX (reflecting current state)
  output$citation_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_citation(as.Date(input$as_of), curr_url, input$tabs, input$subtabs,
                   landings_asof(), status_asof())
  })

  output$bibtex_text <- renderText({
    qs <- paste0(
      "?as_of=", as.character(as.Date(input$as_of)),
      "&tab=", input$tabs,
      if (!is.null(input$subtabs)) paste0("&subtab=", input$subtabs) else ""
    )
    curr_url <- paste0(ifelse(nzchar(session$clientData$url_hostname),
                              paste0(session$clientData$url_protocol, "//",
                                     session$clientData$url_hostname,
                                     ifelse(nchar(session$clientData$url_port), paste0(":", session$clientData$url_port), ""),
                                     session$clientData$url_pathname),
                              APP_URL_BASE),
                       qs)
    build_bibtex(as.Date(input$as_of), curr_url, input$tabs, input$subtabs,
                 landings_asof(), status_asof())
  })
}

# Bookmarking (URL)
enableBookmarking("url")

# Run app
shinyApp(ui, server)
