## Load required libraries
library(dplyr)
library(icesTAF)


#' List of ICES ecoregions
ecoregions <- c(
  "Baltic Sea", "Bay of Biscay and the Iberian Coast", "Celtic Seas",
  "Greater North Sea", "Norwegian Sea", "Icelandic Waters", "Barents Sea",
  "Greenland Sea", "Faroes", "Oceanic Northeast Atlantic", "Azores"
)

#' Get acronym for an ICES ecoregion
#'
#' Translates a full ICES ecoregion name into the corresponding
#' three-letter acronym used in the app (e.g. `"Baltic Sea"` → `"BtS"`).
#'
#' @param ecoregion A single character string giving the full
#'   ecoregion name. Must be one of:
#'   `"Baltic Sea"`, `"Bay of Biscay and the Iberian Coast"`,
#'   `"Celtic Seas"`, `"Greater North Sea"`, `"Norwegian Sea"`,
#'   `"Icelandic Waters"`, `"Barents Sea"`, `"Greenland Sea"`,
#'   `"Faroes"`, `"Oceanic Northeast Atlantic"`, or `"Azores"`.
#'
#' @return A character string with the corresponding acronym:
#'   `"BtS"`, `"BI"`, `"CS"`, `"NrS"`, `"NwS"`, `"IS"`, `"BrS"`,
#'   `"GS"`, `"FO"`, `"ONA"`, or `"AZ"`.
#'
#' @details
#' If `ecoregion` does not match any of the supported names,
#' the function raises an error via [base::stop()].
#'
#' @examples
#' get_ecoregion_acronym("Baltic Sea")
#' get_ecoregion_acronym("Greater North Sea")
#'
#' @export
get_ecoregion_acronym <- function(ecoregion) {
  switch(ecoregion,
         "Baltic Sea" = "BtS",
         "Bay of Biscay and the Iberian Coast" = "BI",
         "Celtic Seas" = "CS",
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


#' Load ASFIS species reference table
#'
#' Reads the local ASFIS ASFIS\_sp CSV file and returns a data frame with
#' English common name, scientific name, and FAO three-letter species code.
#'
#' @details
#' This function expects the file `./data-raw/ASFIS_sp_2025.csv` to be present
#' relative to the project root (or package root, if used inside a package).
#' Only the columns `English_name`, `Scientific_Name`, and `Alpha3_Code`
#' are kept.
#'
#' @return
#' A data frame (or tibble, if used with dplyr) with three character columns:
#' \describe{
#'   \item{English_name}{English common name of the species.}
#'   \item{Scientific_Name}{Scientific (Latin) name of the species.}
#'   \item{Alpha3_Code}{FAO three-letter species code.}
#' }
#'
#' @examples
#' \dontrun{
#' species <- load_asfis_species()
#' head(species)
#' }
#'
load_asfis_species <- function() {
#   url <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"
    species <- read.csv("./data-raw/ASFIS_sp_2025.csv", na.strings = "", stringsAsFactors = FALSE)
    species <- dplyr::select(species, English_name, Scientific_Name, Alpha3_Code)
    return(species)
}

#' Load ICES historical catches data
#'
#' Reads the local `ICES_historical_catches.csv` file and returns a data frame
#' with historical catches as provided by ICES.
#'
#' @details
#' This function expects the file `./data-raw/ICES_historical_catches.csv`
#' to be available relative to the project (or package) root. All columns
#' in the CSV file are returned unchanged.
#'
#' @return
#' A data frame containing the contents of `ICES_historical_catches.csv`.
#'
#' @examples
#' \dontrun{
#' hist <- load_historical_catches()
#' head(hist)
#' }
#'
#' @export
load_historical_catches<- function(){
        hist <- read.csv("./data-raw/ICES_historical_catches.csv", header = TRUE)#, na.strings = "", stringsAsFactors = FALSE)
}


#' Load ICES official catches data
#'
#' Reads the local `ICESCatchDataset2006-2023_noConf.csv` file and returns
#' a data frame with official ICES catch statistics.
#'
#' @details
#' This function expects the file
#' `./data-raw/ICESCatchDataset2006-2023_noConf.csv` to be available
#' relative to the project (or package) root. All columns in the CSV file
#' are returned unchanged.
#'
#' @return
#' A data frame containing the contents of
#' `ICESCatchDataset2006-2023_noConf.csv`.
#'
#' @examples
#' \dontrun{
#' official <- load_official_catches()
#' head(official)
#' }
#'
#' @export
load_official_catches<- function(){
        official <- read.csv("./data-raw/ICESCatchDataset2006-2023_noConf.csv", header = TRUE)#, na.strings = "", stringsAsFactors = FALSE)
}


#' Format historical, official and preliminary catches for an ecoregion
#'
#' Combines ICES historical catches, official catches and (optionally)
#' preliminary catch data into a single tidy data set for the selected
#' ecoregion(s). The function harmonises country names and codes,
#' assigns ICES ecoregions, and attaches species and fisheries guild
#' information.
#'
#' @param year A numeric vector of years. Currently not used inside the
#'   function, but kept for interface compatibility and potential
#'   future filtering.
#' @param ecoregion Character vector of ICES ecoregion names to keep
#'   (e.g. `"Baltic Sea"`, `"Greater North Sea"`, `"Celtic Seas"`,
#'   `"Norwegian Sea"`, `"Barents Sea"`, `"Faroes"`, `"Azores"`,
#'   `"Oceanic Northeast Atlantic"`, `"Greenland Sea"`,
#'   `"Icelandic Waters"`). Used both in some area–ecoregion mappings
#'   and in the final filter (`ECOREGION %in% ecoregion`).
#' @param historical Data frame of ICES historical catches, with at
#'   least the columns `Country`, `Species`, `Division` and yearly
#'   catch columns (e.g. `X1950`, `X1951`, ...).
#' @param official Data frame of ICES official catches, with at least
#'   the columns `Country` (ISO2 code), `Species`, `Area`, `Units`
#'   and yearly catch columns.
#' @param preliminary Optional data frame of preliminary catches
#'   (default `NULL`). If supplied, it is merged on top of historical
#'   and official data. Expected to contain at least `Year`,
#'   `Country` (ISO2 code), `Area`, a catch column, and a species
#'   column (Latin name).
#' @param species_list Data frame with ASFIS species information,
#'   typically from [load_asfis_species()], containing columns
#'   `English_name`, `Scientific_Name` and `Alpha3_Code`.
#' @param sid Data frame with stock information (e.g. ICES SID),
#'   used to derive fisheries guilds. Must contain at least
#'   `StockKeyLabel` (from which `Alpha3_Code` is derived) and
#'   `FisheriesGuild`.
#'
#' @return
#' A data frame with one row per year–country–guild–ecoregion–species
#' combination, containing:
#' \describe{
#'   \item{YEAR}{Year (numeric).}
#'   \item{COUNTRY}{Country name (string).}
#'   \item{ISO3}{ISO3 country code.}
#'   \item{GUILD}{Fisheries guild (e.g. `"Pelagic"`, `"Demersal"`,
#'     `"undefined"`).}
#'   \item{ECOREGION}{ICES ecoregion name.}
#'   \item{SPECIES_NAME}{Scientific (Latin) species name.}
#'   \item{SPECIES_CODE}{FAO three-letter species code.}
#'   \item{COMMON_NAME}{English common name of the species.}
#'   \item{VALUE}{Catch value (typically in tonnes).}
#' }
#'
#' @details
#' Historical catches are reshaped from wide (year columns) to long
#' format, area divisions are mapped to ICES ecoregions, country names
#' are normalised and converted to ISO3 codes, and species are linked
#' to ASFIS codes and fisheries guilds via `species_list` and `sid`.
#'
#' Official and, if supplied, preliminary catches are treated similarly,
#' and then all sources are combined. The final data set is filtered to
#' the requested `ecoregion` values.
#'
#' Values reported as `"<0.5"` in the historical data are converted to
#' zero. Missing guilds are set to `"undefined"`. Some special cases
#' (e.g. Faroe Islands historical records and sandeel (`Ammodytes`)
#' records) are handled explicitly in the function body.
#'
#' @examples
#' \dontrun{
#' species <- load_asfis_species()
#' hist    <- load_historical_catches()
#' off     <- load_official_catches()
#'
#' df <- format_catches(
#'   year       = 2025,
#'   ecoregion  = "Greater North Sea",
#'   historical = hist,
#'   official   = off,
#'   preliminary = NULL,
#'   species_list = species,
#'   sid = sid_table
#' )
#' head(df)
#' }
#'
#' @export
format_catches <- function(year, ecoregion, historical, official, preliminary = NULL, species_list, sid) {
        
        fish_category <- dplyr::mutate(sid, Alpha3_Code = substr(sid$StockKeyLabel, start = 1, stop = 3))
        fish_category <- dplyr::select(fish_category, Alpha3_Code, FisheriesGuild)
        fish_category$Alpha3_Code <- toupper(fish_category$Alpha3_Code)
        fish_category <- unique(fish_category)
        fish_category$FisheriesGuild[which(fish_category$Alpha3_Code == "POK")] <- "Demersal"
        
        # fish_category<- fish_category[complete.cases(fish_category),]
        
        historic_bs <- c("III (not specified)", "III b  Baltic 23",
                         "III b+c (not specified)", "III b-d (not specified)",
                         "III c  Baltic 22", "III d  (not specified)",
                         "III d  Baltic 24", "III d  Baltic 25",
                         "III d  Baltic 26", "III d  Baltic 27",
                         "III d  Baltic 28 (not specified)", "III d  Baltic 28-1",
                         "III d  Baltic 28-2", "III d  Baltic 29",
                         "III d  Baltic 30", "III d  Baltic 31",
                         "III d  Baltic 32")
        
        historic_ns <- c("III a", "IIIa  and  IV  (not specified)",
                         "IIIa  and  IVa+b  (not specified)", "IV (not specified)",
                         "IV a", "IV a+b (not specified)",
                         "IV b", "IV b+c (not specified)",
                         "IV c", "VII d")
        
        historic_uk <- paste0(c("^UK", "^Channel", "^Isle of Man"),
                              collapse = "|")
        #these historical catches definition need decision on conflicts
        historic_bob <- c("VIII a", "VIII b", "VIII c", "VIII d2", "VIII e2",
                          "IX a", "IX b2", "VIII d (not specified)", "VIII (not specified)","VIII e (not specified)", "IX (not specified)",
                          "IX b (not specified)")
        
        historic_cs <- c("VI a", "VI b2", "VII a", "VII b", "VII c2", "VII f", "VII g", "VII h",
                         "VII j2", "VII k2", "VII (not specified)", "VII b+c (not specified)",
                         "VII c (not specified)", "VII d-k (not specified)", "VII f-k (not specified)",
                         "VII g-k (not specified)", "VII j (not specified)")
        
        historic_is <- c("V a (North-East)", "V a (South-West)", "V a1", "V a (not specified)", "V a2")
        historic_az <- c("X (not specified)", "X a (not specified)")
        historic_gs <- c("XII a3", "XIV (not specified)", 
                         "XIV a", "XIV b (not specified)", "XIV b2" )
        
        
        if(ecoregion == "Norwegian Sea"){
                historic_nw <- c( "II a1", "II b1", "I  and  IIa (not specified)","II a (not specified)",
                                  "II (not specified)", "II a2", "II b (not specified)",
                                  "II b2", "XIV", "XIVa" )
        }
        if(ecoregion == "Barents Sea"){
                historic_br <- c( "I (not specified)", "I a","I b",  "I  and  IIa (not specified)","II a (not specified)",
                                  "II (not specified)", "II a2", "II b (not specified)",
                                  "II b2" )
        }
        if(ecoregion == "Faroes"){
                historic_fo <- c( "V b2","V b (not specified)", "V b1 (not specified)", "V b1B")
                
        }
        if(ecoregion == "Oceanic Northeast Atlantic"){
                historic_nea <- c( "V b1A", "VI b1", "VII c1", "VII j1", "VII k1", "VIII d1", "VIII e1",
                                   "IX b1", "X b", "XII a1", "XII b", "XIV b1","X (not specified)", "X a (not specified)", 
                                   "XII (not specified)")
        }
        fo_2020 <- historical %>% dplyr::filter(Division == "ICES Area (not specified)")
        fo_2020 <-fo_2020%>% dplyr::filter(Country == "Faeroe Islands")
        
        
        
        fo_2020$Division <- "V b1 (not specified)"
        fo_2020$Division[which(fo_2020$Species == "Atlantic mackerel")] <- "ICES Area (not specified)"
        fo_2020$Division[which(fo_2020$Species == "Atlantic horse mackerel")] <- "ICES Area (not specified)"
        fo_2020$Division[which(fo_2020$Species == "Atlantic herring")] <- "ICES Area (not specified)"
        historical <- full_join(historical, fo_2020)
        
        
        historical[is.na(historical)] <- 0
        
        catch_dat_1950 <- tidyr::gather(historical, YEAR, VALUE, -Country, -Species, -Division) 
        catch_dat_1950 <- 
                dplyr::mutate(catch_dat_1950, YEAR = as.numeric(gsub("X", "", YEAR)),
                              VALUE = ifelse(VALUE == "<0.5",
                                             as.numeric(0),
                                             VALUE),
                              VALUE = ifelse(!is.na(VALUE),
                                             as.numeric(VALUE),
                                             NA),
                              Country = dplyr::case_when(
                                      grepl(historic_uk, Country) ~ "United Kingdom",
                                      grepl("^Germany", Country) ~ "Germany",
                                      Country %in% c("Un. Sov. Soc. Rep.", "Russian Federation") ~ "Russia",
                                      grepl("Faeroe Islands", Country) ~ "Faroe Islands",
                                      grepl("Other nei", Country) ~ "OTHER",
                                      TRUE ~ Country
                              ),
                              ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
                              ECOREGION = dplyr::case_when(
                                      Division %in% historic_bs ~ "Baltic Sea",
                                      Division %in% historic_ns ~ "Greater North Sea",
                                      Division %in% historic_bob ~ "Bay of Biscay and the Iberian Coast",
                                      Division %in% historic_cs ~ "Celtic Seas",
                                      Division %in% historic_is ~ "Icelandic Waters",
                                      Division %in% historic_az ~ "Azores",
                                      Division %in% historic_gs ~ "Greenland Sea",
                                      if(ecoregion == "Oceanic Northeast Atlantic"){
                                              Division %in% historic_nea ~ "Oceanic Northeast Atlantic"
                                              
                                      }, 
                                      if(ecoregion == "Faroes"){
                                              Division %in% historic_fo ~ "Faroes"
                                              
                                      },
                                      if(ecoregion == "Norwegian Sea"){
                                              Division %in% historic_nw ~ "Norwegian Sea"
                                      },
                                      if(ecoregion == "Barents Sea"){
                                              Division %in% historic_br ~ "Barents Sea"
                                      },
                                      TRUE ~ "OTHER")) 
        
        catch_dat_1950 <- dplyr::filter(catch_dat_1950, YEAR <= 2005)
        catch_dat_1950 <- dplyr::left_join(catch_dat_1950, species_list, c("Species" = "English_name"))# Merge to add FAO species information
        catch_dat_1950 <- dplyr::left_join(catch_dat_1950, species_list,c("Species" = "Scientific_Name", # Merge to add FAO species information
                                                                              "Alpha3_Code"))
        catch_dat_1950 <- dplyr::left_join(catch_dat_1950, fish_category, by = "Alpha3_Code")
        catch_dat_1950 <- dplyr::select(catch_dat_1950,YEAR,
                                        COUNTRY = Country,
                                        ISO3,
                                        GUILD = FisheriesGuild,
                                        ECOREGION,
                                        SPECIES_NAME = Scientific_Name,
                                        SPECIES_CODE = Alpha3_Code,
                                        COMMON_NAME = Species,
                                        VALUE)
        
        # add in official
        catch_dat_2010 <- tidyr::gather(official, YEAR, VALUE, -Country, -Species, -Area, -Units)
        catch_dat_2010$VALUE <- as.numeric(catch_dat_2010$VALUE)
        catch_dat_2010$VALUE[is.na(catch_dat_2010$VALUE)] <- 0
        catch_dat_2010 <- dplyr::filter(catch_dat_2010, Country != "")
        catch_dat_2010 <- dplyr::mutate(catch_dat_2010,YEAR = as.numeric(gsub("X", "", YEAR)),
                                        VALUE = as.numeric(VALUE),
                                        Country = countrycode::countrycode(Country,"iso2c", "country.name"),
                                        Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                                                         "United Kingdom",
                                                         Country),
                                        ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
                                        Country = gsub("(United Kingdom) .*", "\\1", Country),
                                        Area = tolower(Area))
        catch_dat_2010 <- dplyr::mutate(catch_dat_2010,
                                        ECOREGION = dplyr::case_when(
                                                Area %in% c("27.3.bc", "27.3.d", "27.3_nk") ~ "Baltic Sea",
                                                Area %in% c("27.3.a", "27.4", "27.7.d") ~ "Greater North Sea",
                                                Area %in% c("27.8.a", "27.8.b","27.8.c",
                                                            "27.8.d.2", "27.8.e.2", "27.9.a",
                                                            "27.9.b.2") ~ "Bay of Biscay and the Iberian Coast",
                                                Area %in% c("27.6.a", "27.6.b.2","27.7.a", "27.7.b", "27.7.c.2",
                                                            "27.7.f", "27.7.g", "27.7.h","27.7.j.2", "27.7.k.2") ~ "Celtic Seas",
                                                Area %in% c("27.5.a.1", "27.5.a.2","27.5.a_NK","27.5.a_nk","27.12.a.4") ~ "Icelandic Waters",
                                                if(ecoregion == "Norwegian Sea"){
                                                        Area %in% c("27.2.a.1", "27.2.a.2","27.2.a_NK","27.2.a_nk", "27.2.b.1", "27.2.b.2", "27.2.b_NK","27.2.b_nk","27.14.a", "27.14_NK", "27.14_nk") ~ "Norwegian Sea"
                                                },
                                                if(ecoregion == "Azores"){
                                                        Area %in% c("27.10.a.2", "27.10.a_NK", "27.10.a_nk", "27.10_NK", "27.10_nk") ~ "Azores"
                                                },
                                                if(ecoregion == "Greenland Sea"){
                                                        Area %in% c("27.12.a.3", "27.14.a", "27.14.b.2", "27.14.b_NK", "27.14.b_nk", "27.14_NK", "27.14_nk") ~ "Greenland Sea"
                                                },
                                                if(ecoregion == "Faroes"){
                                                        Area %in% c("27.5.b.2","27.5.b.1.a","27.5.b.1.b", "27.5.b.1_NK", "27.5.b_NK", "27.5.b.1_nk",
                                                                    "27.5.b_nk") ~ "Faroes"
                                                },
                                                if(ecoregion =="Barents Sea"){
                                                        Area %in% c("27.1.a", "27.1.b","27.2.a.2","27.2.a_NK","27.2.a_nk", "27.2.b.2","27.2.b_NK","27.2.b_nk", "27.1_NK", "27.1_nk") ~ "Barents Sea"
                                                },
                                                if(ecoregion == "Oceanic Northeast Atlantic"){
                                                        Area %in% c("27.5.b.1.a", "27.6.b.1","27.7.c.1", "27.7.j.1",
                                                                    "27.7.k.1", "27.8.d.1", "27.8.e.1", "27.9.b.1", 
                                                                    "27.10.a.1", "27.10.b", "27.12_nk","27.12_NK", "27.12.a.1", "27.12.b", 
                                                                    "27.12.c", "27.14.b.1") ~ "Oceanic Northeast Atlantic" 
                                                },
                                                TRUE ~ "OTHER"))
        catch_dat_2010 <- dplyr::left_join(catch_dat_2010,species_list, c("Species" = "Alpha3_Code"))
        catch_dat_2010 <- dplyr::left_join(catch_dat_2010,fish_category, by = c("Species" = "Alpha3_Code")) 
        catch_dat_2010<- catch_dat_2010[!is.na(catch_dat_2010$ECOREGION),]
        catch_dat_2010 <- dplyr::select(catch_dat_2010,YEAR,
                                        COUNTRY = Country,
                                        ISO3,
                                        GUILD = FisheriesGuild,
                                        ECOREGION,
                                        SPECIES_NAME = Scientific_Name,
                                        SPECIES_CODE = Species,
                                        COMMON_NAME = English_name,
                                        VALUE)
        catch_dat_2010 <- dplyr::group_by(catch_dat_2010,YEAR, COUNTRY, GUILD, ECOREGION,COMMON_NAME)
        catch_dat_2010 <- dplyr::summarise(catch_dat_2010, VALUE = sum(VALUE))
        
        # now do preliminary catches
        
        if (is.null(preliminary)) {
                df <- dplyr::bind_rows(catch_dat_2010,catch_dat_1950)
        } else {
                catch_dat_prelim <- dplyr::filter(preliminary, Country != "")
                catch_dat_prelim$VALUE <- catch_dat_prelim[,7]
                catch_dat_prelim <- catch_dat_prelim[, -grep("AMS", colnames(catch_dat_prelim))]
                catch_dat_prelim <- catch_dat_prelim[, -grep("BMS", colnames(catch_dat_prelim))]
                catch_dat_prelim$Species.Latin.Name <- catch_dat_prelim[,3]
                # catch_dat_prelim <- catch_dat_prelim[, -grep("Species.Latin.Name", colnames(catch_dat_prelim))]
                # tidyr::gather(ï..Year, -Country, -AphiaID, -Area, -Catch) %>%
                catch_dat_prelim <- dplyr::mutate(catch_dat_prelim, YEAR = Year,
                                                  Country = countrycode::countrycode(Country,"iso2c", "country.name"),
                                                  Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country),
                                                                   "United Kingdom",
                                                                   Country),
                                                  ISO3 = countrycode::countrycode(Country, "country.name", "iso3c", warn = FALSE),
                                                  Country = gsub("(United Kingdom) .*", "\\1", Country),
                                                  Area = tolower(Area))
                
                
                #check why areas names are different!!
                catch_dat_prelim <- dplyr::mutate(catch_dat_prelim, ECOREGION = dplyr::case_when(
                        Area %in% c("27_3_bc", "27_3_c_22","27_3_d","27_3_d_24","27_3_d_25","27_3_d_26","27_3_d_30",
                                    "27_3_d_27","27_3_d_31","27_3_nk", "27_3_b_23", "27_3_d_28_2","27_3_d_32","27_3_d_29") ~ "Baltic Sea",
                        Area %in% c("27_3_a", "27_4_a","27_4_b", "27_4_c", "27_7_d") ~ "Greater North Sea",
                        
                        Area %in% c("27_8_a", "27_8_b","27_8_c",
                                    "27_8_d_2", "27_8_e_2", "27_9_a",
                                    "27_9_b_2")~ "Bay of Biscay and the Iberian Coast",
                        Area %in% c("27_6_a", "27_6_b_2","27_7_a", "27_7_b", "27_7_c_2","27_7.e",
                                    "27_7_f", "27_7_g", "27_7_h","27_7_j_2", "27_7_k_2")~"Celtic Seas",
                        
                        Area %in% c("5_a_1", "5_a_2","12_a_4")~"Icelandic Waters",
                        Area %in% c("27_10_a_2", "27_10_A_2")~"Azores",
                        Area %in% c("27_1_a", "27_1_b", "27_2_b_2")~ "Barents Sea",
                        Area %in% c("27_2_a_1", "27_2_a_2", "27_2_b_1", "27_2_b_2", "27_14_a", "27_2_a", "27_2_b")~"Norwegian Sea",
                        Area %in% c("27_5_b_1_A", "27_6_b_1","27_7_c_1", "27_7_j_1","27_7_k_1",
                                    "27_8_d_1", "27_8_e_1", "27_9_b_1", "27_10_a_1", "27_10_b", 
                                    "27_12_a_1", "27_12_b", "27_12_c", "27_14_b_1")~"Oceanic Northeast Atlantic",
                        Area %in% c("27_14_B", "27_14", "27_14_B_2", "27_14_A", "27_14_NK")~"Greenland Sea",
                        Area %in% c(" 27_5_b", "27_5_b_1", "27_5_b_2", "27_5_b_1_b")~"Faroes",
                        TRUE ~ "OTHER"))
                
                catch_dat_prelim <- dplyr::filter(catch_dat_prelim,ECOREGION != "OTHER")
                catch_dat_prelim <- dplyr::left_join(catch_dat_prelim, species_list, c("Species.Latin.Name" = "Scientific_Name"))
                
                catch_dat_prelim <- dplyr::left_join(catch_dat_prelim, fish_category, by = "Alpha3_Code")
                catch_dat_prelim <- dplyr::select(catch_dat_prelim,YEAR,
                                                  COUNTRY = Country,
                                                  ISO3 = Alpha3_Code,
                                                  GUILD = FisheriesGuild,
                                                  ECOREGION,
                                                  SPECIES_NAME = "Species.Latin.Name",
                                                  SPECIES_CODE = Alpha3_Code,
                                                  COMMON_NAME = English_name,
                                                  VALUE)
                catch_dat_prelim$COMMON_NAME[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "Sandeels(=Sandlances) nei"
                catch_dat_prelim$SPECIES_CODE[which(catch_dat_prelim$SPECIES_NAME == "Ammodytes")] <- "SAN"
                catch_dat_prelim$VALUE <- as.numeric(catch_dat_prelim$VALUE)
                df <- dplyr::bind_rows(catch_dat_2010,catch_dat_1950, catch_dat_prelim)
        }
        
        
        df <- dplyr::ungroup(df)
        df <- dplyr::mutate(df, GUILD = ifelse(is.na(GUILD),
                                               "undefined",
                                               GUILD))
        
        df$COUNTRY<-gsub("Russian Federation", "\\Russia\\",df$COUNTRY)
        df$COUNTRY<-gsub("Russia", "Russian Federation", df$COUNTRY)
        df <- dplyr::select(df,YEAR,
                            COUNTRY,
                            ISO3,
                            GUILD ,
                            ECOREGION,
                            SPECIES_NAME,
                            SPECIES_CODE,
                            COMMON_NAME,
                            VALUE)
        
        # df$GUILD[which(ices_catch_dat$SPECIES_CODE == "WHB")] <- "pelagic"
        df <- dplyr::filter(df, ECOREGION %in% ecoregion)
        
        return(df)
}




################### Getting data from ICES ###################
sid <- icesSD::getSD(NULL, 2025)

fish_category <- dplyr::mutate(sid, X3A_CODE = substr(sid$StockKeyLabel, start = 1, stop = 3))
fish_category <- dplyr::select(fish_category, X3A_CODE, FisheriesGuild)
fish_category$X3A_CODE <- toupper(fish_category$X3A_CODE)
fish_category <- unique(fish_category)
#CAA, SEH, SEZ  have no guild
#REB is both pelagic and demersal
sid$FisheriesGuild[which(sid$StockKeyLabel == "caa.27.5a")] <- "Demersal"
#Should we include seals? maybe not
sid <- sid %>% dplyr::filter(SpeciesScientificName != "Pagophilus groenlandicus")
sid <- sid %>% dplyr::filter(SpeciesScientificName != "Cystophora cristata")


species_list <- load_asfis_species()

hist <- load_historical_catches()
hist$Country[which(hist$Country == "Germany, New L\xe4nder")]<- "Germany"

official <- load_official_catches()

## Ceate folders using the acronyms of the ecoregions
for (ecoregion in ecoregions) {
        acronym <- get_ecoregion_acronym(ecoregion)
        mkdir(paste0("./data-raw/", acronym))

        catch_dat <- format_catches(2025, ecoregion, hist, official, NULL, species_list, sid)
    

        catch_dat$COUNTRY[which(catch_dat$COUNTRY == "Russian Federation")] <- "Russia"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Atlantic mackerel")] <- "mackerel"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Atlantic horse mackerel")] <- "horse mackerel"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Atlantic cod")] <- "cod"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Atlantic herring")] <- "herring"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "cod")] <- "Demersal"
        catch_dat$GUILD[which(catch_dat$SPECIES_CODE == "POK")] <- "Demersal"
        catch_dat$GUILD[which(catch_dat$SPECIES_CODE == "REB")] <- "Demersal"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "European pilchard(=Sardine)")] <- "Sardine"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Scomber mackerels nei")] <- "Mackerels"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Mackerels nei")] <- "Mackerels"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Atlantic chub mackerel")] <- "Chub mackerel"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Mackerels")] <- "pelagic"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Chub mackerel")] <- "pelagic"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Jack and horse mackerels nei")] <- "Jack and horse mackerels"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Atlantic horse mackerel")] <- "Jack and horse mackerels"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "horse mackerel")] <- "Jack and horse mackerels"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Atlantic mackerel")] <- "mackerel"
        #adg suggestions 2025
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Angler(=Monk)")] <- "anglerfish"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Anglerfishes NEI")] <- "anglerfish"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Blackbellied angler")] <- "anglerfish"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Monkfishes NEI")] <- "anglerfish"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Monkfishes nei")] <- "anglerfish"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Megrims nei")] <- "megrim"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Megrims NEI")] <- "megrim"

        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Jack and horse mackerels")] <- "pelagic"
        # catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Monkfishes nei")] <- "Anglerfishes nei"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Anglerfishes nei")] <- "benthic"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Pelagic fishes nei")] <- "pelagic"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Raja rays nei")] <- "elasmobranch"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Bathyraja rays nei")] <- "elasmobranch"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Albacore")] <- "pelagic"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Pouting(=Bib)")] <- "demersal"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Gadiformes nei")] <- "demersal"
        catch_dat$COMMON_NAME[which(catch_dat$COMMON_NAME == "Octopuses, etc. nei")] <- "Octopuses"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Blue mussel")] <- "crustacean"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Sea mussels nei")] <- "crustacean"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Cockles nei")] <- "crustacean"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Common edible cockle")] <- "crustacean"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Tuberculate cockle")] <- "crustacean"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Pouting(=Bib)")] <- "demersal"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Gadiformes nei")] <- "demersal"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Cupped oysters nei")] <- "crustacean"
        catch_dat$GUILD[which(catch_dat$COMMON_NAME == "Pacific cupped oyster")] <- "crustacean"

        catch_dat$GUILD <- tolower(catch_dat$GUILD)
        catch_dat$GUILD[catch_dat$GUILD == "crustacean"] <- "shellfish"

        catch_dat <- unique(catch_dat)

        write.taf(catch_dat, file = paste0("catch_dat_", acronym, ".csv"), dir = paste0("./data-raw/", acronym, "/"), quote = TRUE)
}


for (ecoregion in ecoregions) {
  acronym <- get_ecoregion_acronym(ecoregion)
  
  file_path <- paste0("./data-raw/", acronym, "/catch_dat_", acronym, ".csv")
  
  if (file.exists(file_path)) {
    # Read the CSV file
    catch_dat <- read.csv(file_path)
    
    # Assign it to a variable named after the acronym
    assign(acronym, catch_dat)
    
    # Save the data in an .rda file using a structured name
    save(list = acronym, file = paste0("./data/", acronym, ".rda"))
    
    message("Saved: ", acronym)
  } else {
    warning("File not found: ", file_path)
  }
}


