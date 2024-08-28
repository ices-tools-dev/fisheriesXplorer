#catches
library(dplyr)

current_catches <- read.csv(file = "data-raw/GNS/catch_current.csv")

# current_catches <- current_catches %>% group_by(StockKeyLabel) %>% mutate(total = ifelse(all(is.na(Catches) & is.na(Landings)), yes = NA, no = max(Catches, Landings, na.rm = TRUE))) %>% 
#                     ungroup() %>% 
#                     mutate(StockKeyLabel = forcats::fct_reorder(StockKeyLabel,total))

usethis::use_data(current_catches, overwrite = TRUE)


trends <- read.csv(file = "data-raw/GNS/trends.csv")

usethis::use_data(trends, overwrite = TRUE)


clean_status <- read.csv(file = "data-raw/GNS/clean_status.csv")

usethis::use_data(clean_status, overwrite = TRUE)


catch_trends <- read.csv(file = "data-raw/GNS/catch_trends.csv")

usethis::use_data(catch_trends, overwrite = TRUE)

formatted_catch_data <- read.csv(file = "data-raw/GNS/catch_dat.csv")

usethis::use_data(formatted_catch_data, overwrite = TRUE)



annex_data <- read.csv(file = "data-raw/GNS/annex_table.csv")

usethis::use_data(annex_data, overwrite = TRUE)
