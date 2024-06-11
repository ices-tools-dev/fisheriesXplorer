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
