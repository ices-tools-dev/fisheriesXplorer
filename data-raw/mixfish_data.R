## code to prepare `catchScenarioStock` dataset goes here

catchScenarioStk <- read.table("./data-raw/GNS/catchScenStk.csv")

usethis::use_data(catchScenarioStk, overwrite = TRUE)
