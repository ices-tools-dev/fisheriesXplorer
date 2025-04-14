## code to prepare `texts` dataset goes here
library(readxl)

path <- "data-raw/texts.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path)
names(texts) <- sheets

usethis::use_data(texts, overwrite = TRUE)

