## code to prepare `catchScenarioStock` dataset goes here
library(dplyr)
library(icesTAF)

# catchScenarioStk <- read.table("./data-raw/GNS/catchScenStk.csv")

# usethis::use_data(catchScenarioStk, overwrite = TRUE)


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
    df <- read.csv(download_url, sep = c(",", ";"), header = TRUE)
    
    return(df)
}




# Define the regions to download data for
regions <- c("NrS","CS","IrS","IW", "BoB")


#### catchScenarioStk
## download data from github for each region
for (region in regions) {
  # Construct the file path for the region
  file_path <- paste0("shiny/Figure1_HeadlinePlot_data.csv")
  
  # Download the data from GitHub
  assign(paste0("catchScenarioStk_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
}

# Combine the data into a single data frame
catchScenarioStk <- do.call(rbind, lapply(regions, function(region) {
  df <- get(paste0("catchScenarioStk_", region))
  df <- df %>% select(stock, scenario, catch)
  # if region is CS, rename df$ecoregion to CSx, else df$ecoregion = region
  if (region == "CS") {
    df$ecoregion <- paste0(region, "x")
  } else {
    df$ecoregion <- region
  }
  # df$ecoregion <- region  # Add the ecoregion column

  return(df)
}))


# Save the combined data frame as a rda file
save(catchScenarioStk, file = "data/catchScenarioStk.rda")



#### catchRange
for (region in regions) {
  # Construct the file path for the region
  file_path <- paste0("shiny/Figure1_HeadlinePlot_advice.csv")
  
  # Download the data from GitHub
  assign(paste0("catchRange_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
  
}


# Combine the data into a single data frame
catchRange <- do.call(rbind, lapply(regions, function(region) {
  df <- get(paste0("catchRange_", region))
  df <- df %>% select(stock, advice, lower,  upper)
  if (region == "CS") {
    df$ecoregion <- paste0(region, "x")
  } else {
    df$ecoregion <- region
  }
  return(df)
}))

save(catchRange, file = "data/catchRange.rda")





#### reTable
for (region in regions) {
  # Construct the file path for the region
  file_path <- paste0("shiny/refTable.csv")
  
  # Download the data from GitHub
  assign(paste0("refTable_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
  }


# # Combine the data into a single data frame
refTable <- bind_rows(lapply(regions, function(region) {
  df <- get(paste0("refTable_", region))
  
  # Coerce known columns to consistent types
  if ("ref" %in% colnames(df)) df$ref <- as.character(df$ref)
  
  # Add ecoregion
  df$ecoregion <- ifelse(region == "CS", paste0(region, "x"), region)  
  
  return(df)
}))
save(refTable, file = "data/refTable.rda")
# # Download the data from GitHub
# NrS_catchScenarioStk <- download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")
# download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")


# flref