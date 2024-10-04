# Load required libraries
library(tidyverse)
library(here)
library(yaml)

# Define constants
RAW_DATA_DIR <- here("data-raw")
OUTPUT_DIR <- here("data")

config_path <- file.path(RAW_DATA_DIR, "config.yml")
all_config <- yaml.load_file(config_path)

# Function to process data for a single ecoregion
process_ecoregion_data <- function(ecoregion, config) {
  # Read raw data files for the ecoregion

  config <- all_config[[ecoregion]]
  
  if (is.null(config)) {
    stop(paste("Configuration for ecoregion", ecoregion, "not found"))
  }

 # Read raw data files for the ecoregion based on config
  data_list <- lapply(names(config$files), function(file_key) {
    file_path <- file.path(RAW_DATA_DIR, ecoregion, config$files[[file_key]])
    if (!file.exists(file_path)) {
      warning(paste("File not found:", file_path))
      return(NULL)
    }
    read.csv(file_path)
  })
  names(data_list) <- names(config$files)

# Remove any NULL entries (files that weren't found)
  data_list <- data_list[!sapply(data_list, is.null)]

  # Process the data
  
  # Return a list of topic data frames
  return(data_list)
}

# Get list of ecoregions (assuming each subdirectory in RAW_DATA_DIR is an ecoregion)
ecoregions <- list.dirs(RAW_DATA_DIR, full.names = FALSE, recursive = FALSE)

# Process data for each ecoregion
all_data <- map(ecoregions, process_ecoregion_data, all_config)
names(all_data) <- ecoregions

# Save processed data
usethis::use_data(all_data, overwrite = TRUE)
