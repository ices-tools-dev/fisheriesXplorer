#' helpers
#'
#' @description ´display_text´ subsets a list of dataframes and extracts the relevant section of text
#'
#' @param list_object a list of dataframes each containing 'ref' and 'text' columns
#' @param tab a character vector - the name of a list entry
#' @param section a character vector indicating which row to extract
#' @returns A character string
#'
#' @noRd

select_text <- function(list_object, tab, section){
  
  list_object[[tab]] %>% dplyr::filter(section == !!section) %>% dplyr::pull(.data$text)
}



icon_mapping <- function(value) {
  
  if (value[1] == "GREEN") {
    '<i class="fas fa-check-circle" style="color:green; font-size:38px;"></i>'
  } else if (value[1] == "RED") {
    '<i class="fas fa-times-circle" style="color:red; font-size:38px;"></i>'
  } else if (value[1] == "ORANGE") {
    '<i class="fas fa-exclamation-circle" style="color:orange; font-size:38px;"></i>'
  } else if (value[1] == "GREY") {
    '<i class="fas fa-question-circle" style="color:grey; font-size:38px;"></i>'
  } else {
    value  # If no match, return the original value
  }
}


merge_cells <- function(values) {
  values <- as.character(values)
  unique_values <- unique(values)
  spans <- cumsum(rle(values)$lengths)
  mapply(function(start, end, value) {
    if (start == end) {
      tags$td(value)
    } else {
      tags$td(rowspan = end - start + 1, value)
    }
  }, c(1, spans[-length(spans)] + 1), spans, unique_values)
}