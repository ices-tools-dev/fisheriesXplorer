#' helpers
#'
#' @description ´display_text´ subsets a list of dataframes and extracts the relevant section of text
#'
#' @param list_object a list of dataframes each containing 'ref' and 'text' columns
#' @param tab a character vector - the name of a list entry
#' @param section a character vector indicating which row to extract
#'
#' @return A character string
#'
#' @noRd

select_text <- function(list_object, tab, section){
  
  list_object[[tab]] %>% dplyr::filter(section == !!section) %>% dplyr::pull(text)
}