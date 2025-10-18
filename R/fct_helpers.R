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

mod_flex_header_ui <- function(ns, left_id, right_id) {  
  div(  
    style = "display: flex; justify-content: space-between; align-items: center;  
         padding: 10px; font-weight: bold; font-size: 1.2em; margin-bottom: 0px;",  
    span(textOutput(ns(left_id))),  
    span(textOutput(ns(right_id)))  
  )  
}  



make_tooltip_choice <- function(label_text, tooltip_html) {
    tags$div(
      class = "tooltip-wrapper",
      HTML(label_text),
      tags$span(class = "custom-tooltip", HTML(tooltip_html))
    )
  }


safe_min <- function(x, default = NA) {
  if (length(x) == 0) default else min(x, na.rm = TRUE)
}

safe_max <- function(x, default = NA) {
  if (length(x) == 0) default else max(x, na.rm = TRUE)
}


# --- Helper: robust downloader with curl fallback
        safe_download <- function(url, dest) {
          tryCatch(
            {
              if (requireNamespace("curl", quietly = TRUE)) {
                curl::curl_download(url, destfile = dest, quiet = TRUE)
              } else {
                utils::download.file(url, destfile = dest, quiet = TRUE, mode = "wb")
              }
              file.exists(dest) && file.info(dest)$size > 0
            },
            error = function(e) FALSE
          )
        }


safe_download <- function(url, dest, retries = 2, timeout = 30, quiet = TRUE) {
  attempt <- function() {
    if (requireNamespace("curl", quietly = TRUE)) {
      # curl path
      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = timeout, timeout = timeout)
      curl::curl_download(url, destfile = dest, quiet = quiet, handle = h)
    } else {
      # utils path
      utils::download.file(url, destfile = dest, mode = "wb", quiet = quiet, method = "auto")
    }
    file.exists(dest) && isTRUE(file.info(dest)$size > 0)
  }

  ok <- FALSE
  for (i in seq_len(retries + 1L)) {
    ok <- isTRUE(tryCatch(attempt(), error = function(e) FALSE))
    if (ok) break
  }
  ok
}


