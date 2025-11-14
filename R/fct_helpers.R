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

# select_text <- function(list_object, tab, section){
  
#   list_object[[tab]] %>% dplyr::filter(section == !!section) %>% dplyr::pull(.data$text)
# }
select_text <- function(list_object, tab, section) {
  # 1) Existence checks
  if (is.null(list_object[[tab]])) {
    stop("Table '", tab, "' not found in the provided list_object.")
  }
  df <- list_object[[tab]]

  # 2) If there's no 'section' column, return the whole table (e.g., glossary)
  if (!"section" %in% names(df)) {
    return(df)
  }

  # 3) There is a 'section' column: filter by the requested section
  df_sub <- dplyr::filter(df, .data$section == !!section)

  # 4) If there's a 'text' column (your normal case), return it as a character vector
  if ("text" %in% names(df_sub)) {
    out <- dplyr::pull(df_sub, .data$text)
    if (length(out) == 0) return("")   # safe empty for renderUI(HTML())
    # Coerce just in case
    return(as.character(out))
  }

  # 5) Fallback: return the filtered data.frame (no 'text' column present)
  df_sub
}

get_ecoregion_acronym <- function(ecoregion) {
  switch(ecoregion,
         "Baltic Sea" = "BtS",
         "Bay of Biscay and the Iberian Coast" = "BI",
         "Bay of Biscay" = "BoB",
         "Iberian Waters" = "IW",
         "Celtic Seas" = "CS",
         "Celtic Sea" = "CSx",
         "Irish Sea" = "IrS",
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

get_active_region_acronym <- function(subregion, ecoregion) {
  region <- if (!is.null(subregion)) subregion else ecoregion
  get_ecoregion_acronym(region)
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

# mod_flex_header_ui <- function(ns, left_id, right_id) {  
#   div(  
#     style = "display: flex; justify-content: space-between; align-items: center;  
#          padding: 10px; font-weight: bold; font-size: 1.2em; margin-bottom: 0px;",  
#     span(textOutput(ns(left_id))),  
#     span(textOutput(ns(right_id)))  
#   )  
# }  
mod_flex_header_ui <- function(ns, left_id, right_id) {
  div(
    style = "display:flex;justify-content:space-between;align-items:center;
             padding:10px;font-weight:bold;font-size:1.2em;margin-bottom:0;",
    uiOutput(ns(left_id)),
    uiOutput(ns(right_id))
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


# # --- Helper: robust downloader with curl fallback
#         safe_download <- function(url, dest) {
#           tryCatch(
#             {
#               if (requireNamespace("curl", quietly = TRUE)) {
#                 curl::curl_download(url, destfile = dest, quiet = TRUE)
#               } else {
#                 utils::download.file(url, destfile = dest, quiet = TRUE, mode = "wb")
#               }
#               file.exists(dest) && file.info(dest)$size > 0
#             },
#             error = function(e) FALSE
#           )
#         }


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

# Takes ns (the namespace function), not id
mod_flex_header_ui <- function(ns, left_id, right_id) {
  tags$div(
    class = "fx-header-center",
    uiOutput(ns(left_id),  inline = TRUE),
    tags$span(class = "fx-sep", "\u00B7"),
    uiOutput(ns(right_id), inline = TRUE)
  )
}


# ------------------------
  # bookmarking Helpers
  # ------------------------
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a else b

  .base_url <- function(session) {
    proto <- session$clientData$url_protocol %||% "https:"
    host  <- session$clientData$url_hostname %||% ""
    port  <- session$clientData$url_port
    path  <- session$clientData$url_pathname %||% "/"
    port_part <- if (!is.null(port) && nzchar(port) &&
                     !(proto == "https:" && port == "443") &&
                     !(proto == "http:"  && port == "80")) paste0(":", port) else ""
    paste0(proto, "//", host, port_part, path)
  }

  parse_nav <- function(hash, search) {
    if (nzchar(hash)) shiny::parseQueryString(sub("^#", "", hash))
    else if (nzchar(search)) shiny::parseQueryString(search)
    else list()
  }

  # Map subtab input ids (read) & selectors (write)
  SUBTAB_INPUTS <- list(
    overview     = "overview_1-tabs_overview",
    landings     = "landings_1-main_tabset",
    stock_status = "stock_status_1-main_tabset",
    resources    = "resources_1-resources_nav"
  )

  get_current_subtab <- function(tab, input) {
    id <- SUBTAB_INPUTS[[tab]]
    if (is.null(id)) "" else as.character(input[[id]] %||% "")
  }

  select_subtab <- function(tab, value, session) {
    if (!nzchar(value)) return(invisible(NULL))
    if (tab == "stock_status" && requireNamespace("bslib", quietly = TRUE) &&
        utils::packageVersion("bslib") >= "0.5.0") {
      bslib::nav_select(id = SUBTAB_INPUTS[[tab]], selected = value, session = session)
    } else {
      updateTabsetPanel(session, SUBTAB_INPUTS[[tab]], selected = value)
    }
  }

  write_hash <- function(eco, tab, sub) {
    paste0(
      "#eco=", utils::URLencode(eco %||% "", reserved = TRUE),
      "&tab=", utils::URLencode(tab %||% "", reserved = TRUE),
      if (nzchar(sub %||% "")) paste0("&subtab=", utils::URLencode(sub, reserved = TRUE)) else ""
    )
  }
