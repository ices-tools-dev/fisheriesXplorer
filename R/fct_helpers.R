#' Select text (or rows) from a list of tables
#'
#' Helper to retrieve text snippets or rows from a list of data frames,
#' typically used to store tab- and section-specific content for a Shiny app.
#'
#' @param list_object A named list of data frames, where each element
#'   corresponds to a tab or content table (e.g. glossary, help text).
#' @param tab Character scalar giving the name of the element in
#'   \code{list_object} to use (e.g. `"about"`, `"methods"`, `"glossary"`).
#' @param section Character scalar specifying the value to match in the
#'   \code{section} column of the selected data frame.
#'
#' @return
#' If the selected table does not contain a \code{section} column, the
#' entire data frame is returned (useful for tables such as glossaries).
#'
#' If a \code{section} column is present:
#' \itemize{
#'   \item If a \code{text} column is present, a character vector of the
#'     matching \code{text} values is returned. If no rows match, an empty
#'     string (\code{""}) is returned (which is safe for \code{renderUI(HTML())}).
#'   \item Otherwise, the filtered data frame (rows where
#'     \code{section == section}) is returned.
#' }
#'
#' @details
#' The function first checks that \code{tab} exists in \code{list_object};
#' if not, it stops with an informative error. It then optionally filters
#' by \code{section} and, when possible, pulls the \code{text} column as a
#' character vector.
#'
#' @examples
#' \dontrun{
#' # Suppose `content_list` is a named list of data frames
#' # with columns: section, text
#' select_text(content_list, tab = "about", section = "intro")
#'
#' # Glossary table without a `section` column:
#' glossary_df <- select_text(content_list, tab = "glossary", section = "ignored")
#' }
#'
#' @export
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

#' Map status values to ICES status icons
#'
#' Converts status codes (e.g. `"GREEN"`, `"RED"`, `"ORANGE"`, `"GREY"`)
#' into HTML strings containing ICES status icons, suitable for use in
#' Shiny UI outputs.
#'
#' @param value A character vector of status codes. Only the first element
#'   (\code{value[1]}) is inspected. Expected values are
#'   \code{"GREEN"}, \code{"RED"}, \code{"ORANGE"}, or \code{"GREY"}.
#'
#' @return
#' A single character string containing an HTML \code{<i>} tag with the
#' appropriate ICES status icon and colour if \code{value[1]} matches
#' one of the known codes. Otherwise, the original \code{value} is
#' returned unchanged.
#'
#' @details
#' The function returns raw HTML strings such as
#' \code{'<i class="fas fa-check-circle" ...></i>'}. In Shiny, these
#' should typically be rendered with \code{htmltools::HTML()} or inside
#' functions that recognise HTML (e.g. \code{renderUI()}, \code{DT} with
#' \code{escape = FALSE}, etc.). The app must load Font Awesome for the
#' icons to display correctly.
#'
#' @examples
#' icon_mapping("GREEN")
#' icon_mapping("RED")
#'
#' @export
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

#' Create merged table cells with rowspans
#'
#' Helper to generate HTML `<td>` elements with appropriate `rowspan`
#' attributes for sequences of identical values in a column. This is
#' useful when you want to visually merge repeated values in a table
#' (e.g. in a Shiny app or DT table).
#'
#' @param values A vector of values (typically one column of a data
#'   frame) in display order. Identical values that are adjacent will
#'   be merged into a single cell via the `rowspan` attribute.
#'
#' @return
#' A list of \code{shiny.tag} objects (HTML `<td>` elements). For runs of
#' length 1, a normal `<td>` without `rowspan` is returned; for longer
#' runs, a single `<td>` with `rowspan = run_length` is returned.
#'
#' @details
#' The function assumes that all values that should be merged are
#' already contiguous in \code{values}; it does not reorder the input.
#' The first element of each run of identical values becomes a `<td>`
#' with a suitable `rowspan`, and subsequent elements in that run are
#' omitted (their rows will visually share the same cell).
#'
#' In a Shiny/DT context, these `<td>` elements are generally inserted
#' into a custom table-rendering routine, and HTML escaping must be
#' disabled where appropriate.
#'
#' @examples
#' \dontrun{
#' vals <- c("A", "A", "B", "C", "C")
#' cells <- merge_cells(vals)
#' # `cells` is a list of <td> tags, where "A" and "C" each span 2 rows.
#' }
#'
#' @export
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


#' Create a label with a custom tooltip
#'
#' Builds a small UI fragment consisting of a label and a tooltip element,
#' wrapped in a `<div>` with class `tooltip-wrapper`. The tooltip content
#' is shown/hidden via CSS/JavaScript defined elsewhere in the app.
#'
#' @param label_text Character string containing the main label text.
#'   This can include HTML markup if needed.
#' @param tooltip_html Character string containing the tooltip body.
#'   This is treated as HTML (e.g. can include paragraphs, lists, etc.).
#'
#' @return
#' A \code{shiny.tag} representing a `<div>` with:
#' \itemize{
#'   \item the label (rendered via \code{HTML(label_text)}), and
#'   \item a `<span>` with class `custom-tooltip` containing
#'     \code{tooltip_html}.
#' }
#'
#' @details
#' The function only generates the HTML structure; styling and behaviour
#' (e.g. show on hover/click) must be implemented via CSS/JavaScript for
#' the classes `tooltip-wrapper` and `custom-tooltip`. Both arguments
#' are passed through \code{htmltools::HTML()}, so they should be
#' trusted strings, not user-supplied input.
#'
#' @examples
#' \dontrun{
#' make_tooltip_choice(
#'   label_text   = "Fishing pressure",
#'   tooltip_html = "<strong>Definition:</strong> ICES fishing pressure category."
#' )
#' }
#'
#' @export
make_tooltip_choice <- function(label_text, tooltip_html) {
    tags$div(
      class = "tooltip-wrapper",
      HTML(label_text),
      tags$span(class = "custom-tooltip", HTML(tooltip_html))
    )
  }

#' Safe minimum with a default for empty vectors
#'
#' Computes the minimum of a vector, but returns a default value if the
#' vector is empty.
#'
#' @param x A vector (typically numeric, Date, or POSIXt) for which the
#'   minimum should be computed.
#' @param default Value to return when \code{x} has length zero.
#'   Defaults to \code{NA}.
#'
#' @return
#' A single value: \code{min(x, na.rm = TRUE)} if \code{x} is non-empty,
#' otherwise \code{default}.
#'
#' @details
#' Missing values are always removed via \code{na.rm = TRUE}. Note that
#' if \code{x} is non-empty but all elements are \code{NA}, the result
#' will be \code{Inf} (as per base \code{min()} behaviour) rather than
#' \code{default}; the \code{default} is only used when
#' \code{length(x) == 0}.
#'
#' @examples
#' safe_min(c(3, 1, 5))
#' safe_min(numeric(0), default = 0)
#' safe_min(c(NA, 2, NA))
#'
#' @export
safe_min <- function(x, default = NA) {
  if (length(x) == 0) default else min(x, na.rm = TRUE)
}

#' Safe maximum with a default for empty vectors
#'
#' Computes the maximum of a vector, but returns a default value if the
#' vector is empty.
#'
#' @param x A vector (typically numeric, Date, or POSIXt) for which the
#'   maximum should be computed.
#' @param default Value to return when \code{x} has length zero.
#'   Defaults to \code{NA}.
#'
#' @return
#' A single value: \code{max(x, na.rm = TRUE)} if \code{x} is non-empty,
#' otherwise \code{default}.
#'
#' @details
#' Missing values are always removed via \code{na.rm = TRUE}. Note that
#' if \code{x} is non-empty but all elements are \code{NA}, the result
#' will be \code{-Inf} (as per base \code{max()} behaviour) rather than
#' \code{default}; the \code{default} is only used when
#' \code{length(x) == 0}.
#'
#' @examples
#' safe_max(c(3, 1, 5))
#' safe_max(numeric(0), default = 0)
#' safe_max(c(NA, 2, NA))
#'
#' @export
safe_max <- function(x, default = NA) {
  if (length(x) == 0) default else max(x, na.rm = TRUE)
}



#' Safely download a file with retries and timeouts
#'
#' Attempts to download a file from a URL to a local destination, using
#' the \pkg{curl} package when available and falling back to
#' \code{utils::download.file()} otherwise. The function can retry the
#' download several times and checks that a non-empty file was created.
#'
#' @param url Character string; the URL of the file to download.
#' @param dest Character string; path to the destination file on disk.
#' @param retries Integer; number of retry attempts after the initial
#'   attempt. Default is \code{2}, meaning up to three total attempts.
#' @param timeout Numeric; timeout in seconds for the connection and
#'   overall transfer when using \pkg{curl}. Ignored by the
#'   \code{utils::download.file()} fallback. Default is \code{30}.
#' @param quiet Logical; passed to the underlying download functions to
#'   suppress progress output. Default is \code{TRUE}.
#'
#' @return
#' Logical scalar: \code{TRUE} if a non-empty file exists at \code{dest}
#' after one of the attempts, otherwise \code{FALSE}.
#'
#' @details
#' When \pkg{curl} is available, \code{curl::curl_download()} is used
#' with a handle configured for the given \code{timeout}. If \pkg{curl}
#' is not installed, the function falls back to
#' \code{utils::download.file()} with \code{mode = "wb"}.
#'
#' After each attempt, the function checks that \code{dest} exists and
#' has a size greater than zero. Any errors during download are caught
#' and treated as failures. The function is side-effecting (creates or
#' overwrites \code{dest}) and is primarily intended for robust,
#' non-interactive downloads in data preparation scripts.
#'
#' @examples
#' \dontrun{
#' ok <- safe_download(
#'   url  = "https://example.com/data.csv",
#'   dest = "data-raw/data.csv"
#' )
#' if (!ok) warning("Download failed")
#' }
#'
#' @export
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

#' Centered header UI with inline outputs and separator
#'
#' Creates a header-style UI fragment with two inline \code{uiOutput()}
#' elements, separated by a middle dot. IDs are namespaced using the
#' provided \code{ns} function, making this suitable for use inside
#' Shiny modules.
#'
#' @param ns A namespacing function, typically created by
#'   [shiny::NS()] inside a module server/UI pair.
#' @param left_id Character scalar giving the (unnamespaced) output ID
#'   for the left-hand side \code{uiOutput()}.
#' @param right_id Character scalar giving the (unnamespaced) output ID
#'   for the right-hand side \code{uiOutput()}.
#'
#' @return
#' A \code{shiny.tag} representing a \code{<div>} with class
#' \code{"fx-header-center"}, containing:
#' \itemize{
#'   \item a left \code{uiOutput()},
#'   \item a \code{<span>} with class \code{"fx-sep"} displaying
#'     a middle dot, and
#'   \item a right \code{uiOutput()}.
#' }
#' Intended for inclusion in Shiny UI definitions.
#'
#' @details
#' The visual styling (e.g. centering, spacing, typography) is controlled
#' by CSS rules associated with the classes \code{"fx-header-center"}
#' and \code{"fx-sep"}, which should be defined elsewhere in the app.
#'
#' @examples
#' \dontrun{
#' mod_flex_header_ui(ns, left_id = "title", right_id = "subtitle")
#' }
#'
#' @export
mod_flex_header_ui <- function(ns, left_id, right_id) {
  tags$div(
    class = "fx-header-center",
    uiOutput(ns(left_id),  inline = TRUE),
    tags$span(class = "fx-sep", "\u00B7"),
    uiOutput(ns(right_id), inline = TRUE)
  )
}


############################ bookmarking Helpers ##########################

#' Null / empty-coalescing infix operator
#'
#' Returns the first argument \code{a} if it is considered "meaningful",
#' otherwise returns \code{b}. A value is treated as meaningful if it is
#' not \code{NULL}, has length > 0, its first element is not \code{NA},
#' and the first element is a non-empty string when coerced to character.
#'
#' @param a First candidate value. Typically a vector (often length 1)
#'   that may be \code{NULL}, empty, \code{NA}, or a blank string.
#' @param b Fallback value to use when \code{a} is not meaningful.
#'
#' @return
#' Either \code{a} (if it passes the checks) or \code{b} (otherwise).
#'
#' @details
#' This operator is a convenience for expressions such as:
#' \code{if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a else b}.
#' It is particularly useful in Shiny bookmarking and URL/query parsing
#' logic, where missing or empty parameters should fall back to default
#' values.
#'
#' @examples
#' "foo" %||% "bar"           # "foo"
#' NULL  %||% "default"       # "default"
#' ""    %||% "fallback"      # "fallback"
#' NA    %||% 10              # 10
#'
#' @export
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a else b


#' Construct base URL from a Shiny session
#'
#' Internal helper that reconstructs the base URL of the current Shiny
#' app request from \code{session$clientData}. The result includes the
#' protocol, host, (optional) port, and path.
#'
#' @param session A Shiny session object, typically the \code{session}
#'   argument passed into a Shiny server function or module server.
#'
#' @return
#' A single character string of the form
#' \code{"<protocol>//<host>[:port]<path>"}, for example
#' \code{"https://example.org/app/"}.
#'
#' @details
#' The function uses:
#' \itemize{
#'   \item \code{session$clientData$url_protocol}, defaulting to
#'     \code{"https:"} if missing.
#'   \item \code{session$clientData$url_hostname}, defaulting to
#'     the empty string.
#'   \item \code{session$clientData$url_port}, which is omitted when it
#'     matches the default port for the protocol (80 for HTTP, 443 for
#'     HTTPS) or is empty.
#'   \item \code{session$clientData$url_pathname}, defaulting to
#'     \code{"/"}.
#' }
#'
#' It is mainly used for constructing absolute URLs for bookmarking,
#' share links, or redirects inside the app. This function relies on
#' the \code{\%||\%} operator to provide sensible defaults.
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'   base <- .base_url(session)
#'   # e.g., paste0(base, "?tab=overview")
#' })
#' }
#'
#' @keywords internal
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

#' Parse navigation state from URL hash or search
#'
#' Helper to extract query parameters from either the URL hash
#' (e.g. \code{"#tab=overview&eco=CS"}) or the search/query string
#' (e.g. \code{"?tab=overview&eco=CS"}), returning them as a named list.
#'
#' @param hash Character string, typically \code{session$clientData$url_hash}
#'   (including the leading \code{"#"} if present).
#' @param search Character string, typically
#'   \code{session$clientData$url_search} (including the leading
#'   \code{"?"} if present).
#'
#' @return
#' A named list of query parameters as returned by
#' \code{shiny::parseQueryString()}. If \code{hash} is non-empty, it is
#' parsed (after stripping the leading \code{"#"}). Otherwise, if
#' \code{search} is non-empty, it is parsed. If both are empty, an empty
#' list is returned.
#'
#' @details
#' This function is designed for use in bookmarking and share-link logic
#' where navigation state may be encoded in either the hash fragment or
#' the query string. It gives precedence to \code{hash} over
#' \code{search} when both are available.
#'
#' @examples
#' \dontrun{
#' # Using clientData from a Shiny session
#' nav <- parse_nav(
#'   hash   = session$clientData$url_hash,
#'   search = session$clientData$url_search
#' )
#' nav$tab
#' }
#'
#' @export
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

#' Get current sub-tab for a main tab
#'
#' Looks up the input ID associated with a given main tab in
#' \code{SUBTAB_INPUTS} and retrieves the corresponding value from
#' \code{input}, returning it as a character string. If no sub-tab
#' input is defined or the value is missing/empty, an empty string is
#' returned.
#'
#' @param tab Character scalar giving the name/key of the main tab
#'   (used to index \code{SUBTAB_INPUTS}).
#' @param input The Shiny \code{input} object (or a list-like object
#'   providing subscript access to input values).
#'
#' @return
#' A character scalar with the current sub-tab value for the given
#' \code{tab}, or \code{""} if no sub-tab is defined or selected.
#'
#' @details
#' \code{SUBTAB_INPUTS} is expected to be a named list mapping main tab
#' names to the corresponding input IDs of their sub-tab controls
#' (e.g. \code{radioButtons}, \code{tabsetPanel}, etc.). The function
#' uses the \code{\%||\%} operator to default to an empty string when
#' the input value is \code{NULL}, empty, or otherwise not meaningful.
#'
#' @examples
#' \dontrun{
#' # Suppose SUBTAB_INPUTS <- list(main = "main_subtab")
#' current <- get_current_subtab("main", input)
#' }
#'
#' @export
get_current_subtab <- function(tab, input) {
  id <- SUBTAB_INPUTS[[tab]]
  if (is.null(id)) "" else as.character(input[[id]] %||% "")
}

#' Programmatically select a sub-tab for a main tab
#'
#' Selects a sub-tab within a main tab panel, using either
#' \code{bslib::nav_select()} (for Bootstrap 5 / bslib >= 0.5.0) or
#' \code{updateTabsetPanel()} as a fallback. The input ID for the
#' sub-tab control is taken from \code{SUBTAB_INPUTS}.
#'
#' @param tab Character scalar giving the name/key of the main tab
#'   (used to index \code{SUBTAB_INPUTS}).
#' @param value Character scalar giving the sub-tab value to select.
#'   If \code{value} is empty (\code{!nzchar(value)}), the function
#'   returns immediately and does nothing.
#' @param session A Shiny session object, typically the \code{session}
#'   argument passed to a server function or module server.
#'
#' @return
#' Invisibly returns \code{NULL}. Called for its side effects on the
#' Shiny session (changing the selected sub-tab).
#'
#' @details
#' The function expects a global (or package-level) object
#' \code{SUBTAB_INPUTS}, which is a named list mapping main tab names
#' to the corresponding input IDs of their sub-tab controls.
#'
#' For the \code{"stock_status"} tab, if \pkg{bslib} is installed and
#' has version \code{>= 0.5.0}, \code{bslib::nav_select()} is used,
#' which is appropriate for Bootstrap 5 navigation components. For all
#' other cases, \code{updateTabsetPanel()} is used as a generic
#' fallback.
#'
#' @examples
#' \dontrun{
#' # Select the "overview" sub-tab under main tab "stock_status"
#' select_subtab("stock_status", "overview", session)
#' }
#'
#' @export
select_subtab <- function(tab, value, session) {
  if (!nzchar(value)) return(invisible(NULL))
  if (tab == "stock_status" && requireNamespace("bslib", quietly = TRUE) &&
      utils::packageVersion("bslib") >= "0.5.0") {
    bslib::nav_select(id = SUBTAB_INPUTS[[tab]], selected = value, session = session)
  } else {
    updateTabsetPanel(session, SUBTAB_INPUTS[[tab]], selected = value)
  }
}

#' Write URL hash for bookmarking / share links
#'
#' Constructs a URL hash fragment encoding the selected ecoregion, main
#' tab, and (optionally) sub-tab. The values are URL-encoded and joined
#' in the form \code{"#eco=<...>&tab=<...>&subtab=<...>"}.
#'
#' @param eco Character scalar; ecoregion identifier to include in the
#'   hash (e.g. an acronym or full name). If \code{NULL} or empty,
#'   an empty string is used.
#' @param tab Character scalar; main tab identifier. If \code{NULL} or
#'   empty, an empty string is used.
#' @param sub Character scalar; sub-tab identifier. If \code{NULL} or
#'   empty, the \code{subtab} parameter is omitted from the hash.
#'
#' @return
#' A single character string representing the URL hash fragment, starting
#' with \code{"#"} (e.g. \code{"#eco=CS&tab=stock_status&subtab=overview"}).
#'
#' @details
#' Each component is passed through \code{utils::URLencode()} with
#' \code{reserved = TRUE} to ensure a safe encoding for use in URLs.
#' The helper operator \code{\%||\%} is used to fall back to \code{""}
#' when inputs are \code{NULL} or otherwise not meaningful.
#'
#' This function is typically used together with \code{parse_nav()} and
#' \code{.base_url()} in bookmarking and share-link logic for Shiny
#' applications.
#'
#' @examples
#' write_hash("CS", "stock_status", "overview")
#' write_hash("BtS", "overview", NULL)  # no subtab parameter
#'
#' @export
write_hash <- function(eco, tab, sub) {
  paste0(
    "#eco=", utils::URLencode(eco %||% "", reserved = TRUE),
    "&tab=", utils::URLencode(tab %||% "", reserved = TRUE),
    if (nzchar(sub %||% "")) paste0("&subtab=", utils::URLencode(sub, reserved = TRUE)) else ""
  )
}

####################################### End bookmarking Helpers #######################################


#' Add rows by replicating a stock template with new keys
#'
#' Helper to append additional rows to a data frame by copying the first
#' row matching a given stock label and replacing a key column with a
#' vector of new keys.
#'
#' @param df A data frame containing at least the columns
#'   \code{StockKeyLabel} and \code{key_col}.
#' @param stock_label Character scalar used to filter \code{df} by
#'   \code{StockKeyLabel}. The first matching row is used as the template.
#' @param keys Vector of new key values to insert into \code{key_col}
#'   for the replicated rows.
#' @param key_col Character scalar giving the name of the key column to
#'   modify. Defaults to \code{"AssessmentKey"}.
#'
#' @return
#' A data frame consisting of the original \code{df} plus one additional
#' row for each element of \code{keys}, where all other columns are
#' copied from the template row.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters \code{df} to \code{StockKeyLabel == stock_label} and
#'     takes the first row as a template.
#' \item Replicates this template once per element of \code{keys}.
#'   \item Replaces \code{key_col} in these replicated rows with
#'     \code{keys}.
#'   \item Appends the new rows to \code{df} via \code{dplyr::bind_rows()}.
#' }
#'
#' If no rows match \code{stock_label}, the behaviour will depend on
#' the result of the initial filter (typically an error or empty
#' additions), so it is advisable to ensure that at least one template
#' row exists.
#'
#' @examples
#' \dontrun{
#' df2 <- add_keys(
#'   df          = stock_list_long,
#'   stock_label = "cod.27.46a7d20",
#'   keys        = c(12345, 67890)
#' )
#' }
#'
#' @export
add_keys <- function(df, stock_label, keys, key_col = "AssessmentKey") {
          template <- df %>%
            dplyr::filter(StockKeyLabel == stock_label) %>%
            dplyr::slice(1)
          additions <- template[rep(1, length(keys)), ]
          additions[[key_col]] <- keys
          dplyr::bind_rows(df, additions)
}

#' Prepare the name of a file for download.
#' 
#' Generates consistently named download files from the \code{content_type} and \code{selected_ecoregion},
#' converting selected ecoregion to its acronym and adding a date tag
#'
#' @param content_typw
#' @param selected_ecoregion 
#' @param file_type
#'
#' @details The function results in a filename with the following format:
#' [content_type]_data_bundle_[ecoregion acronym]_[date_tag].zip
#'
#' @examples
#' \dontrun{
#' zip_filename <- prep_bundle_filename(
#' content_type="vms_effort",
#' selected_ecoregion="Greater North Sea",
#' file_type = "zip")
#' }
prep_bundle_filename <- function(content_type, selected_ecoregion, output_type) {
  function() {
    ecoregion <- selected_ecoregion()
    acronym  <- get_ecoregion_acronym(ecoregion)
    date_tag <- format(Sys.Date(), "%d-%b-%y")
    paste0(content_type,"_", acronym, "_", date_tag, ".", output_type)
  }
}