#' Landings and discards UI module
#'
#' This module UI creates the main \emph{Landings} section of the app,
#' with two tabs: aggregated landings by different categories and
#' discards trends/coverage. It uses a side panel for explanatory text
#' and cards for plots and controls.
#'
#' @param id A character string used as the module namespace. Passed to
#'   \code{shiny::NS()} and used to namespace all UI elements within the
#'   module.
#'
#' @details
#' The UI contains:
#' \itemize{
#'   \item A header created by \code{mod_flex_header_ui()} showing the
#'     current ecoregion label and date.
#'   \item A \code{tabsetPanel} with two tabs:
#'     \itemize{
#'       \item \code{"Landings"}: a sidebar with dynamic explanatory text
#'         (\code{landings_text}) and a main card containing:
#'         \itemize{
#'           \item A radio-button selector for the landings layer
#'             (main landed species, fisheries guild, country).
#'           \item A CSV download link for landings data
#'             (\code{download_landings_data}).
#'           \item A dynamic output for the selected layer
#'             (\code{landings_layer}), typically a map or plot, wrapped
#'             in \code{shinycssloaders::withSpinner()}.
#'         }
#'       \item \code{"Discards"}: a sidebar with dynamic text
#'         (\code{discards_text}) and cards containing:
#'         \itemize{
#'           \item A \code{plotly} discard trends plot
#'             (\code{discard_trends}) with a CSV download link
#'             (\code{download_discard_data}).
#'           \item Two additional \code{plotly} outputs for recorded and
#'             all discards (\code{recorded_discards}, \code{all_discards}),
#'             each wrapped in \code{withSpinner()}.
#'         }
#'     }
#' }
#'
#' The corresponding server logic is implemented in
#' \code{mod_landings_server()}.
#'
#' @return A \code{shiny.tag.list} representing the landings and discards
#'   UI, suitable for inclusion in a Shiny app.
#'
#' @importFrom shiny NS tagList tabsetPanel tabPanel sidebarLayout uiOutput radioButtons downloadLink
#' @importFrom bslib layout_sidebar sidebar card card_header card_body
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'   ui <- fluidPage(mod_landings_ui("landings"))
#'   server <- function(input, output, session) {
#'     mod_landings_server("landings")
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @export
mod_landings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    tabsetPanel(
      id = ns("main_tabset"),
      tabPanel(
        title = "Landings", value = "landings", # <-- add value
        layout_sidebar(
          bg = "white", fg = "black",
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("landings_text"))
          ),
          card(
            height = "85vh",
            card_header(
              radioButtons(ns("landings_layer_selector"), "Select grouping:",
                inline = TRUE,
                choices = c(
                  "Main landed species" = "Common name",
                  "Fisheries Guild" = "Fisheries guild",
                  "Country" = "Country"
                )
              ),
              downloadLink(
                ns("download_landings_data"),
                HTML(paste0("<span class='hovertext' data-hover='Landings csv files (takes a few seconds)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
              )
            ),
            card_body(
              withSpinner(
                uiOutput(ns("landings_layer"), height = "65vh")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Discards", value = "discards", # <-- add value
        layout_sidebar(
          bg = "white", fg = "black",
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("discards_text"))
          ),
          card(
            card_header(
              "Discard trends",
              downloadLink(ns("download_discard_data"),
                HTML(paste0("<span class='hovertext' data-hover='Discards csv file'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
              )
            ),
            card_body(
              style = "overflow-y: hidden;",
              withSpinner(plotlyOutput(ns("discard_trends")))
            )
          ),
          card(
            card_body(
              style = "overflow-y: hidden;",
              layout_column_wrap(
                width = 1 / 2,
                withSpinner(plotlyOutput(ns("recorded_discards"))),
                withSpinner(plotlyOutput(ns("all_discards")))
              )
            )
          )
        )
      )
    )
  )
}

#' Server logic for landings and discards module
#'
#' This module server powers the \emph{Landings} section of the app. It
#' manages bookmarking of the main tabset, renders header labels and a
#' floating glossary, generates landings and discards plots, and provides
#' downloadable data bundles for both landings and discards.
#'
#' @param id Module id, matching the id used in \code{mod_landings_ui()}.
#' @param cap_year Optional numeric year indicating the data capture or
#'   reference year for landings/discards (reserved for use in labels).
#' @param cap_month Optional numeric month indicating the data capture or
#'   reference month (reserved for use in labels).
#' @param selected_ecoregion A reactive expression returning the currently
#'   selected ecoregion name (character). Used for labelling and to select
#'   the appropriate cached data file.
#' @param shared A list or environment containing shared data objects,
#'   typically including \code{SAG} and \code{SID} tables used by
#'   \code{format_sag()} and \code{CLD_trends()} for discard calculations.
#' @param bookmark_qs A reactive returning a list of bookmark query-string
#'   parameters (including \code{subtab}) or \code{NULL}. Used to restore
#'   the selected main tab (\code{"landings"} or \code{"discards"}) on load.
#' @param set_subtab A callback function taking a single character argument
#'   (the current subtab id). Used to inform the parent app of tab changes
#'   for bookmarking or other side effects.
#'
#' @details
#' The module:
#' \itemize{
#'   \item Restores the active tab (\code{main_tabset}) from
#'     \code{bookmark_qs()} on first load and reports it via
#'     \code{set_subtab()}.
#'   \item Renders a header showing the current ecoregion and a "Last data
#'     update" label that depends on the active tab.
#'   \item Attaches a floating glossary via
#'     \code{mod_glossary_float_server()}, using entries from the
#'     \code{texts} table (type \code{"glossary"}).
#'   \item Renders explanatory text blocks for landings and discards from
#'     \code{texts} (type \code{"landings_discards"}).
#'   \item For landings:
#'     \itemize{
#'       \item Loads an ecoregion-specific \code{.rda} file (by acronym).
#'       \item Calls \code{plot_catch_trends_plotly()} with parameters
#'         derived from the radio-button selection and cleans series names
#'         for display.
#'       \item Provides a ZIP bundle download (\code{download_landings_data})
#'         containing a CSV export, a disclaimer, and external ICES ZIP
#'         files, using \code{safe_download()} where needed.
#'     }
#'   \item For discards:
#'     \itemize{
#'       \item Uses \code{format_sag()} and \code{CLD_trends()} based on
#'         \code{shared$SAG} and \code{shared$SID}.
#'       \item Produces a \code{plotly} time-series trends plot and two
#'         current-year summary plots (stocks with recorded discards and all
#'         stocks).
#'       \item Provides a ZIP bundle download
#'         (\code{download_discard_data}) containing a CSV export and a
#'         disclaimer.
#'     }
#' }
#'
#' @return No direct return value; the function is called for the side
#'   effects of registering observers, outputs, and download handlers in a
#'   Shiny server context.
#'
#' @importFrom shiny moduleServer observeEvent updateTabsetPanel renderUI
#'   downloadHandler reactive req tags tagList
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom utils write.csv
#'
#' @export
mod_landings_server <- function(
    id, cap_year, cap_month, selected_ecoregion, shared,
    bookmark_qs = reactive(NULL),
    set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ################################## bookmarking #########################################
    # This module participates in the global bookmarking via two hooks:
    # - `bookmark_qs`: a reactive list provided by the main server with the
    #   parsed query-string (including $subtab).
    # - `set_subtab()`: a callback into the main server to report *user-driven*
    #   changes of the internal tab state.
    #
    # Restore path:
    # - On first non-null bookmark_qs(), we read the desired subtab.
    # - If it is valid for this module, we wait for the UI to flush, then
    #   select the corresponding tabsetPanel value.
    # - We also call set_subtab() once so the main server can see that the
    #   module has accepted the requested subtab.
    #
    # Report path:
    # - Any later changes to input$tabs_overview (ignoring the initial) are
    #   forwarded upstream via set_subtab(), so the main server can update
    #   the URL hash / desired() state.
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("landings", "discards")
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "main_tabset", selected = wanted)
          isolate(set_subtab(wanted))
        }, once = TRUE)
      }
    })

    # REPORT on user changes, skip initial default
    observeEvent(input$main_tabset,
      {
        set_subtab(input$main_tabset)
      },
      ignoreInit = TRUE
    )


    output$ecoregion_label <- renderUI({
      req(selected_ecoregion())
      tags$span(tags$b("ICES ecoregion:"), " ", paste0(selected_ecoregion(), " (", get_ecoregion_acronym(selected_ecoregion()), ")"))
    })

    ################################## header + glossary #########################################
    output$current_date <- renderUI({
      tab <- input$main_tabset
      if (is.null(tab)) tab <- "landings"

      date_text <- switch(tab,
        "landings" = "October, 2025",
        "discards" = format(Sys.Date(), "%B %d, %Y"),
        ""
      )

      tagList(
        tags$span(tags$b("Last data update:"), " ", date_text),
        tags$span(" \u00B7 "),
        mod_glossary_float_ui(ns("app_glossary"), link_text = "Glossary", panel_title = "Glossary")
      )
    })

    mod_glossary_float_server(
     "app_glossary",
     terms = reactive({
       df <- select_text(texts, "glossary", NULL) # your texts.rda table
       df[, intersect(names(df), c("term", "definition", "source")), drop = FALSE]
     })
   )
    
    ################################## Landings plots #########################################

    output$landings_layer <- renderUI({
      req(!is.null(input$landings_layer_selector))

      plotting_params <- list()
      plotting_params$landings <- list(
        "Common name" = list("n" = 10, type = "line"),
        "Fisheries guild" = list("n" = 6, type = "line"),
        "Country" = list("n" = 8, type = "line")
      )
      params <- plotting_params$landings[[input$landings_layer_selector]]
      ecoregion <- selected_ecoregion()
      acronym <- get_ecoregion_acronym(ecoregion)
      rda_path <- paste0("./data/", acronym, ".rda")
      load(rda_path)
      fig <- plot_catch_trends_plotly(get(get_ecoregion_acronym(ecoregion)), type = input$landings_layer_selector, line_count = params$n, dataUpdated = "October, 2025", session = session, ecoregion = acronym) # %>%
      

      for (i in 1:length(fig$x$data)) {
        if (!is.null(fig$x$data[[i]]$name)) {
          fig$x$data[[i]]$name <- gsub("\\(", "", strsplit(fig$x$data[[i]]$name, ",")[[1]][1])
        }
      }
      fig
    })

    ############################### Download landings data bundle ###############################
    output$download_landings_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("landings_trends_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # Temp workspace
        td <- tempfile("landings_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)


        # Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # ---- Build CSV from cached RDA (include acronym + date) ----
        rda_path <- file.path("data", paste0(acronym, ".rda"))
        e <- new.env(parent = emptyenv())
        load(rda_path, envir = e)
        dat <- get(acronym, envir = e)

        csv_name <- paste0("landings_trends_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # ---- Disclaimer.txt (fixed name, no acronym/date) ----
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer landings trends data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # ---- Extra ICES ZIPs (keep original filenames) ----
        extra_urls <- c(
          "https://www.ices.dk/data/Documents/CatchStats/OfficialNominalCatches.zip",
          "https://www.ices.dk/data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
        )

        downloaded_paths <- character(0)
        for (u in extra_urls) {
          dest_path <- file.path(td, basename(u)) # original filename
          if (safe_download(u, dest_path)) {
            downloaded_paths <- c(downloaded_paths, dest_path)
          } else {
            note <- file.path(td, paste0("MISSING_", tools::file_path_sans_ext(basename(u)), ".txt"))
            writeLines(c(
              paste0("Could not download: ", u),
              "This file was unavailable at the time of packaging."
            ), con = note)
            downloaded_paths <- c(downloaded_paths, note)
          }
        }

        # ---- Zip everything ----
        files_to_zip <- c(csv_path, disc_path, downloaded_paths)

        if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
          zip::zipr(zipfile = file, files = files_to_zip, root = td)
        } else {
          owd <- setwd(td)
          on.exit(setwd(owd), add = TRUE)
          zip::zip(zipfile = file, files = basename(files_to_zip))
        }
      },
      contentType = "application/zip"
    )
    ############### Discards plots ##########################################################

    year <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()

    output$discard_trends <- renderPlotly({
      fig2 <- ggplotly(plot_discard_trends_app_plotly(CLD_trends(format_sag(shared$SAG, shared$SID)),
        year,
        ecoregion = get_ecoregion_acronym(selected_ecoregion())
      ))
      for (i in seq_along(fig2$x$data)) {
        if (!is.null(fig2$x$data[[i]]$name)) {
          fig2$x$data[[i]]$name <- gsub("\\(", "", strsplit(fig2$x$data[[i]]$name, ",")[[1]][1])
        }
      }
      fig2
    })

    output$recorded_discards <- renderPlotly({
      catch_trends2 <- CLD_trends(format_sag(shared$SAG, shared$SID)) %>% filter(Discards > 0)
      plot_discard_current_plotly(catch_trends2,
        year = year,
        position_letter = paste0("Current discards & Landings\nStocks with recorded discards (", year-1, ", ", get_active_region_acronym(selected_ecoregion()), ")"),
        ecoregion = get_ecoregion_acronym(selected_ecoregion())
      )
    })

    output$all_discards <- renderPlotly({
      plot_discard_current_plotly(CLD_trends(format_sag(shared$SAG, shared$SID)),
        year = year,
        position_letter = paste0("Current discards & Landings\nAll Stocks (", year-1, ", ", get_active_region_acronym(selected_ecoregion()), ")") ,
        ecoregion = get_ecoregion_acronym(selected_ecoregion())
      )
    })

    ############################### Download discard data bundle ###############################
    output$download_discard_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("discard_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # Temp workspace
        td <- tempfile("discard_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)


        # Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # ---- Build CSV (include acronym + date) ----
        # If CLD_trends() returns multiple measures, filter upstream if needed to "discards" only.
        dat <- CLD_trends(format_sag(shared$SAG, shared$SID))

        csv_name <- paste0("discard_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # ---- Disclaimer.txt (fixed name, no acronym/date) ----
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer discard data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # ---- Zip CSV + Disclaimer ----
        files_to_zip <- c(csv_path, disc_path)

        if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
          zip::zipr(zipfile = file, files = files_to_zip, root = td)
        } else {
          owd <- setwd(td)
          on.exit(setwd(owd), add = TRUE)
          zip::zip(zipfile = file, files = basename(files_to_zip))
        }
      },
      contentType = "application/zip"
    )

    ################################## Sidebar texts #########################################

    output$landings_text <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("landings_discards_", get_ecoregion_acronym(selected_ecoregion())), "landings"))
      )
    })

    output$discards_text <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("landings_discards_", get_ecoregion_acronym(selected_ecoregion())), "discards"))
      )
    })
  })
}

    
## To be copied in the UI
# mod_landings_ui("landings_1")
    
## To be copied in the server
# mod_landings_server("landings_1")
