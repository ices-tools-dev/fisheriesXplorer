#' Stock status UI module
#'
#' This module UI creates the main \emph{Stock status} section of the app.
#' It shows summary status plots, trends by functional group, Kobe/CLD
#' views, and a stock-level lookup table, all for the currently selected
#' ecoregion.
#'
#' @param id A character string used as the module namespace. Passed to
#'   \code{shiny::NS()} and used to namespace all UI elements within the
#'   module.
#'
#' @details
#' The layout consists of:
#' \itemize{
#'   \item A flexible header created by \code{mod_flex_header_ui()},
#'     displaying the ecoregion label and current date.
#'   \item A \code{bslib::navset_tab()} (\code{main_tabset}) with four
#'     \code{nav_panel()} views:
#'     \itemize{
#'       \item \strong{Status Summary} (\code{"status_summary"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text1}).
#'           \item Two cards containing static plots:
#'             \code{status_summary_ices} (MSY & Precautionary Approach)
#'             and \code{status_summary_ges} (catches vs MSY status),
#'             each with a combined data + graph download link
#'             (\code{download_clean_status_data},
#'             \code{download_status_catch_data}).
#'         }
#'       \item \strong{Trends by group} (\code{"trends_by_group"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text2}).
#'           \item A card with a radio-button selector
#'             (\code{status_trend_selector}) for functional groups
#'             (elasmobranch, benthic, shellfish, demersal, pelagic),
#'             a CSV download link (\code{download_trends_data}), and a
#'             \code{plotly} trends plot (\code{status_trends}).
#'         }
#'       \item \strong{Kobe-CLD} (\code{"kobe_cld"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text3}).
#'           \item Controls for group selection
#'             (\code{status_kobe_cld_selector}) and a year/period slider
#'             (\code{kobe_cld_slider}), plus a combined data + graphs
#'             download link (\code{download_CLD_data}).
#'           \item Two plots: CLD (\code{status_cld}) and Kobe diagram
#'             (\code{status_kobe}).
#'         }
#'       \item \strong{Stock status Lookup} (\code{"status_lookup"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text4}).
#'           \item A card with a \code{reactable} table
#'             (\code{stock_status_table_reactable}) and a CSV download
#'             link (\code{download_status_table}).
#'         }
#'     }
#' }
#'
#' The corresponding server logic is implemented in
#' \code{mod_stock_status_server()}.
#'
#' @return A \code{shiny.tag.list} representing the stock status UI,
#'   suitable for inclusion in a Shiny application.
#'
#' @importFrom shiny NS tagList fluidRow column uiOutput downloadLink
#'   radioButtons plotOutput
#' @importFrom bslib navset_tab nav_panel layout_sidebar sidebar card
#'   card_header card_body
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#' @importFrom reactable reactableOutput
#'
#' @export
mod_stock_status_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

    # Give the navset an id; give each nav_panel a stable value
    navset_tab(
      id = ns("main_tabset"),   

      nav_panel(
        "Status Summary", value = "status_summary",   
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text1"))
          ),
          fluidRow(
            column(
              6,
              card(
                height = "85vh", full_screen = TRUE,
                card_header(
                  "MSY & Precautionary Approach",
                  downloadLink(ns("download_clean_status_data"),
                    HTML(paste0("<span class='hovertext' data-hover='Data + graph'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                  )
                ),
                card_body(
                  fillable = TRUE,
                  withSpinner(plotOutput(ns("status_summary_ices"), height = "75vh"),
                              caption = "Getting status data...")
                )
              )
            ),
            column(
              6,
              card(
                height = "85vh", full_screen = TRUE,
                card_header(
                  "Catches in relation to MSY status",
                  downloadLink(ns("download_status_catch_data"),
                    HTML(paste0("<span class='hovertext' data-hover='Data + graph'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                  )
                ),
                card_body(
                  fillable = TRUE,
                  withSpinner(plotOutput(ns("status_summary_ges"), height = "75vh"),
                              caption = "Getting assessment data...")
                )
              )
            )
          )
        )
      ),

      nav_panel(
        "Trends by group", value = "trends_by_group",   
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text2"))
          ),
          column(
            12,
            card(
              height = "85vh", full_screen = TRUE,
              card_header(
                radioButtons(ns("status_trend_selector"), "Select group",
                  inline = TRUE,
                  choices = c(
                    "Elasmobranchs" = "elasmobranch",
                    "Benthic"       = "benthic",
                    "Shellfish"    = "shellfish",
                    "Demersal"      = "demersal",
                    "Pelagic"       = "pelagic"
                  )
                ),
                downloadLink(ns("download_trends_data"),
                  HTML(paste0("<span class='hovertext' data-hover='Download stock status trends (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                )
              ),
              card_body(withSpinner(plotlyOutput(ns("status_trends"), height = "68vh")))
            )
          )
        )
      ),

      nav_panel(
        "Kobe-CLD", value = "kobe_cld",  
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black", open = FALSE,
            uiOutput(ns("status_text3"))
          ),
          card(
            card_header(
              column(
                6,
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding: 0 16px;",
                  radioButtons(ns("status_kobe_cld_selector"), "Select group",
                    inline = TRUE,
                    choices = c(
                      "Benthic"   = "benthic",
                      "Demersal"  = "demersal",
                      "Shellfish"= "shellfish",
                      "Pelagic"   = "pelagic",
                      "All Stocks"= "All"
                    ),
                    selected = "All"
                  )
                )
              ),
              column(
                6,
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding: 0 16px;",
                  uiOutput(ns("kobe_cld_slider")),
                  downloadLink(ns("download_CLD_data"),
                    HTML(paste0("<span class='hovertext' data-hover='Data + graphs'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              card(fillable = TRUE, height = "70vh", full_screen = TRUE,
                   withSpinner(plotOutput(ns("status_cld"),  height = "67vh")))
            ),
            column(
              6,
              card(fillable = TRUE, height = "75vh", full_screen = TRUE,
                   withSpinner(plotOutput(ns("status_kobe"), height = "67vh")))
            )
          )
        )
      ),

      nav_panel(
        "Stock status Lookup", value = "status_lookup",   # <-- NEW value
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw", bg = "white", fg = "black",
            open = FALSE,
            uiOutput(ns("status_text4"))
          ),
          card(
            card_header(
              "Stock status table",
              downloadLink(ns("download_status_table"),
                HTML(paste0("<span class='hovertext' data-hover='Download table (csv)'><font size= 4>Download data <i class='fa-solid fa-cloud-arrow-down'></i></font></span>"))
              )
            ),
            card_body(withSpinner(reactableOutput(ns("stock_status_table_reactable"))))
          )
        )
      )
    )
  )
}

        
    
#' Server logic for stock status module
#'
#' This module server powers the \emph{Stock status} section of the app. It
#' manages bookmarking of the main status tabs, renders header labels and the
#' floating glossary, computes derived status objects, generates plots for all
#' four views (summary, trends, Kobe/CLD, lookup), and provides downloadable
#' ZIP bundles for each type of status output.
#'
#' @param id Module id, matching the id used in \code{mod_stock_status_ui()}.
#' @param cap_year Integer (optional) capture/reference year for the status
#'   data, used in captions and filenames where available.
#' @param cap_month Integer (optional) capture/reference month for the status
#'   data, used in captions and filenames where available.
#' @param selected_ecoregion A reactive expression returning the currently
#'   selected ecoregion name (character). Used for labelling, filtering, and
#'   in file naming tokens.
#' @param shared A list or environment containing shared data objects, typically
#'   including \code{SAG}, \code{SID}, and \code{clean_status}, which are used
#'   by helper functions such as \code{format_sag()}, \code{CLD_trends()},
#'   \code{stockstatus_CLD_current_proxy()}, \code{stock_trends_proxy()}, and
#'   \code{format_annex_table()}.
#' @param bookmark_qs A reactive returning a list of bookmark query-string
#'   parameters (including \code{subtab}) or \code{NULL}. Used to restore the
#'   selected main tab (\code{"status_summary"}, \code{"trends_by_group"},
#'   \code{"kobe_cld"}, \code{"status_lookup"}) on load.
#' @param set_subtab A callback function taking a single character argument
#'   (the current subtab id). Used to inform the parent app of tab changes for
#'   bookmarking or other side effects.
#'
#' @details
#' The module:
#' \itemize{
#'   \item Restores the active tab from \code{bookmark_qs()} on first load,
#'     using \code{bslib::nav_select()} when available, and reports it via
#'     \code{set_subtab()}.
#'   \item Observes changes to \code{input$main_tabset} (ignoring the initial
#'     default) and calls \code{set_subtab()} whenever the user switches tab.
#'   \item Renders a header showing:
#'     \itemize{
#'       \item The current ecoregion label (\code{ecoregion_label}).
#'       \item A "Last data update" timestamp (current system date).
#'       \item A floating glossary trigger wired up via
#'         \code{mod_glossary_float_server()}, populated from the
#'         \code{texts} table (type \code{"glossary"}).
#'     }
#'   \item Uses a common sidebar text block for all four tabs, taken from
#'     \code{select_text(texts, "status", "sidebar")}.
#'   \item For the \strong{Status Summary} tab:
#'     \itemize{
#'       \item Computes \code{catch_current()} from \code{shared$SAG} and
#'         \code{shared$SID} via \code{format_sag()},
#'         \code{add_proxyRefPoints()}, and
#'         \code{stockstatus_CLD_current_proxy()}.
#'       \item Renders MSY/PA pies with \code{plot_status_prop_pies()} and
#'         GES pies with \code{plot_GES_pies()}, using client data width for
#'         responsive sizing.
#'       \item Provides ZIP bundles for clean status data
#'         (\code{download_clean_status_data}) and GES/catch status
#'         (\code{download_status_catch_data}), each containing a CSV export,
#'         a disclaimer text (fetched via \code{safe_download()}), and a PNG
#'         plot (generated with \code{ragg::agg_png()} or
#'         \code{ggplot2::ggsave()} where possible).
#'     }
#'   \item For the \strong{Trends by group} tab:
#'     \itemize{
#'       \item Builds trends data with \code{stock_trends_proxy()} and
#'         \code{trends_data()}.
#'       \item Renders a \code{plotly} time-series plot via
#'         \code{plot_stock_trends()} for the selected fisheries guild(s).
#'       \item Exposes a ZIP download (\code{download_trends_data}) with the
#'         trends CSV and a disclaimer.
#'     }
#'   \item For the \strong{Kobe-CLD} tab:
#'     \itemize{
#'       \item Computes \code{kobe_cld_data()} from \code{catch_current()}
#'         filtered by the selected guild(s) and \code{plot_CLD_bar_app()}
#'         with \code{return_data = TRUE}.
#'       \item Renders \code{status_kobe} and \code{status_cld} plots by
#'         slicing the top \code{n} stocks and passing them to
#'         \code{plot_kobe_app()} and \code{plot_CLD_bar_app()}
#'         (\code{return_data = FALSE}).
#'       \item Provides a ZIP bundle (\code{download_CLD_data}) containing the
#'         filtered CSV, a disclaimer, and PNG images of both Kobe and CLD
#'         plots.
#'     }
#'   \item For the \strong{Stock status lookup} tab:
#'     \itemize{
#'       \item Builds a processed data frame suitable for a rich table via
#'         \code{format_annex_table()}, icon paths
#'         (\code{match_stockcode_to_illustration()}), and status icon
#'         mapping (\code{icon_mapping()}), pivoted to provide MSY and PA
#'         columns.
#'       \item Renders a \code{reactable} table with HTML columns, grouped
#'         headers, and filters.
#'       \item Exposes a ZIP bundle (\code{download_status_table}) with the
#'         raw annex table CSV and a disclaimer.
#'     }
#' }
#'
#' @return No direct return value; the function is called for its side
#'   effects of registering observers, reactives, outputs, and download
#'   handlers in a Shiny server context.
#'
#' @importFrom shiny moduleServer observeEvent updateTabsetPanel renderUI
#'   reactive req tags tagList downloadHandler sliderInput
#' @importFrom bslib nav_select
#' @importFrom plotly renderPlotly
#' @importFrom reactable renderReactable
#' @importFrom utils write.csv
#' @importFrom dplyr filter slice_max mutate select
#' @importFrom tidyr pivot_wider
#'
#' @export 
mod_stock_status_server <- function(
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
      valid <- c("status_summary", "trends_by_group", "kobe_cld", "status_lookup")
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          if (utils::packageVersion("bslib") >= "0.5.0") {
            bslib::nav_select(id = "main_tabset", selected = wanted, session = session)
          } else {
            updateTabsetPanel(session, "main_tabset", selected = wanted)
          }
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


    ################################## header + glossary #########################################
    output$ecoregion_label <- renderUI({
      req(selected_ecoregion())
      tags$span(tags$b("Ecoregion:"), " ", selected_ecoregion())
    })

    
    output$current_date <- renderUI({
      tab <- input$main_tabset
      if (is.null(tab)) tab <- "landings"
      
      date_text <- format(Sys.Date(), "%B %d, %Y")

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
    

    output$status_text1 <- output$status_text2 <- output$status_text3 <- output$status_text4 <- renderUI({
      HTML(select_text(texts, "status", "sidebar"))
    })

    ######################### Status summary tab #################################################

    catch_current <- reactive({
      stockstatus_CLD_current_proxy(add_proxyRefPoints(format_sag(shared$SAG, shared$SID), custom_refpoints_path = "data/custom_refpoints_2025.csv"))
    })

    output$status_summary_ices <- renderPlot({
      key <- "output_stock_status_1-status_summary_ices_width"
      req(!is.null(session$clientData[[key]]), session$clientData[[key]] > 0)
      w <- session$clientData[[key]]
      # plot_status_prop_pies(shared$clean_status, return_data = FALSE)
      plot_status_prop_pies(shared$clean_status, width_px = w, return_data = FALSE)
    })

    
    output$download_clean_status_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
       

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (with acronym + date)
        csv_name <- paste0("status_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        dat <- shared$clean_status
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer status data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- 3) Plot image (PNG) of the static pies
        png_name <- paste0("status_pie_plot_", acronym, "_", date_tag, ".png")
        png_path <- file.path(td, png_name)

        plot_ok <- FALSE
        try(
          {
            p <- plot_status_prop_pies(dat) # your static ggplot function
            if (inherits(p, "ggplot")) {
              # Prefer ragg for crisp text; fall back to ggsave
              if (requireNamespace("ragg", quietly = TRUE)) {
                ragg::agg_png(filename = png_path, width = 2200, height = 1400, units = "px", res = 144)
                print(p)
                grDevices::dev.off()
              } else {
                ggplot2::ggsave(
                  filename = png_path, plot = p, width = 14, height = 9,
                  dpi = 150, limitsize = FALSE
                )
              }
              plot_ok <- file.exists(png_path) && file.info(png_path)$size > 0
            }
          },
          silent = TRUE
        )

        if (!plot_ok) {
          writeLines(
            c(
              "Plot image could not be generated.",
              "Check that 'shared$clean_status' has these columns:",
              "StockKeyLabel, FisheriesGuild, lineDescription, FishingPressure, StockSize."
            ),
            con = file.path(td, "PLOT_GENERATION_FAILED.txt")
          )
        }

        # --- Zip everything
        files_to_zip <- c(csv_path, disc_path, if (plot_ok) png_path)
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

    ################################ GES pies ##################################################
    output$status_summary_ges <- renderPlot({
      key <- "output_stock_status_1-status_summary_ges_width" # adjust if different
      req(!is.null(session$clientData[[key]]), session$clientData[[key]] > 0)
      w <- session$clientData[[key]]

      plot_GES_pies(shared$clean_status, catch_current(), width_px = w, return_data = FALSE)
    })

    ############################### GES download ##################################################
    output$download_status_catch_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_catch_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # Temp workspace
        td <- tempfile("status_catch_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # 1) CSV (includes acronym + date)
        dat <- plot_GES_pies(shared$clean_status, catch_current(), return_data = TRUE)
        csv_name <- paste0("status_catch_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # 2) Disclaimer.txt (fixed name)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer status & catch data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # 3) PNG plot image
        png_name <- paste0("status_catch_pie_plot_", acronym, "_", date_tag, ".png")
        png_path <- file.path(td, png_name)
        plot_ok <- FALSE
        try(
          {
            p <- plot_GES_pies(shared$clean_status, catch_current(), return_data = FALSE)
            if (inherits(p, "ggplot")) {
              if (requireNamespace("ragg", quietly = TRUE)) {
                ragg::agg_png(filename = png_path, width = 2200, height = 1400, units = "px", res = 144)
                print(p)
                grDevices::dev.off()
              } else {
                ggplot2::ggsave(filename = png_path, plot = p, width = 14, height = 9, dpi = 150, limitsize = FALSE)
              }
              plot_ok <- file.exists(png_path) && file.info(png_path)$size > 0
            }
          },
          silent = TRUE
        )

        if (!plot_ok) {
          writeLines(
            c(
              "Plot image could not be generated.",
              "Check that 'plot_GES_pies' returns a ggplot object when return_data = FALSE."
            ),
            con = file.path(td, "PLOT_GENERATION_FAILED.txt")
          )
        }

        # Zip bundle
        files_to_zip <- c(csv_path, disc_path, if (plot_ok) png_path)
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


    ##################### Stock trends tab ###############################################
    trends_data <- reactive({
      stock_trends_proxy(add_proxyRefPoints(format_sag(shared$SAG, shared$SID), custom_refpoints_path = "data/custom_refpoints_2025.csv"))
    })

    output$status_trends <- renderPlotly({
      req(!is.null(input$status_trend_selector))
      if (input$status_trend_selector == "all_stocks") {
        guild <- c("demersal", "pelagic", "shellfish", "benthic", "elasmobranch")
      } else {
        guild <- input$status_trend_selector
      }
      plot_stock_trends(trends_data(), guild,  return_data = FALSE, ecoregion = get_ecoregion_acronym(selected_ecoregion()))
    })

    ######################### Download stock trends data ##########################################
    output$download_trends_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_trends_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_trends_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (includes acronym + date)
        dat <- trends_data()
        csv_name <- paste0("status_trends_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer trends data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- Zip bundle
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

    ######################### Kobe-CLD tab ################################################
    output$kobe_cld_slider <- renderUI({
      slider_max <- nrow(kobe_cld_data())
      div(
        id = "custom_slider",
        sliderInput(ns("n_selector"), 
          HTML("Choose <em>n</em> of stocks"),
          min = 1, 
          max = slider_max, 
          value = min(10, slider_max), 
          step = 1,
          width = "200px"
        )
      )
    })

    kobe_cld_data <- reactive({
      if (input$status_kobe_cld_selector == "All") {
        guild <- c("demersal", "pelagic", "shellfish", "benthic", "elasmobranch")
        tmp <- catch_current() %>% dplyr::filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, return_data = TRUE)
      } else {
        guild <- input$status_kobe_cld_selector
        tmp <- catch_current() %>% dplyr::filter(FisheriesGuild %in% guild)
        tmp <- plot_CLD_bar_app(tmp, guild = input$status_kobe_cld_selector, return_data = TRUE)
      }
    })
    ########################### Kobe-CLD plots ##############################################
    output$status_kobe <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
      req(!is.null(input$n_selector))
      plot_data <- kobe_cld_data() %>% dplyr::slice_max(order_by = total, n = input$n_selector)
      plot_kobe_app(plot_data, guild = input$status_kobe_cld_selector, return_data = FALSE)
    })

    output$status_cld <- renderPlot({
      req(!is.null(input$status_kobe_cld_selector))
      req(!is.null(input$n_selector))
      plot_data <- kobe_cld_data() %>% dplyr::slice_max(order_by = total, n = input$n_selector)
      plot_CLD_bar_app(plot_data, guild = input$status_kobe_cld_selector,  return_data = FALSE)
    })

    ######################### CLD/Kobe download ################################################
    output$download_CLD_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_CLD_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_CLD_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- Inputs for plots (with safe fallbacks)
        guild <- input$status_kobe_cld_selector %||% "All"
        n_sel <- input$n_selector
        if (is.null(n_sel) || !is.finite(n_sel) || n_sel <= 0) n_sel <- 10L

        # Optional caption tokens (fallback to current date if not in scope)
        capY <- if (exists("cap_year", inherits = TRUE)) get("cap_year") else format(Sys.Date(), "%Y")
        capM <- if (exists("cap_month", inherits = TRUE)) get("cap_month") else format(Sys.Date(), "%m")

        # --- 1) CSV (with acronym + date)
        dat <- kobe_cld_data()
        plot_data <- dat %>% dplyr::slice_max(order_by = total, n = n_sel)
        csv_name <- paste0("status_CLD_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(plot_data, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer CLD/Kobe data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- 3) PNGs: Kobe + CLD bar (saved at high resolution)
        kobe_png_name <- paste0("status_kobe_plot_", acronym, "_", date_tag, ".png")
        kobe_png_path <- file.path(td, kobe_png_name)
        cld_png_name <- paste0("status_CLD_bar_", acronym, "_", date_tag, ".png")
        cld_png_path <- file.path(td, cld_png_name)

        # Generate Kobe plot
        kobe_ok <- FALSE
        try(
          {
            p_kobe <- plot_kobe_app(
              plot_data,
              guild = guild,              
              return_data = FALSE
            )
            if (inherits(p_kobe, "ggplot")) {
              if (requireNamespace("ragg", quietly = TRUE)) {
                ragg::agg_png(filename = kobe_png_path, width = 2200, height = 1600, units = "px", res = 144)
                print(p_kobe)
                grDevices::dev.off()
              } else {
                ggplot2::ggsave(kobe_png_path, plot = p_kobe, width = 14, height = 10, dpi = 150, limitsize = FALSE)
              }
              kobe_ok <- file.exists(kobe_png_path) && file.info(kobe_png_path)$size > 0
            }
          },
          silent = TRUE
        )

        # Generate CLD bar plot
        cld_ok <- FALSE
        try(
          {
            p_cld <- plot_CLD_bar_app(
              plot_data,
              guild = guild,
              return_data = FALSE
            )
            if (inherits(p_cld, "ggplot")) {
              if (requireNamespace("ragg", quietly = TRUE)) {
                ragg::agg_png(filename = cld_png_path, width = 2200, height = 1400, units = "px", res = 144)
                print(p_cld)
                grDevices::dev.off()
              } else {
                ggplot2::ggsave(cld_png_path, plot = p_cld, width = 14, height = 9, dpi = 150, limitsize = FALSE)
              }
              cld_ok <- file.exists(cld_png_path) && file.info(cld_png_path)$size > 0
            }
          },
          silent = TRUE
        )

        if (!kobe_ok || !cld_ok) {
          msg <- c("One or more plot images could not be generated.")
          if (!kobe_ok) msg <- c(msg, "• Kobe plot failed.")
          if (!cld_ok) msg <- c(msg, "• CLD bar plot failed.")
          msg <- c(msg, "Check that plot_* functions return ggplot objects and inputs are available.")
          writeLines(msg, con = file.path(td, "PLOT_GENERATION_FAILED.txt"))
        }

        # --- Zip everything
        files_to_zip <- c(csv_path, disc_path, if (kobe_ok) kobe_png_path, if (cld_ok) cld_png_path)
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
    ##################### Stock status lookup tab ######################################################

    processed_data_reactable <- reactive({
      annex_data <- format_annex_table(shared$clean_status, as.integer(format(Sys.Date(), "%Y")), shared$SID, shared$SAG)
      
      annex_data_cleaned <- annex_data %>%
        dplyr::mutate(
          icon = paste0("<img src='", paste0("www/fish/", match_stockcode_to_illustration(StockKeyLabel, .)), "' height=30>"),
          StockKeyLabel = paste0("<a href='https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=", AssessmentKey, "&assessmentcomponent=", AssessmentComponent, "' target='_blank'>", StockKeyLabel, "</a>")
        ) %>%
        dplyr::select(
          "Stock code (_component)" = StockKeyLabel,
          " " = icon,
          "Stock Description" = StockKeyDescription,
          "Scientific Name" = SpeciesScientificName,
          "Common Name" = SpeciesCommonName,
          "Fisheries Guild" = FisheriesGuild,
          "Data Category" = DataCategory,
          "Assessment Year" = YearOfLastAssessment,
          "Advice Category" = AdviceCategory,
          "Approach" = lineDescription,
          "Fishing Pressure" = FishingPressure,
          "Stock Size" = StockSize
        ) %>%
        dplyr::mutate(Approach = tolower(Approach)) %>%
        tidyr::pivot_wider(
          names_from = Approach,
          values_from = c(`Fishing Pressure`, `Stock Size`),
          names_glue = "{Approach}_{.value}"
        ) %>%
        dplyr::mutate(
          `MSY Fishing Pressure` = sapply(`maximum sustainable yield_Fishing Pressure`, icon_mapping),
          `MSY Stock Size` = sapply(`maximum sustainable yield_Stock Size`, icon_mapping),
          `PA Fishing Pressure` = sapply(`precautionary approach_Fishing Pressure`, icon_mapping),
          `PA Stock Size` = sapply(`precautionary approach_Stock Size`, icon_mapping)
        ) %>%
        dplyr::select(
          -`maximum sustainable yield_Fishing Pressure`, -`maximum sustainable yield_Stock Size`,
          -`precautionary approach_Fishing Pressure`, -`precautionary approach_Stock Size`
        )
    })

    
    ##################################### Stock status table display #################################
    output$stock_status_table_reactable <- renderReactable({
      req(nrow(processed_data_reactable()) != 0)
      reactable::reactable(processed_data_reactable(),
        filterable = TRUE,
        defaultPageSize = 150,
        resizable = TRUE,
        wrap = TRUE,
        bordered = TRUE,
        columns = list(
          "Stock code (_component)" = reactable::colDef(html = TRUE, filterable = TRUE),
          " " = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "MSY Fishing Pressure" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "MSY Stock Size" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "PA Fishing Pressure" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          "PA Stock Size" = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center"))
        ),
        columnGroups = list(
          reactable::colGroup(name = "Maximum sustainable yield", columns = c("MSY Fishing Pressure", "MSY Stock Size")),
          reactable::colGroup(name = "Precautionary approach", columns = c("PA Fishing Pressure", "PA Stock Size"))
        )
      )
    })

    ######################### Stock status table download ##############################################
    output$download_status_table <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_table_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_table_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (with acronym + date)
        csv_name <- paste0("status_table_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        year_int <- as.integer(format(Sys.Date(), "%Y"))

        dat <- tryCatch(
          format_annex_table(shared$clean_status, year_int, shared$SID, shared$SAG),
          error = function(e) NULL
        )

        if (is.null(dat)) {
          writeLines(
            c(
              "Data generation failed in format_annex_table().",
              "Check inputs: shared$clean_status, shared$SID, shared$SAG."
            ),
            con = file.path(td, "DATA_GENERATION_FAILED.txt")
          )
        } else {
          utils::write.csv(dat, csv_path, row.names = FALSE)
        }

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer status table data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- Zip everything
        files_to_zip <- c(if (file.exists(csv_path)) csv_path, disc_path)
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

  })
}

    
## To be copied in the UI
# mod_stock_status_ui("stock_status_1")
    
## To be copied in the server
# mod_stock_status_server("stock_status_1")
