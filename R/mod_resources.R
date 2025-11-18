#####
# modules/mod_resources.R
mod_resources_ui <- function(id) {
  ns <- NS(id)
  repo_url <- "https://github.com/ices-tools-prod/fisheriesXplorer"
  # Build the citation strings for display
  app_cite <- build_app_citation()

  bslib::navset_tab(
    id = ns("resources_nav"),

    # --- Contact & Feedback
    bslib::nav_panel(
      tagList(icon("envelope"), "Contact & Feedback"),
      tags$style(HTML("
        .msform-embed { position: relative; width: 100%; max-width: 1100px; }
        .msform-embed iframe { width: 100%; height: 70vh; border: 0; }
        @media (max-width: 600px){ .msform-embed iframe { height: 85vh; }
      ")),
      h3("Contact & Feedback"),
      p("We’d love to hear from you. For questions, bug reports, or suggestions:"),
      tags$ul(tags$li(HTML("Email: <a href='mailto:luca.lamoni@ices.dk'>luca.lamoni@ices.dk</a>"))),
      p("Or fill out the form below:"),
      div(
        class = "msform-embed",
        tags$iframe(
          src = "https://forms.office.com/e/vF0shUzLRk?embed=true",
          width = "640px", height = "480px", frameborder = "0",
          marginwidth = "0", marginheight = "0",
          style = "border: none; max-width:100%; max-height:100vh",
          loading = "lazy", allowfullscreen = NA, webkitallowfullscreen = NA,
          mozallowfullscreen = NA, msallowfullscreen = NA
        )
      ),
      p(
        em("Having trouble submitting inside the app? "),
        a("Open the form in a new tab.",
          href = "https://forms.office.com/e/vF0shUzLRk", target = "_blank", rel = "noopener"
        )
      )
    ),


    bslib::nav_panel(
      tagList(icon("database"), "Data Sources"),

      # Scoped CSS (works in BS3/BS5)
      tags$style(HTML("
                    .resources-page { margin-top: .5rem; }
                    .resources-page .intro-card, .resources-page .fair-card {
                      border: 1px solid #e5e7eb; border-radius: 12px; background:#fff;
                      padding: 12px 16px; margin-bottom: 16px;
                    }
                    .resources-grid { display: grid; grid-template-columns: 1fr; gap: 16px; margin-bottom: 16px; }
                    @media (min-width: 992px){ .resources-grid { grid-template-columns: 1fr 1fr; } }
                    .source-card { border: 1px solid #e5e7eb; border-radius: 12px; background:#fff; padding: 12px 16px; }
                    .source-title { font-weight: 600; margin: 0 0 6px 0; font-size: 2.15rem; }
                    .kv { margin: .25rem 0 0 0; }
                    .kv dt { width: 9.5rem; float: left; clear: left; color: #445; font-weight: 600; }
                    .kv dd { margin-left: 10rem; }
                    .source-notes { margin-top: 10px; font-size: .92em; color: #5f6b7a; background: #f8f9fa; border-radius: 8px; padding: 8px 10px; }
                    /* avoid clipped first letters with rounded corners */
                    .resources-page .source-card, .resources-page .intro-card, .resources-page .fair-card { overflow: visible; }
                  ")),
      div(
        class = "resources-page",

        # Intro
        div(
          class = "intro-card",
          h4(HTML("<b>About these data</b>")),
          p(
            "fisheriesXplorer displays (and cites) data from ICES services.",
            " Each source below lists what is used, where it comes from, how to access it, and how to cite/reuse it."
          )
        ),

        # Grid of standardized cards
        div(
          class = "resources-grid",

          # 1) ICES Spatial Facility
          resource_card(
            title = "ICES Spatial Facility (maps)",
            description = "Ecoregion layers and spatial context used across the app.",
            dataset_url = "https://gis.ices.dk/sf/index.html",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/4745e824-a612-4a1f-bc56-b540772166eb?language=all",
            services = list("Ecoregions shapefiles (zip)" = "https://gis.ices.dk/shapefiles/ICES_ecoregions.zip"),
            notes = "See dataset page for data access and conditions."
          ),

          # 2) Stock Information Database (SID)
          resource_card(
            title = "Stock Information Database (SID)",
            description = "Used to resolve stock metadata and keys used across the app.",
            dataset_url = "http://sid.ices.dk",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/ec374765-55e8-401a-b219-e011b231ae1b?language=all",
            services = list("APIs" = "http://sid.ices.dk/services/"),
            notes = "See dataset page for data access and conditions."
          ),

          # 3) Stock Assessment Graphs (SAG)
          resource_card(
            title = "Stock Assessment Graphs (SAG)",
            description = "Assessment outputs and status indicators (e.g., F/FMSY, SSB/MSY Btrigger) shown in status and trends views.",
            dataset_url = "https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/f5992b7d-b9da-40d4-81b9-d6db9e87e759?language=all",
            services = list("APIs" = "https://sag.ices.dk/sag_api/docs/swagger/index.html"),
            notes = "See dataset page for data access and conditions."
          ),

          # 4) Landings / Catch series
          resource_card(
            title = "Historical and Nominal catches",
            description = "Compiled landings (and related series) used in the Landings results.",
            dataset_url = "https://www.ices.dk/data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx",
            metadata_url = "https://gis.ices.dk/geonetwork/srv/api/records/7d242743-1069-417b-81e3-57f25c791a26",
            notes = "See dataset page for data access and conditions."
          ),

          # 5) Application source code (not a dataset, but important for reuse)
          resource_card(
            title = "Application source code",
            description = "Code for this application (versioning, issues, reproducibility).",
            services = list("GitHub" = "https://github.com/ices-tools-dev/fisheriesXplorer"),
            notes = "This application is open source under the MIT license. See the repository for code, issues, and contribution guidelines."
          ),
          resource_card(
            title = "Connected applications",
            description = "This app integrates with other ICES tools for data or embedded components.",
            services = list("adviceXplorer" = "https://ices-taf.shinyapps.io/advicexplorer/"),
            notes = "See the relevant application metadata record for details on data sources and terms."
          )
        ),

        # FAIR checklist
        div(
          class = "fair-card",
          h4("FAIR at a glance"),
          tags$ul(
            tags$li(HTML("<b>Findable</b>: clear titles, landing pages, and a metadata record.")),
            tags$li(HTML("<b>Accessible</b>: direct links to landing pages and services.")),
            tags$li(HTML("<b>Interoperable</b>: machine-readable formats (CSV/JSON) and documented schemas.")),
            tags$li(HTML("<b>Reusable</b>: citation text, version/build info, and licensing notes."))
          )
        )
      ),

      # Optional JSON-LD (kept from your version)
      tags$script(
        type = "application/ld+json",
        HTML(jsonlite::toJSON(
          list(
            "@context" = "https://schema.org", "@type" = "Dataset",
            "name" = "ICES Fish catch and stock assessment dataset",
            "url" = "https://www.ices.dk/data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx",
            "identifier" = "https://gis.ices.dk/geonetwork/srv/api/records/7d242743-1069-417b-81e3-57f25c791a26",
            "publisher" = list("@type" = "Organization", "name" = "ICES"),
            "license" = "See dataset page",
            "isAccessibleForFree" = TRUE
          ),
          auto_unbox = TRUE, pretty = TRUE
        ))
      )
    ),
    bslib::nav_panel(
      tagList(icon("exclamation-triangle"), "Data disclaimer & policy"),
      make_disclaimer_block()
    ),
    bslib::nav_panel(
      tagList(icon("quote-right"), "Citation"),
      tags$style(HTML("
        .cite-card { border:1px solid #e5e7eb; border-radius:12px; background:#fff; padding:12px 16px; }
        .cite-title { font-weight:600; font-size:1.15rem; margin:0 0 6px 0; }
        .codeblock { background:#f6f8fa; border-radius:6px; padding:8px 10px; white-space:pre-wrap;
                     font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace; }
        .cite-actions { margin-top:.5rem; display:flex; gap:.5rem; flex-wrap:wrap; }
        details summary { cursor:pointer; margin-top:.5rem; font-weight:600; }
      ")),
      div(class = "cite-card",
        div(class = "cite-title", "How to cite this application"),
        p("Please cite the application to support reproducibility:"),
        div(class = "codeblock", app_cite$text),

        tags$details(
          tags$summary("BibTeX"),
          div(class = "codeblock", app_cite$bibtex)
        ),
        tags$details(
          tags$summary("CSL-JSON"),
          div(class = "codeblock", jsonlite::toJSON(app_cite$csl, auto_unbox = TRUE, pretty = TRUE))
        ),

        div(class = "cite-actions",
          downloadButton(ns("dl_citations_txt"), "Download TXT"),
          downloadButton(ns("dl_citations_bib"), "Download BibTeX"),
          downloadButton(ns("dl_citations_csl"), "Download CSL-JSON")
        ),

        # Optional: machine-readable JSON-LD for the app
        tags$script(
          type = "application/ld+json",
          HTML(jsonlite::toJSON(
            list(
              "@context"="https://schema.org",
              "@type"="SoftwareApplication",
              "name"="fisheriesXplorer",
              "applicationCategory"="DataVisualization",
              "url"="https://ices-tools-dev.shinyapps.io/fisheriesXplorer/",
              "publisher"=list("@type"="Organization","name"="ICES"),
              "license"="https://creativecommons.org/licenses/by/4.0/"
            ), auto_unbox = TRUE, pretty = TRUE))
        )
      )
    )
  )
}

mod_resources_server <- function(
  id,
  bookmark_qs = reactive(NULL),
  set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {

    # RESTORE once, defer until after first flush, then push up
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("exec_summary", "introduction", "who_is_fishing") ### this needs to get fixed
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "tabs_overview", selected = wanted)
          isolate(set_subtab(wanted)) # one-arg setter
        }, once = TRUE)
      }
    })
    
    # REPORT on user changes, skip initial default
    observeEvent(input$tabs_overview,
      {
        set_subtab(input$tabs_overview) # one arg only
      },
      ignoreInit = TRUE
    )

    app_cite <- build_app_citation()

    output$dl_citations_txt <- downloadHandler(
      filename = function() sprintf("fisheriesXplorer_citation_%s.txt", Sys.Date()),
      content  = function(file) writeLines(app_cite$text, file)
    )

    output$dl_citations_bib <- downloadHandler(
      filename = function() sprintf("fisheriesXplorer_citation_%s.bib", Sys.Date()),
      content  = function(file) writeLines(app_cite$bibtex, file)
    )

    output$dl_citations_csl <- downloadHandler(
      filename = function() sprintf("fisheriesXplorer_citation_%s.json", Sys.Date()),
      content  = function(file) writeLines(jsonlite::toJSON(app_cite$csl, auto_unbox = TRUE, pretty = TRUE), file)
    )
  })
}
#-- end of mod_resources.R --#