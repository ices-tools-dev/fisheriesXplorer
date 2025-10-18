resource_card <- function(title, description,
                          dataset_url  = NULL,
                          metadata_url = NULL,
                          services     = NULL,   # named list OR (named/unnamed) character vector
                          repo_url     = NULL,
                          app_url      = NULL,
                          notes        = NULL) {

  # normalize services to a named list
  svc <- NULL
  if (!is.null(services)) {
    if (is.list(services)) {
      svc <- services
    } else if (is.character(services)) {
      if (is.null(names(services)) || any(names(services) == "")) {
        names(services) <- paste("Service", seq_along(services))
      }
      svc <- as.list(services)
    }
  }

  # build the list items
  items <- list()
  if (!is.null(dataset_url))
    items <- c(items, list(tags$li(a("Dataset page",   href = dataset_url,  target = "_blank", rel = "noopener"))))
  if (!is.null(metadata_url))
    items <- c(items, list(tags$li(a("Metadata record", href = metadata_url, target = "_blank", rel = "noopener"))))
  if (!is.null(svc)) {
    items <- c(items, lapply(seq_along(svc), function(i) {
      nm  <- names(svc)[i]
      url <- unname(svc[[i]])
      tags$li(a(nm, href = url, target = "_blank", rel = "noopener"))
    }))
  }
  if (!is.null(repo_url))
    items <- c(items, list(tags$li(a("GitHub repository", href = repo_url, target = "_blank", rel = "noopener"))))
  if (!is.null(app_url))
    items <- c(items, list(tags$li(a("Application link",  href = app_url,  target = "_blank", rel = "noopener"))))

  tags$div(class = "source-card",
    tags$div(class = "source-title", title),
    tags$p(description),
    if (length(items)) tags$ul(class = "source-links", do.call(tagList, items)),
    if (!is.null(notes)) tags$p(class = "source-notes", notes)
  )
}

make_disclaimer_block <- function() {
  tagList(
    tags$style(HTML("
      .disclaimer-card{border:1px solid #e5e7eb;border-radius:12px;background:#fff;padding:12px 16px;margin-bottom:16px;}
      .disclaimer-card h3{margin-top:0}
      .cc-strip{display:flex;align-items:center;gap:.5rem;margin-top:.25rem}
      .cc-strip img{height:36px}
      .disclaimer-card ul{margin:0 0 0 1.1rem}
    ")),

    div(class = "disclaimer-card",
      h3(HTML("<b>Data disclaimer</b>")),

      p("The fisheriesXplorer Data Disclaimer can be found ",
        a("here", href = "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt", target = "_blank", rel = "noopener"), "."),      
      p("The general ICES Data Disclaimer can be found ",
        a("here", href = "https://www.ices.dk/Pages/Disclaimer.aspx", target = "_blank", rel = "noopener"), "."),

    br(),

      h3(HTML("<b>ICES Data Policy</b>")),
      p("Under the revised ICES Data Policy (2021), public data are available under ",
        a("CC BY 4.0", href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", rel = "noopener"),
        ", and data products are by default publicly available."),

      tags$ul(
        tags$li("Exclusions to unrestricted public access (relevant to fisheriesXplorer) include:"),
        tags$ul(
          tags$li("Commercial catch data from RDB-FishFrame and InterCatch"),
          tags$li("VMS and Logbook data")
        )
      ),
      p(
        "See the full policy on the ICES website.",
        a("ICES Data Policy", href = "https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx",
          target = "_blank", rel = "noopener"), "."
      ),
      div(class = "cc-strip",
        img(src = "www/by.png", alt = "CC BY 4.0"),
        a("creativecommons.org/licenses/by/4.0/", href = "https://creativecommons.org/licenses/by/4.0/",
          target = "_blank", rel = "noopener")
      )
    )
  )
}



# Build strings for the application citation
build_app_citation <- function(
  app_name    = "fisheriesXplorer",
  org         = "ICES",
  authors     = c("ICES"),
  year        = format(Sys.Date(), "%Y"),
  app_url     = "https://ices-tools-dev.shinyapps.io/fisheriesXplorer/",
  repo_url    = "https://github.com/ices-tools-prod/fisheriesXplorer",
  version     = Sys.getenv("APP_VERSION", unset = NA),
  commit      = Sys.getenv("APP_COMMIT",  unset = NA),
  license     = "CC BY 4.0",
  access_date = as.character(Sys.Date()),
  doi         = NULL
){
  verbits <- na.omit(c(version, commit))
  verbits <- if (length(verbits)) paste0(" (", paste(verbits, collapse = ", "), ")") else ""

  text <- sprintf(
    "%s (%s). %s%s [Shiny application]. %s. Accessed: %s. Repository: %s.%s",
    paste(authors, collapse = ", "), year, app_name, verbits, app_url, access_date, repo_url,
    if (!is.null(doi)) paste0(" DOI: ", doi) else ""
  )

  bibkey <- gsub("\\W+", "", paste0(app_name, year))
  bibtex <- sprintf(
"@software{%s,
  author  = {%s},
  title   = {%s},
  year    = {%s},
  url     = {%s},
  version = {%s},
  note    = {Repository: %s; Accessed: %s%s}
}",
    bibkey, paste(authors, collapse = " and "), paste0(app_name, verbits), year,
    app_url, ifelse(is.na(version), "", version), repo_url, access_date,
    if (!is.null(doi)) paste0("; DOI: ", doi) else ""
  )

  csl <- list(
    type  = "software",
    id    = bibkey,
    title = paste0(app_name, verbits),
    author = lapply(authors, function(a) list(literal = a)),
    issued = list("date-parts" = list(list(as.integer(year)))),
    URL     = app_url,
    version = if (!is.na(version)) version else NULL,
    `container-title` = org,
    note = paste("Repository:", repo_url, "| Accessed:", access_date, if (!is.null(doi)) paste("| DOI:", doi) else ""),
    DOI  = doi
  )

  list(text = text, bibtex = bibtex, csl = csl)
}


