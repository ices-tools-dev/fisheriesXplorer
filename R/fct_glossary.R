# R/glossary_terms.R
# One source of truth — terms paraphrased from ICES Glossary

# --- Overview
overview_terms <- list(
  list(term = "ICES ecoregion",
       def  = "Large marine area used by ICES for organising advice, assessments, and reporting."),
  list(term = "Fisheries overview",
       def  = "ICES summary of fishing activities and pressures in an ecoregion, used to support advice.")
)

# --- Landings
landings_terms <- list(
  list(term = "Catch",
       def  = "Total quantity taken by fishing operations, including landings and discards."),
  list(term = "Landings",
       def  = "Portion of the catch retained and brought ashore."),
  list(term = "Discards",
       def  = "Portion of the catch returned to the sea (alive or dead)."),
  list(term = "Fisheries guilds",
       def  = paste0(
         "Labels used by ICES to group stocks/fisheries at a broad level. Common guilds are:",
         "<ul style='margin:.25rem 0 0 .9rem'>",
           "<li><b>Pelagic</b> — species living and fished in the open-water column, away from the seabed.</li>",
           "<li><b>Demersal</b> — species associated with or near the seabed; often taken by bottom-contact gears.</li>",
           "<li><b>Benthic</b> — bottom-dwelling species closely tied to the seabed.</li>",
           "<li><b>Crustacean</b> — fisheries targeting crustaceans (e.g., shrimp, crab, lobster).</li>",
           "<li><b>Elasmobranchs</b> — cartilaginous fishes such as sharks, skates, and rays.</li>",
         "</ul>"
       ))
)

# --- Stock Status
stock_status_terms <- list(
  list(term = "Maximum Sustainable Yield (MSY)",
       def  = "Largest average catch that can be continuously taken from a stock under existing environmental conditions."),
  list(term = "FMSY",
       def  = "Fishing mortality that achieves Maximum Sustainable Yield."),
  list(term = "MSY Btrigger",
       def  = "Spawning-stock biomass level that triggers a more cautious response within the ICES MSY approach."),
  list(term = "Blim / Bpa",
       def  = "Blim: limit SSB below which recruitment is impaired; Bpa: precautionary SSB set to keep SSB above Blim with high probability.")
)

# --- Mixed Fisheries
mixed_fisheries_terms <- list(
  list(term = "Mixed fisheries",
       def  = "Fisheries where several stocks are caught together; advice considers trade-offs across fleets and species."),
  list(term = "Technical interactions",
       def  = "Consequences of species being caught together that constrain catches across stocks."),
  list(term = "Métier / fleet grouping",
       def  = "Category of fishing activity defined by gear/target/area/season used to analyse catches and interactions.")
)

# --- VMS
vms_terms <- list(
  list(term = "Vessel Monitoring System (VMS)",
       def  = "Satellite-based system providing vessel positions for monitoring and analysis of fishing activity."),
  list(term = "Swept Area Ratio (SAR)",
       def  = "Area swept by bottom-contacting gear expressed as a ratio of the area of a grid cell (a measure of fishing intensity)."),
  list(term = "Logbook",
       def  = "Official record of a vessel’s fishing operations (e.g., area, gear, catches).")
)

# --- Bycatch
bycatch_terms <- list(
  list(term = "Bycatch",
       def  = "Incidental capture of non-target species or sizes during fishing."),
  list(term = "ETP species",
       def  = "Endangered, Threatened, and/or Protected species considered in ICES bycatch work."),
  list(term = "WGBYC",
       def  = "ICES Working Group on Bycatch of Protected Species.")
)

# ---- Index + accessor (avoid sprinkling object names around the app)
.glossary_index <- list(
  "Overview"       = overview_terms,
  "Landings"       = landings_terms,
  "Stock Status"   = stock_status_terms,
  "Mixed Fisheries"= mixed_fisheries_terms,
  "VMS"            = vms_terms,
  "Bycatch"        = bycatch_terms
)

glossary_for <- function(page) {
  .glossary_index[[page]] %||% list()
}
