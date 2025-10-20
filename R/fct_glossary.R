# R/glossary_terms.R

# ---- Inline lists (one source of truth)
# --- Overview
overview_terms <- list(
  list(term = "ICES ecoregion",
       def  = "Spatial units ICES uses to synthesise evidence for the ecosystem approach and to allocate/report advice."),
  list(term = "Fisheries overview",
       def  = "Summary for each ecoregion of fishing activities and their ecosystem effects, including catches, discards/bycatch, gears, and management context.")
)

# --- Landings
landings_terms <- list(
  list(term = "Catch",
       def  = "Total removals during fishing operations: landings plus discards."),
  list(term = "Landings",
       def  = "The portion of catch brought ashore and reported."),
  list(term = "Discards",
       def  = "The portion of catch taken aboard and returned to the sea.")
)

# --- Stock Status
stock_status_terms <- list(
  list(term = "FMSY",
       def  = "Fishing mortality consistent with achieving Maximum Sustainable Yield."),
  list(term = "MSY Btrigger",
       def  = "Biomass reference point that triggers a cautious response within the ICES MSY framework."),
  list(term = "Blim / Bpa",
       def  = "Blim: limit SSB below which recruitment is impaired; Bpa: precautionary SSB level set to keep SSB above Blim with high probability.")
)

# --- Mixed Fisheries
mixed_fisheries_terms <- list(
  list(term = "Mixed fisheries",
       def  = "Advice that considers stocks caught together and trade-offs across fleets and species."),
  list(term = "Technical interactions",
       def  = "Consequences of species being caught together in multistock, multigear fisheries."),
  list(term = "Métiers / fleet groupings",
       def  = "Categories of fishing activity used by ICES to estimate catch compositions and analyse interactions.")
)

# --- VMS
vms_terms <- list(
  list(term = "VMS & logbooks",
       def  = "ICES database of Vessel Monitoring System position reports and national logbook submissions used to map and analyse fishing activity."),
  list(term = "Fishing intensity (Swept Area Ratio, SAR)",
       def  = "Area swept by bottom-contacting gear expressed as a ratio of grid-cell area; used to map fishing intensity."),
  list(term = "Access & policy",
       def  = "Public summaries exist; detailed VMS/logbook data are governed by specific data calls and access conditions.")
)

# --- Bycatch
bycatch_terms <- list(
  list(term = "Bycatch of ETP species",
       def  = "Incidental capture of protected, endangered, or threatened species (marine mammals, seabirds, turtles, selected fishes) for which ICES issues advice."),
  list(term = "WGBYC",
       def  = "ICES Working Group that collates and assesses information on monitoring and assessment of protected-species bycatch.")
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
