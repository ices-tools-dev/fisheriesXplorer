#' Create a leaflet map of ICES ecoregions
#'
#' Builds a \code{leaflet} map in a Lambert azimuthal equal-area projection
#' (EPSG:3035) showing a background Europe polygon and ICES ecoregion
#' polygons. The ecoregions are added both as a single group and as
#' individual groups per ecoregion, which can be controlled via
#' \code{leaflet} layer controls elsewhere in the app.
#'
#' @param eco_shape An \code{sf} or \code{sp} polygon object containing ICES
#'   ecoregions. Must include at least the columns \code{Ecoregion} and
#'   \code{OBJECTID}, which are used for labels, \code{layerId}, and groups.
#' @param map_shape An \code{sf} or \code{sp} polygon object providing the
#'   background map layer (typically a Europe outline) in the same CRS as
#'   \code{eco_shape}.
#'
#' @details
#' The map uses a custom \code{leafletCRS} with EPSG:3035 and a fixed range of
#' zoom levels. Three polygon layers are added:
#' \itemize{
#'   \item A background \code{map_shape} layer (group \code{"Europe"}).
#'   \item A grey \code{eco_shape} layer for all ecoregions (group
#'     \code{"Eco_regions"}) with labels from \code{Ecoregion}.
#'   \item A blue \code{eco_shape} layer with groups defined by
#'     \code{Ecoregion} and \code{layerId} from \code{OBJECTID}.
#' }
#' The map view is initialised over the North Atlantic / Europe and the
#' per-ecoregion groups are hidden by default using \code{hideGroup()}.
#'
#' @return A \code{leaflet} map widget that can be rendered in a Shiny
#'   application or an R Markdown document.
#'
#' @importFrom leaflet leaflet leafletOptions leafletCRS addPolygons setView hideGroup
#'
#' @examples
#' \dontrun{
#'   library(leaflet)
#'   map_ecoregion(eco_shape = ices_eco, map_shape = europe_outline)
#' }
#'
#' @export
map_ecoregion <- function(eco_shape, map_shape) {

  minZoom <- 0.5
  maxZoom <- 14
  resolutions <- 1.8 * (2^(maxZoom:minZoom))
  crs_laea <- leaflet::leafletCRS(
    crsClass = "L.Proj.CRS", code = "EPSG:3035",
    proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
    resolutions = resolutions
  )
  
  leaflet::leaflet(options = leaflet::leafletOptions(crs = crs_laea, minZoom = 1, maxZoom = 2, dragging = FALSE, )) %>%
    leaflet::addPolygons(
      data = map_shape,
      color = "black",
      weight = 1,
      fillOpacity = 0.4,
      fillColor = "#CCF0FC", # "#E8EAEA"
      group = "Europe"
    ) %>%
    leaflet::addPolygons(
      data = eco_shape,
      fillColor = "#E6E7E8",
      fillOpacity = 0.15,
      color = "black",
      stroke = TRUE,
      weight = 1,
      layerId = ~Ecoregion,
      group = "Eco_regions",
      label = ~Ecoregion
    ) %>%
    leaflet::addPolygons(
      data = eco_shape,
      fillColor = "#00B6F1",
      fillOpacity = .7,
      weight = 1,
      color = "black",
      stroke = TRUE,
      layerId = ~OBJECTID,
      group = ~Ecoregion
    ) %>%
    leaflet::setView(lng = -5, lat = 62.5, zoom = 0.5) %>%
    leaflet::hideGroup(group = eco_shape$Ecoregion)
}