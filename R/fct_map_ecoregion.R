#' @description Function to plot the interactive map of regions.
#'
#'
#' @noRd
#'
#' @param eco_shape (ecoregions' shapefile)
#' @param map_shape (europe's shapefile)
#'
#' @return leaflet object
#'
#' @examples
#' \dontrun{
#' map_ecoregion(eco_shape, map_shape)
#' }
#'
#'
#' @import leaflet
#'
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