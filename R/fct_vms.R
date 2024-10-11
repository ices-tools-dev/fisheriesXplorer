CRS_LAEA_EUROPE <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


plot_effort_map_app <- function (effort, ecoregion, europe_shape, fishing_category, crs) 
{
 
  
  box <- sf::st_bbox(ecoregion)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])
  
  if(fishing_category != "all") {
    effort <- effort %>% dplyr::filter(fishing_category_FO == fishing_category)
  }
  p <- ggplot2::ggplot() + ggplot2::geom_sf(data = ecoregion, color = "grey90", fill = "transparent") + 
    ggplot2::geom_sf(data = europe_shape,fill = "grey80", color = "grey90", size = 1) + 
    ggplot2::geom_sf(data = effort, ggplot2::aes(fill = icesFO:::get_map_breaks(mw_fishinghours)), col = "transparent") + 
    ggplot2::scale_fill_viridis_d(name = "MW Fishing Hours", direction = -1, option = "A", guide = ggplot2::guide_legend(reverse = TRUE)) + 
    ggplot2::theme(#plot.caption = ggplot2::element_text(size = 10), 
                   #plot.subtitle = ggplot2::element_text(size = 11), 
                   axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()) + 
    ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) + 
    ggplot2::labs(caption = "Made with Natural Earth and ICES Marine Data") + 
    ggplot2::theme_bw(base_size = 11) 
  
  if(fishing_category == "all") {
    p <- p + ggplot2::facet_wrap(~fishing_category_FO)
  }
  p
}


plot_sar_map_app <- function (sar, ecoregion, europe_shape, layer, crs) 
{
  what <- match.arg(layer, c("surface", "subsurface"))
  what <- paste0(what, "_sar")
  if (what == "surface_sar") {
    legend_name = "Surface Swept \nArea Ratio"
  }
  else {
    legend_name = "Subsurface Swept\nArea Ratio"
  }
 
  sar$val <- as.numeric(sar[[what]])
  sar <- dplyr::filter(sar, val > 0)
  box <- sf::st_bbox(ecoregion)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])
  p <- ggplot2::ggplot() + ggplot2::geom_sf(data = ecoregion, color = "grey90", fill = "transparent") + 
    ggplot2::geom_sf(data = europe_shape,fill = "grey80", color = "grey90") + 
    ggplot2::geom_sf(data = sar, ggplot2::aes(fill = icesFO:::get_map_breaks(val)), col = "transparent") + 
    ggplot2::scale_fill_viridis_d(name = legend_name, direction = -1, 
                                  option = "A", guide = ggplot2::guide_legend(reverse = TRUE)) + 
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 6), 
                   plot.subtitle = ggplot2::element_text(size = 7), 
                   axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()) + 
    ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) + 
    ggplot2::labs(caption = "Made with Natural Earth and ICES Marine Data") + 
    ggplot2::theme_bw(base_size = 11)

  p
}
