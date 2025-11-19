#' Title
#'
#' @param effort 
#' @param ecoregion 
#' @param land_shape 
#' @param fishing_category 
#' @param crs 
#' @param data_update 
#' @import ggplot2
#' @importFrom sf st_transform st_bbox
#' @importFrom dplyr filter
plot_effort_map_app <- function (effort, ecoregion_name, ecoregion_shape, land_shape, fishing_category, crs, data_update_date, yr) {
  
  ecoregion_shape <- st_transform(ecoregion_shape, crs = crs)
  box <- st_bbox(ecoregion_shape)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])
  
  
  if(fishing_category != "all") {
    effort <- effort %>% filter(fishing_category_FO == fishing_category)
  }
  p <- ggplot() + geom_sf(data = ecoregion_shape, color = "grey90", fill = "transparent") + 
    geom_sf(data = land_shape,fill = "grey80", color = "grey90", size = 1) + 
    geom_sf(data = effort, aes(fill = icesFO:::get_map_breaks(mw_fishinghours)), col = "transparent") + 
    scale_fill_viridis_d(name = "MW Fishing Hours", direction = -1, option = "A", guide = guide_legend(reverse = TRUE)) + 
    theme_bw(base_size = 15) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank()) + 
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) + 
    labs(caption = paste0("Made with Natural Earth and ICES VMS Data \nVMS data updated ", data_update_date))+
    ggtitle(paste0("Average MW Fishing hours ", paste(yr-3, yr, sep = "-")),
            subtitle = paste0(ecoregion_name, ": ", fishing_category))
  
  if(fishing_category == "all") {
    p <- p + facet_wrap(~fishing_category_FO)+
      theme(strip.text = element_text(size = 11))+
      ggtitle(paste0("Average MW Fishing hours ", paste(yr-3, yr, sep = "-")),
              subtitle = paste0(ecoregion_name, ": All gears"))
  }
  p
}



#' Title
#'
#' @param sar_data 
#' @param ecoregion 
#' @param land_shape 
#' @param sar_layer 
#' @param crs 
#' @import ggplot2
#' @importFrom sf st_transform st_bbox
#' @importFrom dplyr filter
#' @importFrom stringr str_to_title
plot_sar_map_app <- function (sar_data,  ecoregion_name, ecoregion_shape, land_shape, sar_layer, crs, data_update_date, yr) {
 
  if(sar_layer != "all") {
    legend_name  <-  paste0(stringr::str_to_title(sar_layer), " Swept\nArea Ratio")
    
    sar_data$sar <- as.numeric(sar_data$sar)
    sar_data$layer <- tolower(sar_data$layer)
    sar_data <- filter(sar_data, layer == sar_layer & sar > 0)
  } else {
    
    legend_name  <-  "Swept\nArea Ratio"
  }
  
  ecoregion_shape <- st_transform(ecoregion_shape, crs = crs)
  box <- st_bbox(ecoregion_shape)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])
  
  p <- ggplot() + geom_sf(data = ecoregion_shape, color = "grey90", fill = "transparent") + 
    geom_sf(data = land_shape,fill = "grey80", color = "grey90") + 
    geom_sf(data = sar_data, aes(fill = icesFO:::get_map_breaks(sar)), col = "transparent") + 
    scale_fill_viridis_d(name = legend_name, direction = -1, 
                                  option = "A", guide = guide_legend(reverse = TRUE)) + 
    theme_bw(base_size = 15) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank()) + 
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) + 
    labs(caption = paste0("Made with Natural Earth and ICES VMS Data \nVMS data updated ", data_update_date)) + 
    ggtitle(paste0("Swept Area Ratio ", paste(yr-3, yr, sep = "-")),
          subtitle = paste0(ecoregion_name, ": ", stringr::str_to_title(sar_layer), " layer"))
  
  if(sar_layer == "all") {

    p <- p + facet_wrap(~layer)+
      theme(strip.text = element_text(size = 11)) +
      ggtitle(paste0("Swept Area Ratio ", paste(yr-3, yr, sep = "-")),
              subtitle = paste0(ecoregion_name, ": Surface and Subsurface layers"))
  }
  p
}

vms_bundle_filename <- function(selected_ecoregion, what) {
  function() {
    ecoregion <- selected_ecoregion()
    acronym  <- get_ecoregion_acronym(ecoregion)
    date_tag <- format(Sys.Date(), "%d-%b-%y")
    paste0("vms_", what, "_data_bundle_", acronym, "_", date_tag, ".zip")
  }
}

#' Title Bundle vms content for download, either effort or sar data
#'
#' @param selected_ecoregion reactive value
#' @param what character, either "sar" or "effort 
#'
#' @importFrom zip zip zipr
vms_bundle_content <- function(selected_ecoregion, what) {

    function(file) {
    
    # --- Naming tokens
    ecoregion <- selected_ecoregion()
    acronym <- get_ecoregion_acronym(ecoregion)
    # date_tag <- format(Sys.Date(), "%d-%b-%y")
    
    # --- 1) zipped shapefiles (with acronym + date)
    shp_zip_path <- file.path("data/", paste0("vms_", what ,"_", acronym, ".zip"))
    
    
    # --- 2) Disclaimer.txt (fixed name; no acronym/date)
    # --- Temp workspace
    td <- tempfile("status_bundle_")
    dir.create(td, showWarnings = FALSE)
    on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
    
    
    disc_path_fx <- file.path(td, "Disclaimer_fisheriesXplorer.txt")
    disc_url_fx <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
    if (!safe_download(disc_url_fx, disc_path_fx)) {
      writeLines(c(
        "Disclaimer for fisheriesXplorer.",
        "The official disclaimer could not be fetched automatically.",
        paste("Please see:", disc_url_fx)
      ), con = disc_path_fx)
    }
    
    disc_path_vms <- file.path(td, "Disclaimer_VMS.txt")
    disc_url_vms <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/disclaimer_vms_data_ouput.txt"
    if (!safe_download(disc_url_vms, disc_path_vms)) {
      writeLines(c(
        "Disclaimer for VMS data output.",
        "The official disclaimer could not be fetched automatically.",
        paste("Please see:", disc_url_vms)
      ), con = disc_path_vms)
    }
    
    # --- 3) Plot image (PNG) of the static pies
    match_pattern <- paste0(acronym, "_", what, "_")
    image_path <- app_sys(paste0("app/www/vms"))
    vms_files <- list.files(image_path)
    required_files <- vms_files[str_starts(vms_files, pattern = match_pattern)]
    image_path <- paste(image_path, required_files, sep = "/")
    
    
    # --- Zip everything
    files_to_zip <- c(shp_zip_path, disc_path_fx, disc_path_vms, image_path)
    if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
      zipr(zipfile = file, files = files_to_zip)
    } else {
      owd <- setwd(td)
      on.exit(setwd(owd), add = TRUE)
      zip(zipfile = file, files = basename(files_to_zip))
    }
  }
}


render_vms <- function(ecoregion, gear, what, ns){

  
  eco_acronym <- get_ecoregion_acronym(ecoregion)
  gear_name <- str_replace_all(tolower(gear), " ", "_")
  file_name <- paste0(eco_acronym, "_", what, "_", gear_name, ".png")
  
  # Web path used by img tag
  webpath <- file.path("www/vms", file_name)
  
  # Filesystem path used for existence check
  file_systempath <- app_sys("app/www/vms", file_name)
  
  validate(
    need(file.exists(file_systempath),
         paste("No data available for",
               gear,
               "in the", ecoregion, "ecoregion"))
  )
  
  tags$img(
    id = ns(paste0("vms_", what, "_layer")),
    src = webpath,
    style = "width: 100%; cursor: pointer;",
    onclick = "toggleFullScreen(this)"
  )
}

