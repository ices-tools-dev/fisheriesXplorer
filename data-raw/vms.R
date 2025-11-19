# Run devtools::load_all() before running this script

library(icesVMS)
library(icesFO)
library(icesVocab)
library(purrr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(stringr)


zip_sf <- function(dat, directory, zip_name, fname) {
  sf::write_sf(dat, paste0(directory, "/", fname, ".shp"))
  files <- dir(directory, pattern = fname, full = TRUE)
  files <- files[tools::file_ext(files) != "png"]
  zip::zip(paste0(directory, "/", zip_name, ".zip"), files)
  file.remove(files)
}

ecoregions <- sort(get_ecoregion_list())
ecoregions <- ecoregions[c(4:7,9:13, 15,16)]

#set year for effort and sar maps
yr <- 2024
# Set resolution for plotting
dpi <- 144 

# SAR and Effort updated November 2025
vms_data_update = "November 2025"
vms_data_update_short = "Nov-25"

CRS_LAEA_EUROPE <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

###### get shapefile for vms plot functions ######
atlantic_land_shp <- ne_countries(scale = 10, type = "map_units", 
                                  continent = c("Europe",
                                                "North America"), returnclass = "sf")

atlantic_land_shp <- atlantic_land_shp[, c("iso_a3", "iso_n3", "admin",
                                           "geometry")]



####### Get effort maps ############## Get effort maps ############## Get effort maps #######

effort_maps <- map(ecoregions, get_effort_map, year = yr)
names(effort_maps) <- ecoregions

gears <- c("Static", "Midwater", "Otter", "Demersal seine","Dredge", "Beam")

wrangle_effort <- function(effort_map) {
  effort_map <-
    effort_map %>%
      dplyr::filter(fishing_category_FO %in% gears) %>%
      dplyr::mutate(
        fishing_category_FO =
          dplyr::recode(fishing_category_FO,
                        Static = "Static gears",
                        Midwater = "Pelagic trawls and seines",
                        Otter = "Bottom otter trawls",
                        `Demersal seine` = "Bottom seines",
                        Dredge = "Dredges",
                        Beam = "Beam trawls"),
        mw_fishinghours = as.numeric(mw_fishinghours)
      ) %>%
      filter(!is.na(mw_fishinghours)) %>% 
      filter(mw_fishinghours != 0)
  
}

effort_maps <- map(effort_maps, wrangle_effort)
effort_maps <- map(effort_maps, ~ mutate(., geometry = st_as_sfc(wkt, crs = 4326)) %>%
                     select(-wkt) %>% 
                     st_sf)


usethis::use_data(effort_maps, overwrite = TRUE)

####### Get SAR maps ############## Get SAR maps ############## Get SAR maps #######

sar_maps <- map(ecoregions, get_sar_map, year = yr)
names(sar_maps) <- ecoregions
sar_maps[["Azores"]] <- NULL


sar_maps <- map(sar_maps, ~ {if(!is.null(.)) mutate(., geometry = st_as_sfc(wkt, crs = 4326)) %>% 
                  select(-wkt) %>%
                  st_sf}) 

sar_maps <- purrr::map(.x = sar_maps, .f = function(.x) tidyr::pivot_longer(.x, cols = c(surface_sar, subsurface_sar), values_to = "sar", names_to = "layer"))
sar_maps <- purrr::map(.x = sar_maps, .f = function(.x) dplyr::mutate(.x, layer = dplyr::recode(layer, 
                                                                                                surface_sar = "Surface",
                                                                                                subsurface_sar = "Subsurface")))

usethis::use_data(sar_maps, overwrite = TRUE)



# Make vms effort plots
gear <- c("all", "Static gears", "Pelagic trawls and seines", "Bottom otter trawls", "Bottom seines", "Dredges", "Beam trawls")
for(i in 1:length(ecoregions)){
    
  
  
  ecoregion_name <- get_ecoregion_acronym(ecoregions[i])
  zip_sf(dat = effort_maps[[ecoregions[i]]],
         directory = "data",
         zip_name = paste0("vms_effort_", ecoregion_name),
         fname = paste("vms_effort", ecoregion_name, vms_data_update_short, sep = "_")
          )
  
  for(j in 1:length(gear)) {
  
  gear_name  <- tolower(gear[j])
  gear_name  <- str_replace_all(gear_name, " ", "_")
    
  name_of_file <- paste0("inst/app/www/vms/",ecoregion_name, "_effort_", gear_name, ".png")
  
  # create plot safely; skip saving plot if warnings as "skip"
  result <-  tryCatch({
     p <- plot_effort_map_app(effort_maps[[ecoregions[i]]], 
                              ecoregion_name = ecoregions[i],
                              ecoregion_shape = ecoregion[[ecoregions[i]]],
                              land_shape = atlantic_land_shp,
                              fishing_category = gear[j],
                              crs = CRS_LAEA_EUROPE,
                              data_update_date = vms_data_update,
                              yr = yr)
      
    
     ggplot2::ggplot_build(p)
     p
    
  },
  warning = function(w) {
    warning(sprintf("Plot creation for %s produced a warning: %s", name_of_file, conditionMessage(w)))
    NULL
  },
  error = function(e) {
    warning(sprintf("Plot creation for %s failed: %s", name_of_file, conditionMessage(e)))
    NULL
  })
  
  if (!is.null(result) && inherits(result, "ggplot")) {
      ragg::agg_png(filename = name_of_file, units = "px",width = 1300, height = 1000, res = dpi)
      print(result)
      grDevices::dev.off()
        
  } else {
    message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
  }
  }
}


# Make vms SAR plots

layers <- c("all", "surface", "subsurface")
for(i in 1:length(sar_maps)){
  
  if(!is.null(sar_maps[[i]][1][[1]])){
    
    ecoregion_name <- get_ecoregion_acronym(names(sar_maps[i]))
    zip_sf(dat = sar_maps[[names(sar_maps[i])]],
           directory = "data",
           zip_name = paste0("vms_sar_", ecoregion_name),
           fname = paste("vms_sar", ecoregion_name, vms_data_update_short, sep = "_")
    )
    
    
    
    for(j in 1:length(layers)) {
      
      name_of_file <- paste0("inst/app/www/vms/",ecoregion_name, "_sar_", layers[j], ".png")
      
      
      result <-  tryCatch({
        p <- plot_sar_map_app(sar_maps[[names(sar_maps[i])]], 
                              ecoregion_name = names(sar_maps[i]), 
                              ecoregion_shape = ecoregion[[names(sar_maps[i])]],
                              land_shape = atlantic_land_shp, 
                              sar_layer = layers[j],
                              crs = CRS_LAEA_EUROPE,
                              data_update_date = vms_data_update,
                              yr = yr) 
        
        ggplot2::ggplot_build(p)
        p
        
      },
      warning = function(w) {
        warning(sprintf("Plot creation for %s produced a warning: %s", name_of_file, conditionMessage(w)))
        NULL
      },
      error = function(e) {
        warning(sprintf("Plot creation for %s failed: %s", name_of_file, conditionMessage(e)))
        NULL
      })
      
      if (!is.null(result) && inherits(result, "ggplot")) {
        ragg::agg_png(filename = name_of_file, units = "px",width = 1300, height = 1000, res = dpi)
        print(result)
        grDevices::dev.off()
      } else {
        message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
      }
  }
  }
}
