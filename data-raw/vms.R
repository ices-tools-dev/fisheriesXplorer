library(icesVMS)
library(icesFO)
library(icesVocab)
library(purrr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(stringr)
devtools::load_all()

ecoregions <- sort(get_ecoregion_list())
ecoregions <- ecoregions[c(4:7,9:13, 15,16)]


####### Get effort maps ############## Get effort maps ############## Get effort maps #######

effort_maps <- map(ecoregions, get_effort_map, year = 2022)
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

sar_maps <- map(ecoregions, get_sar_map, year = 2022)
names(sar_maps) <- ecoregions
sar_maps[["Azores"]] <- NULL


sar_maps <- map(sar_maps, ~ {if(!is.null(.)) mutate(., geometry = st_as_sfc(wkt, crs = 4326)) %>% 
                  select(-wkt) %>%
                  st_sf}) 
usethis::use_data(sar_maps, overwrite = TRUE)


###### get europe shape for vms plot functions ######
europe_land_shp <- ne_countries(scale = 10, type = "countries", 
                                                continent = "europe", returnclass = "sf")
europe_land_shp <- europe_land_shp[, c("iso_a3", "iso_n3", "admin", 
                                 "geometry")]

usethis::use_data(europe_land_shp, overwrite = T)


# Make vms effot plots
gear <- c("all", "Static gears", "Pelagic trawls and seines", "Bottom otter trawls", "Bottom seines", "Dredges", "Beam trawls")
for(i in 1:length(ecoregions)){
    

  ecoregion_name <- get_ecoregion_acronym(ecoregions[i])
  
  for(j in 1:length(gear)) {
  
  gear_name  <- tolower(gear[j])
  gear_name  <- str_replace_all(gear_name, " ", "_")
    
  name_of_file <- paste0("inst/app/www/vms/",ecoregion_name, "_effort_", gear_name, ".jpg")
  
  tryCatch(
    {
      p <- plot_effort_map_app(effort_maps[[ecoregions[i]]], 
                               ecoregion[[ecoregions[i]]], 
                               europe_shape = europe_land_shp, 
                               fishing_category = gear[j],
                               crs = CRS_LAEA_EUROPE) +
        ggtitle(paste0("Average MW Fishing hours ", paste(2019, 2022, sep = "-")))
      
      
      ggsave(
        filename = name_of_file,
        plot = p,
        width = 4961 / 300,   
        height = 3508 / 300,
        units = "in",
        dpi = 300
      )
          
    },
    error = function(e) {
      warning(sprintf("Skipping %s due to error: %s", name_of_file, conditionMessage(e)))
      NULL
    }
  )
  
  }
}


# Make vms SAR plots

sar_maps <- purrr::map(.x = sar_maps, .f = function(.x) tidyr::pivot_longer(.x, cols = c(surface_sar, subsurface_sar), values_to = "sar", names_to = "layer"))
sar_maps <- purrr::map(.x = sar_maps, .f = function(.x) dplyr::mutate(.x, layer = dplyr::recode(layer, 
                                                                                        surface_sar = "Surface",
                                                                                        subsurface_sar = "Subsurface")))

layers <- c("all", "surface", "subsurface")
for(i in 1:length(sar_maps)){
  
  ecoregion_name <- get_ecoregion_acronym(names(sar_maps[i]))
  
  for(j in 1:length(layers)) {
    
    name_of_file <- paste0("inst/app/www/vms/",ecoregion_name, "_sar_", layers[j], ".jpg")
    
    tryCatch(
      {
        p <- plot_sar_map_app(sar_maps[[names(sar_maps[i])]], 
                                 ecoregion[[names(sar_maps[i])]], 
                                 europe_shape = europe_land_shp, 
                                 layer = layers[j],
                                 crs = CRS_LAEA_EUROPE) +
          ggtitle(paste0("Swept Area Ratio ", paste(2019, 2022, sep = "-")))
        
        
        ggsave(
          filename = name_of_file,
          plot = p,
          width = 4961 / 300,   
          height = 3508 / 300,
          units = "in",
          dpi = 300
        )
        
      },
      error = function(e) {
        warning(sprintf("Skipping %s due to error: %s", name_of_file, conditionMessage(e)))
        NULL
      }
    )
    
  }
}
