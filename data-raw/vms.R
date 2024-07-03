library(icesVMS)
library(icesFO)
library(icesVocab)
library(purrr)
library(dplyr)

ecoregions <- sort(get_ecoregion_list())
ecoregions <- ecoregions[c(4:7,9:13, 15,16)]


####### Get effort maps ############## Get effort maps ############## Get effort maps #######

effort_maps <- map(ecoregions, get_effort_map)
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

sar_maps <- map(ecoregions, get_sar_map)
names(sar_maps) <- ecoregions
sar_maps[["Azores"]] <- NULL


sar_maps <- map(sar_maps, ~ {if(!is.null(.)) mutate(., geometry = st_as_sfc(wkt, crs = 4326)) %>% 
                  select(-wkt) %>%
                  st_sf}) 


usethis::use_data(sar_maps, overwrite = TRUE)
