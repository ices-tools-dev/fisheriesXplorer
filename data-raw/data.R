library(icesTAF)
library(sf)
library(leaflet)
library(icesVMS)

# mkdir("data/VMS")

# ecoregions <- c("Baltic Sea", "Bay of Biscay and the Iberian Coast", "Celtic Seas", "Greater North Sea",  "Norwegian Sea", "Icelandic Waters", "Barents Sea", "Greenland Sea", "Faroes", "Oceanic Northeast Atlantic", "Azores")


# # Get the VMS effort for all ecoregions and save each file as a rda file in data/VMS
# for (ecoregion in ecoregions) {
#     vms_effort <- icesVMS::get_effort_map(ecoregion, year = 2022)
#     vms_sar <- icesVMS::get_sar_map(ecoregion, year = 2022)
#     # convert to sf
#     vms_effort$wkt <- sf::st_as_sfc(vms_effort$wkt)
#     vms_effort <- sf::st_sf(vms_effort, sf_column_name = "wkt", crs = 4326)
#     # convert to sf
#     vms_sar$wkt <- sf::st_as_sfc(vms_sar$wkt)
#     vms_sar <- sf::st_sf(vms_sar, sf_column_name = "wkt", crs = 4326)

#     saveRDS(vms_effort, paste0("data/VMS/vms_effort_", ecoregion, ".rda"))
#     saveRDS(vms_sar, paste0("data/VMS/vms_sar_", ecoregion, ".rda"))
# }

ecoregions <- c(
  "Baltic Sea", "Bay of Biscay and the Iberian Coast", "Celtic Seas",
  "Greater North Sea", "Norwegian Sea", "Icelandic Waters", "Barents Sea",
  "Greenland Sea", "Faroes", "Oceanic Northeast Atlantic", "Azores"
)

# Create empty lists to store the sf objects
effort_list <- list()
sar_list <- list()

# Loop through ecoregions and populate the lists
for (ecoregion in ecoregions) {
  vms_effort <- icesVMS::get_effort_map(ecoregion, year = 2022)
  vms_sar <- icesVMS::get_sar_map(ecoregion, year = 2022)

  # Convert to sf
  vms_effort$wkt <- sf::st_as_sfc(vms_effort$wkt)
  vms_effort <- sf::st_sf(vms_effort, sf_column_name = "wkt", crs = 4326)

  vms_sar$wkt <- sf::st_as_sfc(vms_sar$wkt)
  vms_sar <- sf::st_sf(vms_sar, sf_column_name = "wkt", crs = 4326)

  # Add to lists with ecoregion name as the key
  effort_list[[ecoregion]] <- vms_effort
  sar_list[[ecoregion]] <- vms_sar
}

save(effort_list, file = "./data/vms_effort_all.rda")
save(sar_list, file = "./data/vms_sar_all.rda")

