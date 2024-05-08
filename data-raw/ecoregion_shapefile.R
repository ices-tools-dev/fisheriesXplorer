## code to prepare `ecoregion_shapefile` dataset goes here


eco_shape <- sf::st_read("data-raw",
                         layer = "shape_eco_simplified")

eco_shape <- dplyr::filter(eco_shape, Ecoregion %in% c("Azores", "Baltic Sea", "Barents Sea", "Bay of Biscay and the Iberian Coast", "Celtic Seas", "Faroes", "Greater North Sea", "Greenland Sea", "Icelandic Waters", "Norwegian Sea", "Oceanic Northeast Atlantic"))
# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
eco_shape$uid <- paste0("P", 1:11)

usethis::use_data(eco_shape, overwrite = TRUE)

