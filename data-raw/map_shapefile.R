## code to prepare `map_shapefile` dataset goes here

map_shape <- sf::st_read(dsn = "data-raw",
                         layer = "world_map_simplified")

usethis::use_data(map_shape, overwrite = TRUE)
