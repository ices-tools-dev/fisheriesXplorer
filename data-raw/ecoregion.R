## code to prepare `ecoregion` dataset goes here
library(icesVMS)
library(icesFO)
library(purrr)
library(rmapshaper)
library(sf)

ecoregions <- sort(get_ecoregion_list())
ecoregions <- ecoregions[c(4:7,9:13, 15,16)]
crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


ecoregion <- map(ecoregions, ~ load_ecoregion(., precision = 3))
ecoregion <- map(ecoregion, ~ {st_geometry(ms_simplify(.,keep = 0.03, keep_shapes = T)) %>%
  st_transform(crs = crs)})
names(ecoregion) <- ecoregions

usethis::use_data(ecoregion, overwrite = TRUE)
