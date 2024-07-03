## code to prepare `ecoregion` dataset goes here
library(icesVMS)
library(icesFO)
library(purrr)
library(rmapshaper)
library(sf)

ecoregions <- sort(get_ecoregion_list())
ecoregions <- ecoregions[c(4:7,9:13, 15,16)]


####### Get effort maps ############## Get effort maps ############## Get effort maps #######

ecoregion <- map(ecoregions, ~ load_ecoregion(., precision = 3))
ecoregion <- map(ecoregion, ~ st_geometry(ms_simplify(.,keep = 0.03, keep_shapes = T)))
names(ecoregion) <- ecoregions

usethis::use_data(ecoregion, overwrite = TRUE)
