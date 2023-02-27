## code to prepare `Species` dataset goes here

library(terra)
library(tidyverse)

Files <- list.files(path = "data-raw/", pattern = ".tif", full.names = T)

Files <- Files[str_detect(Files, "aux.xml", negate = T)]

Species <- Files |>
  purrr::map(terra::rast) |>
  purrr::map(terra::wrap)

usethis::use_data(Species, overwrite = TRUE)

