## code to prepare `Species_Landuse` dataset goes here
library(terra)
library(tidyverse)

Files <- list.files(path = "data-raw/", pattern = ".tif", full.names = T)

Files <- Files[str_detect(Files, "aux.xml", negate = T)]

Species_Landuse <- Files |>
  purrr::map(terra::rast) |>
  purrr::map(as.numeric) |>
  purrr::map(~.x[[-1]]) |>
  purrr::map(~magrittr::set_names(.x, c("Agriculture", "Forest", "Urban"))) |>
  magrittr::set_names(c("Spp1", "Spp2", "Spp3", "Spp4")) |>
  purrr::map(terra::wrap)

usethis::use_data(Species_Landuse, overwrite = TRUE)
