## code to prepare `Current` dataset goes here

library(terra)
library(tidyverse)
library(magrittr)

Files <- list.files(path = "data-raw/", pattern = ".tif", full.names = T)

Files <- Files[str_detect(Files, "aux.xml", negate = T)]

Species <- Files |>
  purrr::map(terra::rast)


Current <- Species |>
  purrr::map(~.x[[1]]) |>
  purrr::reduce(c) |>
  as.numeric() |>
  magrittr::set_names(c("Spp1", "Spp2", "Spp3", "Spp4")) |>
  terra::wrap()

usethis::use_data(Current, overwrite = TRUE)
