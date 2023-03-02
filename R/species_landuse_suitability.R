#' @title Calculate species suitability for each landuse
#' @description Calculate species suitability from a given raster, species names and landuse
#' @param Rasterspecieslanduse a list of species suitability for each landuse
#' @param species_names character vector of species names
#' @param landuses character vector with all landuses
#' @return character string of species suitabilities for each landuse
#' @export
#'
#' @examples
#' library(terra)
#' data("Species_Landuse")
#' Species_Landuse <- Species_Landuse |> purrr::map(terra::unwrap)
#' species_suitability_landuse(Rasterspecieslanduse = Species_Landuse,
#' species_names = c("Spp1", "Spp2", "Spp3", "Spp4"),
#' landuses = c("Agriculture", "Forest", "Urban"))
#'
#' @importFrom purrr reduce map set_names
#'
#' @importFrom terra as.data.frame
#

species_suitability_landuse <- function(Rasterspecieslanduse, species_names, landuses){

  SuitabilityLanduseTemp <- Rasterspecieslanduse |>
    purrr::map(terra::as.data.frame, cells = T) |>
    purrr::set_names(species_names)
  Result <- list()

  for(i in 1:length(SuitabilityLanduseTemp)){
    Result[[i]] <-  landuses |> purrr::map(~paste_suitabilities_landuse(df = SuitabilityLanduseTemp[[i]], colname = .x, species = species_names[i])) |> purrr::reduce(paste)
  }
  Result <- Result |> purrr::reduce(c)

  Result <- paste(c("param SpeciesSuitabilityLanduse :=", Result, ";"), collapse = " ")

  gsub(Result, pattern = "\\[", replacement = "\n [")
}

paste_suitabilities_landuse <- function(df, species, colname){
  paste0(paste0("[", species, ",", colname, ","), paste0(df$cell, "]", " ", as.vector(df[colname][,1])))
}

