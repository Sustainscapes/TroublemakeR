#' @title Calculate species suitability
#' @description Calculate species suitability from a given raster and species names
#' @param Rastercurrent raster object of current suitability
#' @param species_names character vector of species names
#' @return character string of species suitabilities
#' @export
#'
#' @examples
#' library(terra)
#' data(Current)
#' Current <- terra::unwrap(Current)
#' species_suitability(Rastercurrent = Current, species_names = c("Spp1", "Spp2", "Spp3", "Spp4"))
#'
#' @importFrom purrr reduce map
#'
#' @importFrom terra as.data.frame
#'
species_suitability <- function(Rastercurrent, species_names){
  SuitabilityTemp <- terra::as.data.frame(Rastercurrent, cells = T)
  colnames(SuitabilityTemp)[-1] <- species_names
  result <- species_names |> purrr::map(~paste_suitabilities(df = SuitabilityTemp, colname = .x)) |> purrr::reduce(paste) |> paste(collapse = " ")
  paste(c("param SpeciesSuitability :=", result), collapse = " ")
}


paste_suitabilities <- function(df, colname){
  paste0(paste0("[", colname, ","), paste0(df$cell, "]", " ", as.vector(df[colname][,1])))
}



