#' @title Calculate species suitability
#' @description Calculate species suitability from a given raster and species names and writes them to a .dat file. The file
#' will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#' @param Rastercurrent raster object of current suitability
#' @param species_names character vector of species names
#' @param name The name of the output file
#' @param verbose Logical whether messages will be written while the
#' function is generating calculations, defaults to FALSE
#' @return .dat file. This function is used for the side-effect of writing values to a file.
#' @export
#'
#' @examples
#' library(terra)
#' data(Current)
#' Current <- terra::unwrap(Current)
#' species_suitability(Rastercurrent = Current, species_names = c("Spp1", "Spp2", "Spp3", "Spp4"))
#'
#'file.remove("Problem.dat")
#' @importFrom purrr reduce map
#'
#' @importFrom terra as.data.frame
#'
species_suitability <- function(Rastercurrent, species_names, name = "Problem", verbose = FALSE){
  SuitabilityTemp <- terra::as.data.frame(Rastercurrent, cells = T)
  colnames(SuitabilityTemp)[-1] <- species_names
  result <- species_names |> purrr::map(~paste_suitabilities(df = SuitabilityTemp, colname = .x)) |> purrr::reduce(paste) |> paste(collapse = " ")
  TempSpeciesNames <- paste(c("param SpeciesSuitability default 0 :=", result,  ";"), collapse = " ")
  if(file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    cat(TempSpeciesNames)
    cat("\n")
    sink()
  }
  if(!file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = F)
    cat(TempSpeciesNames)
    cat("\n")
    sink()
  }
  if(verbose){
    message("TempSpeciesNames ready")
  }
  rm(TempSpeciesNames)
  gc()
}


paste_suitabilities <- function(df, colname){
  filtered_df <- df[df[[colname]] == 1, ]
  paste0(paste0("[", colname, ","), paste0(filtered_df$cell, "]", " ", as.vector(filtered_df[colname][,1])))
}



