#' @title Troublemaker
#'
#' @description
#' This function is a metafunction with several functions inside of it it takes several spatial objects and generates a .dat file with a spatial dataset for AMPL
#'
#' @param Rasterdomain A Raster object with any value in the cells that are part of the problem and NA values where the problem is not to be solved
#' @param Rastercurrent raster object of current suitability
#' @param species_names a vector with the names of species
#' @param name The name of the output file
#' @return A .dat file with the spatial problem formated for AMPL
#'
#' @export
#'
#' @examples
#' data(Species)
#' data(Current)
#' library(terra)
#' Test <- Species[[1]] |>
#' terra::unwrap()
#'
#' Current <- terra::unwrap(Current)
#'
#' # Generate the "Problem.dat" file
#'
#' TroublemakeR::troublemaker(Rasterdomain =Test[[1]],
#' Rastercurrent = Current,
#' species_names = c("Spp1", "Spp2", "Spp3", "Spp4"),
#' name = "Problem")
#'
#' # delete the file so the test on cran can pass this
#'
#' file.remove("Problem.dat")
#' @author Derek Corcoran

troublemaker <- function(Rasterdomain = NULL, Rastercurrent = NULL, species_names = NULL, name = "Problem"){
  if(!is.null(Rasterdomain)){
    TempDomain <-  TroublemakeR::define_cells(Rasterdomain = Rasterdomain)
    message("TempDomain ready")
  }
  if(!is.null(species_names)){
    TempSpeciesNames <-  TroublemakeR::species_names(species_names = species_names)
    message("TempSpeciesNames ready")
  }
  if(!is.null(species_names) & !is.null(Rastercurrent)){
    TempSpeciesSuitability <-  TroublemakeR::species_suitability(Rastercurrent = Rastercurrent, species_names = species_names)
    message("TempSpeciesSuitability ready")
  }
  message("Starting to write")
  sink(paste0(name, ".dat"))
  if(!is.null(Rasterdomain)){
    cat(TempDomain)
    cat("\n")
  }
  if(!is.null(species_names)){
    cat(TempSpeciesNames)
    cat("\n")
  }
  if(!is.null(species_names) & !is.null(Rastercurrent)){
    cat(TempSpeciesSuitability)
    cat("\n")
  }
  sink()
}

