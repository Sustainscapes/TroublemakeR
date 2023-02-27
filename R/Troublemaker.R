#' @title Troublemaker
#'
#' @description
#' This function is a metafunction with several functions inside of it it takes several spatial objects and generates a .dat file with a spatial dataset for AMPL
#'
#' @param Rasterdomain A Raster object with any value in the cells that are part of the problem and NA values where the problem is not to be solved
#' @param Rastercurrent raster object of current suitability
#' @param Rasterspecieslanduse a list of species suitability for each landuse
#' @param species_names a vector with the names of species
#' @param landuses character vector with all landuses
#' @param name The name of the output file
#' @param verbose Logical whether messages will be written while the
#' function is generating calculations, defaults to FALSE
#' @return A .dat file with the spatial problem formated for AMPL
#'
#' @export
#'
#' @examples
#' # Example 1 with current suitabilities
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
#'
#' # Example 2 with landuse suitabilities
#'
#' data(Species)
#' data("Species_Landuse")
#'
#' library(terra)
#' Test <- Species[[1]] |>
#' terra::unwrap()
#'
#' Species_Landuse <- Species_Landuse |> purrr::map(terra::unwrap)
#'
#' # Generate the "Problem2.dat" file
#'
#' TroublemakeR::troublemaker(Rasterdomain =Test[[1]],
#' Rasterspecieslanduse = Species_Landuse,
#' species_names = c("Spp1", "Spp2", "Spp3", "Spp4"),
#' landuses = c("Agriculture", "Forest", "Urban"),
#' name = "Problem2")
#'
#' # delete the file so the test on cran can pass this
#'
#' file.remove("Problem2.dat")
#'
#' @author Derek Corcoran

troublemaker <- function(Rasterdomain = NULL, Rastercurrent = NULL, species_names = NULL, Rasterspecieslanduse = NULL, landuses = NULL,
                         name = "Problem", verbose = FALSE){
  if(!is.null(Rasterdomain)){
    TempDomain <-  TroublemakeR::define_cells(Rasterdomain = Rasterdomain)
    if(verbose){
      message("TempDomain ready")
    }
  }
  if(!is.null(species_names)){
    TempSpeciesNames <-  TroublemakeR::species_names(species_names = species_names)
    if(verbose){
      message("TempSpeciesNames ready")
    }
  }
  if(!is.null(species_names) & !is.null(Rastercurrent)){
    TempSpeciesSuitability <-  TroublemakeR::species_suitability(Rastercurrent = Rastercurrent, species_names = species_names)
    if(verbose){
      message("TempSpeciesSuitability ready")
    }
  }
  if(!is.null(species_names) & !is.null(Rasterspecieslanduse) & !is.null(landuses)){
    TempSpeciesSuitabilityLanduse <-  TroublemakeR::species_suitability_landuse(Rasterspecieslanduse =  Rasterspecieslanduse, species_names = species_names, landuses = landuses)
    if(verbose){
      message("TempSpeciesSuitabilityLanduse ready")
    }
  }
  if(verbose){
    message("Starting to write")
  }

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
  if(!is.null(species_names) & !is.null(Rasterspecieslanduse) & !is.null(landuses)){
    cat(TempSpeciesSuitabilityLanduse)
    cat("\n")
  }
  sink()
}

