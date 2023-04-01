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
#' @param budget maximum cost for the problem
#' @param Rastercurrentlanduse raster object of current landuses
#' @param name The name of the output file
#' @param verbose Logical whether messages will be written while the
#' function is generating calculations, defaults to FALSE
#' @return A .dat file with the spatial problem formated for AMPL. This function is used for the side-effect of writing values to a file.
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
#'  # Example 3 with budget and transition cost
#'
#'  data("CurrentLanduse")
#'  CurrentLU <- terra::unwrap(CurrentLanduse)
#'  TroublemakeR::troublemaker(Rasterdomain =Test[[1]],
#'  Rasterspecieslanduse = Species_Landuse,
#'  species_names = c("Spp1", "Spp2", "Spp3", "Spp4"),
#'  landuses = c("Agriculture", "Forest", "Urban"),
#'  Rastercurrentlanduse = CurrentLU,
#'  budget = 2,
#'  name = "Problem3",
#'  verbose = FALSE)
#'
#'  file.remove("Problem3.dat")
#'
#' @author Derek Corcoran

troublemaker <- function(Rasterdomain = NULL, Rastercurrent = NULL, species_names = NULL, Rasterspecieslanduse = NULL, landuses = NULL,
                         budget = NULL,
                         Rastercurrentlanduse = NULL,
                         name = "Problem",
                         verbose = FALSE){
  if(!is.null(Rasterdomain)){
    TroublemakeR::define_cells(Rasterdomain = Rasterdomain, name = name)
    if(verbose){
      message("TempDomain ready")
    }
  }
  if(!is.null(species_names)){
    TroublemakeR::species_names(species_names = species_names, name = name)
  }
  if(!is.null(landuses)){
    TroublemakeR::landuse_names(landuses = landuses)
    if(verbose){
      message("TempLanduses ready")
    }
  }
  if(!is.null(species_names) & !is.null(Rastercurrent)){
    TroublemakeR::species_suitability(Rastercurrent = Rastercurrent, species_names = species_names, name = name)
    if(verbose){
      message("TempSpeciesSuitability ready")
    }
  }
  if(!is.null(species_names) & !is.null(Rasterspecieslanduse) & !is.null(landuses)){
    TroublemakeR::species_suitability_landuse(Rasterspecieslanduse =  Rasterspecieslanduse, species_names = species_names, landuses = landuses,name = name, verbose = verbose)
    if(verbose){
      message("TempSpeciesSuitabilityLanduse ready")
    }
    gc()
  }

  if(!is.null(budget) & !is.null(Rastercurrentlanduse) & !is.null(landuses)){
    TroublemakeR::create_budget(budget, Rastercurrentlanduse, landuses, name = name, verbose = verbose)
    if(verbose){
      message("budget and transition cost ready")
    }
    gc()
  }
}

