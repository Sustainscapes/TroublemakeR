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
    if(file.exists(paste0(name, ".dat"))){
      sink(paste0(name, ".dat"), append = T)
      cat(TempDomain)
      cat("\n")
      sink()
    }
    if(!file.exists(paste0(name, ".dat"))){
      sink(paste0(name, ".dat"), append = F)
      cat(TempDomain)
      cat("\n")
      sink()
    }

    if(verbose){
      message("TempDomain ready")
    }
    rm(TempDomain)
    gc()
  }
  if(!is.null(species_names)){
    TempSpeciesNames <-  TroublemakeR::species_names(species_names = species_names)
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
  if(!is.null(landuses)){
    TempLanduses <-  TroublemakeR::landuse_names(landuses = landuses)
    if(file.exists(paste0(name, ".dat"))){
      sink(paste0(name, ".dat"), append = T)
      cat(TempLanduses)
      cat("\n")
      sink()
    }
    if(!file.exists(paste0(name, ".dat"))){
      sink(paste0(name, ".dat"), append = F)
      cat(TempLanduses)
      cat("\n")
      sink()
    }
    if(verbose){
      message("TempLanduses ready")
    }
    rm(TempLanduses)
    gc()
  }
  if(!is.null(species_names) & !is.null(Rastercurrent)){
    TempSpeciesSuitability <-  TroublemakeR::species_suitability(Rastercurrent = Rastercurrent, species_names = species_names)
    if(file.exists(paste0(name, ".dat"))){
      sink(paste0(name, ".dat"), append = T)
      cat(TempSpeciesSuitability)
      cat("\n")
      sink()
    }
    if(!file.exists(paste0(name, ".dat"))){
      sink(paste0(name, ".dat"), append = F)
      cat(TempSpeciesSuitability)
      cat("\n")
      sink()
    }
    if(verbose){
      message("TempSpeciesSuitability ready")
    }
    rm(TempSpeciesSuitability)
    gc()
  }
  if(!is.null(species_names) & !is.null(Rasterspecieslanduse) & !is.null(landuses)){
    TroublemakeR::species_suitability_landuse(Rasterspecieslanduse =  Rasterspecieslanduse, species_names = species_names, landuses = landuses,name = name)
    if(verbose){
      message("TempSpeciesSuitabilityLanduse ready")
    }
    gc()
  }
}

