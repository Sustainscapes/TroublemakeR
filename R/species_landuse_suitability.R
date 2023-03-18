#' @title Calculate species suitability for each landuse
#' @description Calculate species suitability from a given raster, species names and landuse
#' @param Rasterspecieslanduse a list of species suitability for each landuse
#' @param species_names character vector of species names
#' @param landuses character vector with all landuses
#' @param name The name of the output file
#' @return character string of species suitabilities for each landuse
#' @export
#'
#' @examples
#' library(terra)
#' data("Species_Landuse")
#' Species_Landuse <- Species_Landuse |> purrr::map(terra::unwrap)
#' species_suitability_landuse(Rasterspecieslanduse = Species_Landuse,
#' species_names = c("Spp1", "Spp2", "Spp3", "Spp4"),
#' landuses = c("Agriculture", "Forest", "Urban"), name = "Test")
#'
#' @importFrom purrr reduce map set_names
#'
#' @importFrom terra as.data.frame
#

species_suitability_landuse <- function(Rasterspecieslanduse, species_names, landuses, name = "Problem"){

  if(file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    cat("param SpeciesSuitabilityLanduse :=")
    sink()
  }
  if(!file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = F)
    cat("param SpeciesSuitabilityLanduse :=")
    sink()
  }

  for(i in 1:length(Rasterspecieslanduse)){
    #dir.create(paste0(getwd(), "/Temp"))
    #terraOptions(tempdir = paste0(getwd(), "/Temp"))
    SuitabilityLanduseTemp <- Rasterspecieslanduse[[i]] |>
      as.data.frame(cells = T)
    Result <-  landuses |> purrr::map(~paste_suitabilities_landuse(df = SuitabilityLanduseTemp, colname = .x, species = species_names[i])) |> purrr::reduce(paste)
    sink(paste0(name, ".dat"), append = T)
    cat(gsub(Result, pattern = "\\[", replacement = "\n ["))
    sink()
    #unlink(paste0(getwd(), "/Temp"), recursive = T, force = T)
    rm(Result)
    rm(SuitabilityLanduseTemp)
    gc()
  }

  sink(paste0(name, ".dat"), append = T)
  cat(" ;")
  sink()
}

paste_suitabilities_landuse <- function(df, species, colname){
  paste0(paste0("[", species, ",", colname, ","), paste0(df$cell, "]", " ", as.vector(df[colname][,1])))
}

