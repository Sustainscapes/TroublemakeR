#' @title Calculate species suitability for each landuse
#' @description Calculate species suitability from a given raster, species names and landuse and writes them to a .dat file. The file
#' will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#' @param Rasterspecieslanduse a list of species suitability for each landuse
#' @param species_names character vector of species names
#' @param landuses character vector with all landuses
#' @param parameter The name of the parameter to use
#' @param name The name of the output file
#' @param verbose Logical whether messages will be written while the
#' function is generating calculations, defaults to FALSE
#' @return .dat file. This function is used for the side-effect of writing values to a file.
#' @export
#'
#' @examples
#' library(terra)
#' data("Species_Landuse")
#' Species_Landuse <- Species_Landuse |> purrr::map(terra::unwrap)
#' species_suitability_landuse(Rasterspecieslanduse = Species_Landuse,
#' species_names = c("Spp1", "Spp2", "Spp3", "Spp4"),
#' landuses = c("Agriculture", "Forest", "Urban"), name = "Test")
#' file.remove("Test.dat")
#'
#' @importFrom purrr reduce map set_names
#'
#' @importFrom terra as.data.frame
#

species_suitability_landuse <- function(Rasterspecieslanduse, species_names, landuses, parameter = "SpeciesSuitabilityLanduse", name = "Problem", verbose = FALSE){

  if(file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    cat(paste("param", parameter, "default 0 :="))
    sink()
  }
  if(!file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = F)
    cat(paste("param", parameter, "default 0 :="))
    sink()
  }

  for(i in 1:length(Rasterspecieslanduse)){
    #dir.create(paste0(getwd(), "/Temp"))
    #terraOptions(tempdir = paste0(getwd(), "/Temp"))
    SuitabilityLanduseTemp <- Rasterspecieslanduse[[i]] |>
      as.data.frame(cells = T)
    Result <-  landuses |> purrr::map(~paste_suitabilities_landuse(df = SuitabilityLanduseTemp, colname = .x, species = species_names[i])) |> purrr::reduce(c)
    sink(paste0(name, ".dat"), append = T)
    cat(gsub(Result, pattern = "\\[", replacement = "\n ["))
    sink()
    #unlink(paste0(getwd(), "/Temp"), recursive = T, force = T)
    rm(Result)
    rm(SuitabilityLanduseTemp)
    gc()
    if(verbose){
      message(paste("species", i, "of", length(Rasterspecieslanduse), "ready!", Sys.time()))
    }
  }

  sink(paste0(name, ".dat"), append = T)
  cat(" ; \n")
  sink()
}

paste_suitabilities_landuse <- function(df, species, colname){
  filtered_df <- df[df[[colname]] >= 0, ]
  paste0(paste0("[", species, ",", colname, ","), paste0(filtered_df$cell, "]", " ", as.vector(filtered_df[colname][,1])))
}


