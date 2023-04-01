#' @title Species names
#'
#' @description
#' This function takes a vector of species names and writes them to a .dat file. The file
#' will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#'
#' @param species_names a vector with the names of species
#' @param name The name of the output file
#' @return  .dat file. This function is used for the side-effect of writing values to a file.
#'
#' @export
#'
#' @examples
#'
#' species_names(species_names = c("Spp1", "Spp2"))
#' file.remove("Problem.dat")
#' @author Derek Corcoran
species_names <- function(species_names = NULL, name = "Problem"){
  Result <- paste(c("set Species :=", species_names, ";"), collapse = " ")
  if(file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    cat(Result)
    cat("\n")
    sink()
  }
  if(!file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = F)
    cat(Result)
    cat("\n")
    sink()
  }
  rm(Result)
  gc()
}
