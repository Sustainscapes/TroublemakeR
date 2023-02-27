#' @title Species names
#'
#' @description
#' This function takes a vector of species names and adds it to the problem in problemaker
#'
#' @param species_names a vector with the names of species
#' @return A character vector to be used in troblemaker
#'
#' @export
#'
#' @examples
#'
#' species_names(species_names = c("Spp1", "Spp2"))
#'
#' @author Derek Corcoran
species_names <- function(species_names = NULL){
  Result <- paste(c("set Species :=", species_names, ";"), collapse = " ")
  return(Result)
}
