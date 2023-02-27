#' @title Landuse names
#'
#' @description
#' This function takes a vector of landuse names and adds it to the problem in problemaker
#'
#' @param landuses a vector with the names of the landuses
#' @return A character vector to be used in troblemaker
#'
#' @export
#'
#' @examples
#'
#' landuse_names(landuses =  c("Agriculture", "Forest", "Urban"))
#'
#' @author Derek Corcoran
landuse_names <- function(landuses = NULL){
  Result <- paste(c("set Landuses :=", landuses, ";"), collapse = " ")
  return(Result)
}
