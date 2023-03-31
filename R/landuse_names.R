#' @title Landuse names
#'
#' @description
#' This function takes a vector of landuse names and adds it to the problem in problemaker
#'
#' @param landuses a vector with the names of the landuses
#' @param name The name of the output file
#' @return A character vector to be used in troblemaker
#'
#' @export
#'
#' @examples
#'
#' landuse_names(landuses =  c("Agriculture", "Forest", "Urban"))
#'
#' # delete the file so the test on cran can pass this
#'
#' file.remove("Problem.dat")
#'
#'
#' @author Derek Corcoran
landuse_names <- function(landuses = NULL, name = "Problem"){
  Result <- paste(c("set Landuses :=", landuses, ";"), collapse = " ")
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
