#' @title Define Cells
#'
#' @description
#' This function takes a Raster object and creates a character vector made for the troublemaker function
#'
#' @param Rasterdomain A Raster object with any value in the cells that are part of the problem and NA values where the problem is not to be solved
#' @importFrom terra as.data.frame
#'
#' @return A character vector to be used in troblemaker
#'
#' @export
#'
#' @examples
#' data(Species)
#' library(terra)
#' Test <- Species[[1]] |>
#' terra::unwrap()
#'
#' # Generate the "Problem.dat" file
#'
#' define_cells(Test[[1]])
#'
#' @author Derek Corcoran
define_cells <- function(Rasterdomain){
  Temp <- terra::as.data.frame(Rasterdomain, cells = T, xy = T)
  Result <- paste0(paste(c("set Cells :=", Temp$cell, ";"), collapse = " "), "\n")
  return(Result)
}
