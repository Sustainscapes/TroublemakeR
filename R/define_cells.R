#' @title Define Cells
#'
#' @description
#' This function takes a Raster object and identifies non NA cells and writes them to a .dat file. The file
#' will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#'
#' @param Rasterdomain A Raster object with any value in the cells that are part of the problem and NA values where the problem is not to be solved
#' @param name The name of the output file
#' @importFrom terra as.data.frame
#'
#' @return .dat file. This function is used for the side-effect of writing values to a file.
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
#' file.remove("Problem.dat")
#'
#' @author Derek Corcoran
define_cells <- function(Rasterdomain, name = "Problem"){
  Temp <- terra::as.data.frame(Rasterdomain, cells = T, xy = T)
  Result <- paste0(paste(c("set Cells :=", Temp$cell, ";"), collapse = " "), "\n")
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
