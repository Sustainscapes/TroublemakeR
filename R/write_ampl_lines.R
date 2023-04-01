#' @title Writes an AMPL line
#'
#' @description
#' This function takes a character and writes them to a .dat file. The file
#' will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#'
#' @param line line to be written to .dat file
#' @param name The name of the output file
#'
#' @return .dat file. This function is used for the side-effect of writing values to a file.
#'
#' @examples
#'
#' write_ampl_lines("param s:= 1")
#'
#' file.remove("Problem.dat")
#'
#' @export



write_ampl_lines <- function(line, name = "Problem"){
  if(file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    cat(paste(line, ";", "\n"))
    sink()
  }
  if(!file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = F)
    cat(paste(line, ";", "\n"))
    sink()
  }

}
