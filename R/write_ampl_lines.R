#' @title Create budget
#'
#' @description
#' This function generates or appends the budget and transition cost to
#' a .dat file for ampl
#'
#' @param line line to be written to .dat file
#' @param name The name of the output file
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
