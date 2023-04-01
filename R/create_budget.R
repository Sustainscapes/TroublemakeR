#' @title Create budget
#'
#' @description
#' This function generates or appends the budget and transition cost to
#' a .dat file for ampl. The file
#' will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#'
#' @param budget maximum cost for the problem
#' @param Rastercurrentlanduse raster object of current landuses
#' @param landuses character vector with all landuses
#' @param name The name of the output file
#' @param verbose Logical whether messages will be written while the
#' function is generating calculations, defaults to FALSE
#' @return A .dat file. This function is used for the side-effect of writing values to a file.
#'
#' @export
#'
#' @examples
#'
#' data(CurrentLanduse)
#' CurrentLU <- terra::unwrap(CurrentLanduse)
#'
#'
#' TroublemakeR::create_budget(budget = 2,
#' Rastercurrentlanduse = CurrentLU,
#' landuses = c("Agriculture", "Forest", "Urban"),
#' name = "Problem",
#' verbose = TRUE)
#'
#' # delete the file so the test on cran can pass this
#'
#' file.remove("Problem.dat")
#'
#' @author Derek Corcoran

create_budget <- function(budget, Rastercurrentlanduse, landuses, name = "Problem", verbose = FALSE){
  if(file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    cat(paste("param b :=", budget, ";", "\n"))
    sink()
  }
  if(!file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = F)
    cat(paste("param b :=", budget, ";", "\n"))
    sink()
  }

  sink(paste0(name, ".dat"), append = T)
  cat(paste("param TransitionCost default 1 :="))
  sink()

  for(i in 1:length(landuses)){
    Template <- as.numeric(Rastercurrentlanduse)
    Template[!is.na(Template)] <- NA
    Template[Rastercurrentlanduse == landuses[i]] <- 0
    Template <- as.data.frame(Template, cells = T)
    Template <- paste0(paste0("[", Template$cell, ","), paste0(landuses[i], "]", " ", as.vector(Template$Sutiability)))
    sink(paste0(name, ".dat"), append = T)
    cat(gsub(Template, pattern = "\\[", replacement = "\n ["))
    sink()
    rm(Template)
    gc()
    if(verbose){
      message(paste("Landuse", i, "of", length(landuses), "ready", Sys.time()))
    }
  }
  sink(paste0(name, ".dat"), append = T)
  cat(" ;")
  sink()
}
