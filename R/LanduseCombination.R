#' @title Calculate contiguity bonus based on similarity
#' @description Calculate contiguity bonuses based on landuse similarity and writes them to a .dat file. The file
#' will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#' @param Landuses is a character vector of the land-uses names
#' @param Filters Commonalities to look for in land-uses
#' @param parameter The name of the parameter to use
#' @param name The name of the output file
#' @param verbose Logical whether messages will be written while the
#' function is generating calculations, defaults to FALSE
#' @return .dat file. This function is used for the side-effect of writing values to a file.
#' @export
#'
#' @examples
#'
#'
#'
#' LanduseCombination(Landuses = c("ForestDryPoor",
#' "ForestDryRich", "ForestWetPoor",
#' "ForestWetRich", "OpenDryPoor", "OpenDryRich", "OpenWetPoor",
#' "OpenWetRich"), Filters = c("Dry", "Wet"))
#'
#'file.remove("Problem.dat")
#'
#' @importFrom dplyr all_of c_across filter mutate mutate_if rowwise select
#' @importFrom stringr str_detect
#'
#'

LanduseCombination <- function(Landuses, Filters, parameter = "ContiguityBonus", name = "Problem", verbose = FALSE) {
  Result <- combn(Landuses, m = 2, simplify = TRUE) |>
    as.data.frame() |>
    t() |>
    as.data.frame()

  colnames(Result) <- c("L1", "L2")

  Result2 <- data.frame(L1 = Landuses,
                        L2 = Landuses)

  Result <- rbind(Result, Result2)

  for(filter in Filters) {
    Result <- Result |>
      dplyr::rowwise() |>
      dplyr::mutate(!!filter := sum(stringr::str_detect(dplyr::c_across(dplyr::all_of(c("L1", "L2"))), filter)))
  }

  Result <- Result |>
    dplyr::mutate_if(is.numeric, ~floor(.x/2)) |>
    rowwise() |>
    dplyr::mutate(Total = sum(dplyr::c_across(dplyr::all_of(Filters)))) |>
    dplyr::select(L1, L2, Total) |>
    dplyr::filter(Total != 0)

  parameter

  result <- paste(paste0(" [", Result$L1, ","), paste0(Result$L2, "]"), Result$Total, collapse = "")

  Result <- paste(paste("param", parameter, "default 0 :=", result,  ";"), collapse = " ")

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
  if(verbose){
    message("Landuse bonus ready")
  }
  rm(Result)
  gc()
}
