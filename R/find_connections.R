#' @title Find connections
#'
#' @description
#' This function takes a Raster object and identifies non NA cells and finds adjacent cells, to then store them as edges.
#' Those then will be written to the location specified by the `name` argument. If the file
#' already exists, it will be overwritten. The file format is plain text, with each
#' line terminated by a newline character.
#'
#' @param Rasterdomain A Raster object with any value in the cells that are part of the problem and NA values where the problem is not to be solved
#' @param name The name of the output file
#' @param directions character or matrix to indicated the directions in which cells are considered connected. The following character values are allowed: "rook" or "4" for the horizontal and vertical neighbors; "bishop" to get the diagonal neighbors; "queen" or "8" to get the vertical, horizontal and diagonal neighbors; or "16" for knight and one-cell queen move neighbors. If directions is a matrix it should have odd dimensions and have logical (or 0, 1) values
#' @importFrom terra adjacent ncell
#' @importFrom data.table as.data.table := setnames
#'
#' @return .dat file. This function is used for the side-effect of writing values to a file.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(terra)
#' r <- rast(nrows=500, ncols=500)
#'
#' set.seed(2023)
#' values(r) <- sample(x = 1:6, size = ncell(r),
#' replace = TRUE,
#' prob = c(0.03125, 0.0625, 0.09375, 0.15625, 0.25, 0.40625))
#'
#'##
#'set.seed(2023)
#'
#' ForNA <- sample(1:ncell(r), ceiling(ncell(r)/10))
#'
#' values(r)[ForNA] <- NA
#'
#' find_connections(Rasterdomain = r, name = "Edges")
#' file.remove("Edges.dat")
#' }
#' @author Derek Corcoran

find_connections <- function(Rasterdomain, name = "Problem", directions = "rook"){
  V1 <- V2 <- Expression <- NULL
  NAs <- as.vector(is.na(Rasterdomain))
  adj_df <- terra::adjacent(Rasterdomain, ((1:ncell(Rasterdomain))[!NAs]),
                     directions= directions,
                     pairs = TRUE,
                     symmetrical=TRUE)  |>
    data.table::as.data.table() |>
    data.table::setnames(c("V1", "V2"))

  dt_filtered <- adj_df[!(V1 %in% ((1:ncell(Rasterdomain))[NAs]) | V2 %in% ((1:ncell(Rasterdomain))[NAs]))]
  dt_filtered <- dt_filtered[, Expression := paste0("(", V1,",", V2, ")")]
  write_ampl_lines(line = paste("set E:=", paste(dt_filtered$Expression, collapse = " ")), name = name)
}

