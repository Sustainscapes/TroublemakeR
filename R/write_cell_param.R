#' @title Write cell parameters
#'
#' @description
#' This function takes a Raster object and creates a character vector made for the troublemaker function
#'
#' @param Rasterparam A Raster object with the values for the parameter
#' @param parameter The name of the parameter to use
#' @param default The value of the default value for the parameter if
#' there is one, otherwise keep it as NULL
#' @param name The name of the output file
#' @param verbose Logical whether messages will be written while the
#' function is generating calculations, defaults to FALSE
#' @importFrom terra as.data.frame
#'
#' @return A character vector to be used in troblemaker
#'
#' @export
#'
#' @examples
#'
#' library(terra)
#'
#' A <- TroublemakeR::Current |> terra::unwrap()
#' A <- A[[1]]
#'
#' write_cell_param(Rasterparam = A, parameter = "Suitability", name = "Problem")
#'
#'  write_cell_param(Rasterparam = A, parameter = "Carbon", default = 1,
#'  name = "Problem")
#'
#'  write_cell_param(Rasterparam = A, parameter = "Cost", default = 0,
#'  name = "Problem")
#'
#'  file.remove("Problem.dat")


write_cell_param <- function(Rasterparam,
                         parameter,
                         default = NULL,
                         name = "Problem",
                         verbose = FALSE){
  Temp <- terra::as.data.frame(Rasterparam, cells = T)
  colnames(Temp)[2] <- "Param"

  if(file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    if(is.null(default)){
      cat(paste(c("param", parameter ,":= \n")))
    }
    if(!is.null(default)){
      cat(paste(c("param", parameter, "default", default ,":= \n")))
    }
    cat(paste_params(df = Temp, default = default))
    cat("; \n")
    sink()
  }

  if(!file.exists(paste0(name, ".dat"))){
    sink(paste0(name, ".dat"), append = T)
    if(is.null(default)){
      cat(paste(c("param", parameter ,":= \n")))
    }
    if(!is.null(default)){
      cat(paste(c("param", parameter, "default", default ,":= \n")))
    }
    cat(paste_params(df = Temp, default = default))
    cat("; \n")
    sink()
  }
}


paste_params <- function(df, default){
  if(!is.null(default)){
    df <- df[df[["Param"]] != default, ]
  }

  result <- paste0("[", df$cell, "] ", as.vector(df$Param), "\n")

  last_row <- nrow(df)
  result[last_row] <- gsub("\n", "", result[last_row])

  return(result)
}

