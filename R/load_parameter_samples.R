#' Load results from EpiFusion folder into an R object
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included.
#'
#' @param folderpath filepath to folder
#' @return a list of R compatible EpiFusion output objects
#' @importFrom utils read.csv
#' @export
#'

load_parameter_samples <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "params")
  params <- list()
  for (f in 1:length(filepaths)) {
    param <- utils::read.csv(paste0(folderpath, filepaths[f]), header = T)
    param <- param[,1:(ncol(param)-1)]
    params[[paste0('Chain', f)]] <- param
  }
  return(params)
}
