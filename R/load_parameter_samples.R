#' Load parameter samples from EpiFusion analysis
#'
#' This function loads the parameter samples from an EpiFusion analysis into an R list, with one data frame per chain, with one column per parameter and one row per MCMC sample
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
