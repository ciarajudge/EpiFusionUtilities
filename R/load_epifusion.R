#' Load results from EpiFusion folder into an R object
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included.
#' 
#' @param folderpath filepath to folder
#' @return a list of R compatible EpiFusion output objects
#' @export

load_epifusion <- function(folderpath) {
  filepaths <- list.files(folderpath)
  num_chains <- length('likelihoods' %in% filepaths)
  return(list(num_chains = num_chains))
}
