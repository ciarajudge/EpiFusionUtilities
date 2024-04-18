#' Load results from EpiFusion folder into an R object
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included.
#'
#' @param folderpath filepath to folder
#' @return a list of R compatible EpiFusion output objects
#' @importFrom utils read.table
#' @export


load_likelihoods <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "likelihoods")
  likelihoods <- list()
  for (i in 1:length(filepaths)) {
    likelihoods[[paste0("Chain", as.character(i))]] <- utils::read.table(paste0(folderpath, filepaths[i]), header = F)[,1]
  }
  return(likelihoods)
}
