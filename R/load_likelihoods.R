#' Load likelihood trace from each chain of an EpiFusion analysis
#'
#' This function loads the likelihood traces of each chain of an EpiFusion analysis into a list of vectors, with one vector for each chain
#'
#' @param folderpath filepath to folder
#' @return a list of numeric vectors of likelihoods from an EpiFusion analysis
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
