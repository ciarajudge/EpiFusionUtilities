#' Load acceptance rates from EpiFusion folder into an R object
#' This function loads the acceptance rates for each chain of an EpiFusion analysis into a list of vectors.
#'
#' @param folderpath filepath to folder
#' @return a list of vectors of acceptance rates of each chain
#' @importFrom utils read.table
#' @export
#'

load_acceptance <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "acceptance")
  acceptance <- list()
  for (i in 1:length(filepaths)) {
    acceptance[[paste0("Chain", as.character(i))]] <- utils::read.table(paste0(folderpath, filepaths[i]), header = F)[,1]
  }
  return(acceptance)
}
