#' Load results from EpiFusion folder into an R object
#'
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included.
#'
#' @param folderpath filepath to folder
#' @return a list of R compatible EpiFusion output objects
#' @importFrom utils read.csv
#' @export
#'

load_fitted_epi_cases <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "positivetests")
  if (length(filepaths) == 0) {
    return(NA)
  } else {
    cases <- list()
    for (f in 1:length(filepaths)) {
      traj <- utils::read.csv(paste0(folderpath, filepaths[f]), header = T)
      traj <- traj[,1:(ncol(traj)-1)]
      cases[[paste0('Chain', f)]] <- traj
    }
  }
  return(cases)
}
