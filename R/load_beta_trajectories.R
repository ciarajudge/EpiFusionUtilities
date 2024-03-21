#' Load results from EpiFusion folder into an R object
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included.
#'
#' @param folderpath filepath to folder
#' @return a list of R compatible EpiFusion output objects
#' @export
#'

load_beta_trajectories <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "beta")
  betas <- list()
  for (f in 1:length(filepaths)) {
    beta <- read.csv(paste0(folderpath, filepaths[f]), header = F)
    beta <- beta[,1:(ncol(beta)-1)]
    betas[[paste0('Chain', f)]] <- beta
  }
  return(betas)
}
