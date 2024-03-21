#' Load results from EpiFusion folder into an R object
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included.
#'
#' @param folderpath filepath to folder
#' @return a list of R compatible EpiFusion output objects
#' @export
#'

load_rt_trajectories <- function(folderpath) {
  betafilepaths <- list.files(folderpath, pattern = "betas")
  gammasfilepaths <- list.files(folderpath, pattern = "param")
  rts <- list()
  for (f in 1:length(betafilepaths)) {
    betas <- read.csv(paste0(folderpath, betafilepaths[f]), header = F)
    betas <- betas[,1:(ncol(betas)-1)]
    gammas <- read.csv(paste0(folderpath, gammasfilepaths[f]), header = T)$gamma
    rt <- betas/gammas
    rts[[paste0('Chain',f)]] <- rt
  }
  return(rts)
}
