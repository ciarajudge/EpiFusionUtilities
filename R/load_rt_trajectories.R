#' Load Rt trajectories from EpiFusion folder into an list of dataframes
#'
#' This function loads the Rt trajectories of an EpiFusion analysis into a list of dataframes, with one dataframe for each chain in the analysis. The data frames are structured with ncol = number of days in the analysis, and nrow = number of MCMC samples. The Rt is calculated using the sampled beta trajectory and gamma value from each MCMC step.
#'
#' @param folderpath filepath to folder
#' @return a list of Rt trajectory dataframes
#' @importFrom utils read.csv
#' @export
#'

load_rt_trajectories <- function(folderpath) {
  betafilepaths <- list.files(folderpath, pattern = "betas")
  gammasfilepaths <- list.files(folderpath, pattern = "param")
  rts <- list()
  for (f in 1:length(betafilepaths)) {
    betas <- utils::read.csv(paste0(folderpath, betafilepaths[f]), header = T)
    betas <- betas[,1:(ncol(betas)-1)]
    gammas <- utils::read.csv(paste0(folderpath, gammasfilepaths[f]), header = T)$gamma
    rt <- betas/gammas
    rts[[paste0('Chain',f)]] <- rt
  }
  return(rts)
}
