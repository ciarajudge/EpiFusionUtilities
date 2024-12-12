#' Load infection trajectories from EpiFusion folder into a list of dataframes
#'
#' This function loads the infection trajectories of an EpiFusion analysis into a list of dataframes, with one dataframe for each chain in the analysis. The data frames are structured with ncol = number of days in the analysis, and nrow = number of MCMC samples.
#'
#' @param folderpath filepath to folder
#' @return a list of infection trajectory dataframes
#' @importFrom utils read.csv
#' @export
#'

load_infection_trajectories <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "trajectories")
  trajectories <- list()
  for (f in 1:length(filepaths)) {
    traj <- utils::read.csv(paste0(folderpath, filepaths[f]), header = T)
    traj <- traj[,1:(ncol(traj)-1)]
    traj[traj < 0] <- 0
    trajectories[[paste0('Chain', f)]] <- traj
  }
  return(trajectories)
}
