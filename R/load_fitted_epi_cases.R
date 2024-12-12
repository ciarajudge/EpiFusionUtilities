#' Load trajectories of fitted epidemiological cases from EpiFusion output folder into a list of dataframes
#'
#' This function loads the trajectories of fitted epidemiological cases from an EpiFusion analysis into a list of dataframes, with one dataframe for each chain in the analysis. The data frames are structured with ncol = number of epidemiological case data points, and nrow = number of MCMC samples.
#'
#' @param folderpath filepath to folder
#' @return a list of dataframes with fitted epidemiological case trajectories
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
