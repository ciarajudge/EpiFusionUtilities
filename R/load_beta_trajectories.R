#' Load beta trajectories from EpiFusion output folder into a list of dataframes
#'
#' This function loads the beta trajectories of an EpiFusion analysis into a list of dataframes, with one dataframe for each chain in the analysis. The data frames are structured with ncol = number of days in the analysis, and nrow = number of MCMC samples.
#'
#' @param folderpath filepath to folder
#' @return a list of beta trajectory dataframes
#' @importFrom utils read.csv
#' @export
#'

load_beta_trajectories <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "beta")
  betas <- list()
  for (f in 1:length(filepaths)) {
    beta <- utils::read.csv(paste0(folderpath, filepaths[f]), header = T)
    beta <- beta[,1:(ncol(beta)-1)]
    betas[[paste0('Chain', f)]] <- beta
  }
  return(betas)
}
