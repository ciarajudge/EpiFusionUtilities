#' Load cumulative infections trajectories from EpiFusion output folder into a list of dataframes
#'
#' This function loads the cumulative infection trajectories of an EpiFusion analysis into a list of dataframes, with one dataframe for each chain in the analysis. The data frames are structured with ncol = number of days in the analysis, and nrow = number of MCMC samples.
#'
#' @param folderpath filepath to folder
#' @return a list of cumulative trajectory dataframes
#' @importFrom utils read.csv
#' @export
#'

load_cumulativeinfection_trajectories <- function(folderpath) {
  filepaths <- list.files(folderpath, pattern = "cum")
  cuminfections <- list()
  for (f in 1:length(filepaths)) {
    cuminfection <- utils::read.csv(paste0(folderpath, filepaths[f]), header = T)
    cuminfection <- cuminfection[,1:(ncol(cuminfection)-1)]
    for (i in 1:nrow(cuminfection)) {
      cuminfection[i,] <- cumsum(unlist(cuminfection[i,]))
    }
    cuminfections[[paste0('Chain', f)]] <- cuminfection
  }
  return(cuminfections)
}
