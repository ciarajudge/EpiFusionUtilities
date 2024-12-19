#' Load results from EpiFusion folder into an R object
#'
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included. Also plots the likelihood and parameter traces (set suppress_plots to `TRUE` to prevent this)
#'
#' @param folderpath filepath to EpiFusion output folder
#' @param suppress_plots (optional) set to `TRUE` to prevent the likelihood and parameter trace being plotted automatically
#' @return a list of R compatible EpiFusion output objects
#' @importFrom stringr str_count
#' @export

load_raw_epifusion <- function(folderpath, suppress_plots = FALSE) {
  filepaths <- list.files(folderpath)
  num_chains <- sum(stringr::str_count(filepaths, 'likelihoods'))
  likelihoods <- load_likelihoods(folderpath)
  samples_per_chain <- length(likelihoods$Chain1)
  infection_trajectories <- load_infection_trajectories(folderpath)
  beta_trajectories <- load_beta_trajectories(folderpath)
  rt_trajectories <- load_rt_trajectories(folderpath)
  acceptance <- load_acceptance(folderpath)
  parameter_samples <- load_parameter_samples(folderpath)
  fitted_cases <- load_fitted_epi_cases(folderpath)
  cumulative_infections <- load_cumulativeinfection_trajectories(folderpath)
  raw_epifusion <- list(num_chains = num_chains,
                        samples_per_chain = samples_per_chain,
                        likelihoods = likelihoods,
                        acceptance_rate = acceptance,
                        infection_trajectories = infection_trajectories,
                        parameter_samples = parameter_samples,
                        rt_trajectories = rt_trajectories,
                        fitted_epi_cases = fitted_cases,
                        cumulative_infections = cumulative_infections)
  if (!suppress_plots) {
    print(plot_likelihood_trace(raw_epifusion))
    print(plot_parameter_trace(raw_epifusion))
  }

  return(raw_epifusion)
}


