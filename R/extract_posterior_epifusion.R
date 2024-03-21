#' Load results from EpiFusion folder into an R object
#' This function loads the result of an EpiFusion folder into an R list, with the key characteristics and results of the analysis included.
#'
#' @param raw_epifusion filepath to folder
#' @param burn_in  proportion (as a decimal) of chains to discard as burn in
#' @return a list of R compatible EpiFusion output objects
#' @export


extract_posterior_epifusion <- function(raw_epifusion, burn_in) {
  discard <- round(nrow(raw_epifusion$infection_trajectories[[1]])*burn_in)
  infection_trajectories <- raw_epifusion$infection_trajectories
  infection_trajectory_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(infection_trajectories[[1]])))
  for (i in 1:length(infection_trajectories)) {
    tmp <- infection_trajectories[[i]]
    tmp <- tmp[discard:nrow(tmp),]
    infection_trajectory_posterior <- rbind(infection_trajectory_posterior, tmp)
  }
  rt_trajectories <- raw_epifusion$rt_trajectories
  rt_trajectory_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(rt_trajectories[[1]])))
  for (i in 1:length(rt_trajectories)) {
    tmp <- rt_trajectories[[i]]
    tmp <- tmp[discard:nrow(tmp),]
    rt_trajectory_posterior <- rbind(rt_trajectory_posterior, tmp)
  }
  parameters <- colnames(raw_epifusion$parameter_samples[[1]])
  parameter_posterior <- list()
  for (i in 1:length(parameters)) {
    param <- c()
    for (c in 1:raw_epifusion$num_chains) {
      p <- raw_epifusion$parameter_samples[[c]][[i]]
      param <- append(param, p[discard:length(p)])
    }
    parameter_posterior[[parameters[i]]] <- param
  }
  return(list(infection_trajectories = infection_trajectory_posterior,
              rt_trajectories = rt_trajectory_posterior,
              parameters = parameter_posterior))
}
