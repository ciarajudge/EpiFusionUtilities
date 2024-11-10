#' Parse results from raw EpiFusion object into a R object with infection, Rt and parameter posteriors
#'
#' This function discards the burn-in from the MCMC samples enclosed in a raw EpiFusion object. The posterior samples, and Rhat and ESS statistics for the parameters of the analysis, are stored in a list.
#'
#' @param raw_epifusion raw EpiFusion object
#' @param burn_in  proportion (as a decimal) of each chain to discard as burn in
#' @return a list of R compatible EpiFusion output objects with burnin discarded
#' @importFrom HDInterval hdi
#' @importFrom stableGR stable.GR
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
  mean_infection_trajectory = colMeans(infection_trajectory_posterior)
  HPD0.95 = list(Lower = HDInterval::hdi(infection_trajectory_posterior, 0.95)[1,], Upper = HDInterval::hdi(infection_trajectory_posterior, 0.95)[2,])
  HPD0.88 = list(Lower = HDInterval::hdi(infection_trajectory_posterior, 0.88)[1,], Upper = HDInterval::hdi(infection_trajectory_posterior, 0.88)[2,])
  HPD0.66 = list(Lower = HDInterval::hdi(infection_trajectory_posterior, 0.66)[1,], Upper = HDInterval::hdi(infection_trajectory_posterior, 0.66)[2,])
  infection_trajectory_hpdintervals = list(HPD0.95 = HPD0.95,
                                           HPD0.88 = HPD0.88,
                                           HPD0.66 = HPD0.66)
  infection_trajectories = list(mean_infection_trajectory = mean_infection_trajectory,
                                infection_trajectory_hpdintervals = infection_trajectory_hpdintervals,
                                infection_trajectory_samples = infection_trajectory_posterior)

  rt_trajectories <- raw_epifusion$rt_trajectories
  rt_trajectory_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(rt_trajectories[[1]])))
  for (i in 1:length(rt_trajectories)) {
    tmp <- rt_trajectories[[i]]
    tmp <- tmp[discard:nrow(tmp),]
    rt_trajectory_posterior <- rbind(rt_trajectory_posterior, tmp)
  }
  mean_rt_trajectory = colMeans(rt_trajectory_posterior, na.rm = TRUE)
  HPD0.95 = list(Lower = HDInterval::hdi(rt_trajectory_posterior, 0.95)[1,], Upper = HDInterval::hdi(rt_trajectory_posterior, 0.95)[2,])
  HPD0.88 = list(Lower = HDInterval::hdi(rt_trajectory_posterior, 0.88)[1,], Upper = HDInterval::hdi(rt_trajectory_posterior, 0.88)[2,])
  HPD0.66 = list(Lower = HDInterval::hdi(rt_trajectory_posterior, 0.66)[1,], Upper = HDInterval::hdi(rt_trajectory_posterior, 0.66)[2,])
  rt_trajectory_hpdintervals = list(HPD0.95 = HPD0.95,
                                           HPD0.88 = HPD0.88,
                                           HPD0.66 = HPD0.66)
  rt_trajectories = list(mean_rt_trajectory = mean_rt_trajectory,
                                rt_trajectory_hpdintervals = rt_trajectory_hpdintervals,
                                rt_trajectory_samples = rt_trajectory_posterior)

  parameters <- colnames(raw_epifusion$parameter_samples[[1]])
  parameter_posterior <- list()
  for (i in 1:length(parameters)) {
    paramsamples <- c()
    paramlist <- list()
    for (c in 1:raw_epifusion$num_chains) {
      p <- raw_epifusion$parameter_samples[[c]][[i]]
      paramsamples <- append(paramsamples, p[discard:length(p)])
      paramlist[[c]] <- as.matrix(p[discard:length(p)])
    }
    grstats <- suppressWarnings(stableGR::stable.GR(paramlist))
    rhat <- as.numeric(grstats$psrf)
    ess <- round(as.numeric(grstats$n.eff))
    parameter_posterior[[parameters[i]]] <- list(samples = paramsamples, rhat = rhat, ess = ess)
  }

  cumulative_infections <- raw_epifusion$cumulative_infections
  cuminfection_trajectory_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(cumulative_infections[[1]])))
  for (i in 1:length(cumulative_infections)) {
    tmp <- cumulative_infections[[i]]
    tmp <- tmp[discard:nrow(tmp),]
    cuminfection_trajectory_posterior <- rbind(cuminfection_trajectory_posterior, tmp)
  }
  mean_cuminfection_trajectory = colMeans(cuminfection_trajectory_posterior)
  HPD0.95 = list(Lower = HDInterval::hdi(cuminfection_trajectory_posterior, 0.95)[1,], Upper = HDInterval::hdi(cuminfection_trajectory_posterior, 0.95)[2,])
  HPD0.88 = list(Lower = HDInterval::hdi(cuminfection_trajectory_posterior, 0.88)[1,], Upper = HDInterval::hdi(cuminfection_trajectory_posterior, 0.88)[2,])
  HPD0.66 = list(Lower = HDInterval::hdi(cuminfection_trajectory_posterior, 0.66)[1,], Upper = HDInterval::hdi(cuminfection_trajectory_posterior, 0.66)[2,])
  cuminfection_trajectory_hpdintervals = list(HPD0.95 = HPD0.95,
                                           HPD0.88 = HPD0.88,
                                           HPD0.66 = HPD0.66)
  cumulative_infections = list(mean_cuminfection_trajectory = mean_cuminfection_trajectory,
                                cuminfection_trajectory_hpdintervals = cuminfection_trajectory_hpdintervals,
                                cuminfection_trajectory_samples = cuminfection_trajectory_posterior)



  if (!any(is.na(raw_epifusion$fitted_epi_cases))) {
    fitted_epi_cases <- raw_epifusion$fitted_epi_cases
    fitted_epi_cases_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(fitted_epi_cases[[1]])))
    for (i in 1:length(fitted_epi_cases)) {
      tmp <- fitted_epi_cases[[i]]
      tmp <- tmp[discard:nrow(tmp),]
      fitted_epi_cases_posterior <- rbind(fitted_epi_cases_posterior, tmp)
    }
    mean_fitted_epi_cases = colMeans(fitted_epi_cases_posterior)
    HPD0.95 = list(Lower = HDInterval::hdi(fitted_epi_cases_posterior, 0.95)[1,], Upper = HDInterval::hdi(fitted_epi_cases_posterior, 0.95)[2,])
    HPD0.88 = list(Lower = HDInterval::hdi(fitted_epi_cases_posterior, 0.88)[1,], Upper = HDInterval::hdi(fitted_epi_cases_posterior, 0.88)[2,])
    HPD0.66 = list(Lower = HDInterval::hdi(fitted_epi_cases_posterior, 0.66)[1,], Upper = HDInterval::hdi(fitted_epi_cases_posterior, 0.66)[2,])
    fitted_epi_cases_hpdintervals = list(HPD0.95 = HPD0.95,
                                         HPD0.88 = HPD0.88,
                                         HPD0.66 = HPD0.66)
    fitted_epi_cases = list(mean_fitted_epi_cases = mean_fitted_epi_cases,
                            fitted_epi_cases_hpdintervals = fitted_epi_cases_hpdintervals,
                            fitted_epi_cases_samples = fitted_epi_cases_posterior)
    return(list(infection_trajectories = infection_trajectories,
                rt_trajectories = rt_trajectories,
                parameters = parameter_posterior,
                fitted_epi_cases = fitted_epi_cases,
                cumulative_infections = cumulative_infections))
  } else {
    return(list(infection_trajectories = infection_trajectories,
                rt_trajectories = rt_trajectories,
                parameters = parameter_posterior,
                cumulative_infections = cumulative_infections))
  }

}
