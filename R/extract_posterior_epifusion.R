#' Parse results from raw EpiFusion object into a R object with infection, Rt, cumulative infection and parameter posteriors
#'
#' This function discards the burn-in from the MCMC samples enclosed in a raw EpiFusion object. The posterior samples, and Rhat and ESS statistics for the parameters of the analysis, are stored in a list. Mean values, and HPD intervals for each trajectory type (infections, Rt, cumulative infections and, if applicable, modelled cases) and parameter are provided in the result. To save the posterior samples of each, set `include_samples` to `TRUE` (although doing this may made your object very large)
#'
#' @param raw_epifusion raw EpiFusion object
#' @param burn_in  proportion (as a decimal) of each chain to discard as burn in
#' @param discard_chains (optional) integer vector of chain IDs to discard in the posterior if there were non convergent chains
#' @param include_samples (optional) boolean specifying whether to save the posterior samples in the output object (as opposed to just means and HPDs)
#' @return a list of R compatible EpiFusion output objects with burnin discarded
#' @importFrom HDInterval hdi
#' @importFrom stableGR stable.GR
#' @export


extract_posterior_epifusion <- function(raw_epifusion, burn_in, discard_chains = NA, include_samples = FALSE) {
  chains <- seq(1, length(raw_epifusion$infection_trajectories))
  if (!any(is.na(discard_chains))) {
    chains <- chains[-discard_chains]
  }

  discard <- round(nrow(raw_epifusion$infection_trajectories[[1]])*burn_in)
  infection_trajectories <- raw_epifusion$infection_trajectories
  infection_trajectory_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(infection_trajectories[[1]])))
  for (i in chains) {
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
  if (include_samples) {
    infection_trajectories = list(mean_infection_trajectory = mean_infection_trajectory,
                                  infection_trajectory_hpdintervals = infection_trajectory_hpdintervals,
                                  infection_trajectory_samples = infection_trajectory_posterior)
  } else {
    infection_trajectories = list(mean_infection_trajectory = mean_infection_trajectory,
                                  infection_trajectory_hpdintervals = infection_trajectory_hpdintervals)
  }


  rt_trajectories <- raw_epifusion$rt_trajectories
  rt_trajectory_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(rt_trajectories[[1]])))
  for (i in chains) {
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
  if (include_samples) {
    rt_trajectories = list(mean_rt_trajectory = mean_rt_trajectory,
                           rt_trajectory_hpdintervals = rt_trajectory_hpdintervals,
                           rt_trajectory_samples = rt_trajectory_posterior)
  } else {
    rt_trajectories = list(mean_rt_trajectory = mean_rt_trajectory,
                           rt_trajectory_hpdintervals = rt_trajectory_hpdintervals)
  }


  parameters <- colnames(raw_epifusion$parameter_samples[[1]])
  parameter_posterior <- list()
  for (i in 1:length(parameters)) {
    paramsamples <- c()
    paramlist <- list()
    ind <- 1
    for (c in chains) {
      p <- raw_epifusion$parameter_samples[[c]][[i]]
      paramsamples <- append(paramsamples, p[discard:length(p)])
      paramlist[[ind]] <- as.matrix(p[discard:length(p)])
      ind <- ind + 1
    }
    sink(tempfile())
    grstats <- (stableGR::stable.GR(paramlist))
    sink()
    rhat <- (as.numeric(grstats$psrf))
    ess <- (round(as.numeric(grstats$n.eff)))
    if (include_samples) {
      parameter_posterior[[parameters[i]]] <- list(samples = paramsamples, rhat = rhat, ess = ess)
    } else {
      parameter_posterior[[parameters[i]]] <- list(mean = mean(paramsamples),
                                                   HPD0.95 = HDInterval::hdi(paramsamples, 0.95),
                                                   HPD0.88 = HDInterval::hdi(paramsamples, 0.88),
                                                   HPD0.66 = HDInterval::hdi(paramsamples, 0.66),
                                                   rhat = rhat,
                                                   ess = ess)
    }

  }

  cumulative_infections <- raw_epifusion$cumulative_infections
  cuminfection_trajectory_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(cumulative_infections[[1]])))
  for (i in chains) {
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
  if (include_samples) {
    cumulative_infections = list(mean_cuminfection_trajectory = mean_cuminfection_trajectory,
                                 cuminfection_trajectory_hpdintervals = cuminfection_trajectory_hpdintervals,
                                 cuminfection_trajectory_samples = cuminfection_trajectory_posterior)
  } else {
    cumulative_infections = list(mean_cuminfection_trajectory = mean_cuminfection_trajectory,
                                 cuminfection_trajectory_hpdintervals = cuminfection_trajectory_hpdintervals)
  }




  if (!any(is.na(raw_epifusion$fitted_epi_cases))) {
    fitted_epi_cases <- raw_epifusion$fitted_epi_cases
    fitted_epi_cases_posterior <- data.frame(matrix(0, nrow = 0, ncol = ncol(fitted_epi_cases[[1]])))
    for (i in chains) {
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
    if (include_samples) {
      fitted_epi_cases = list(mean_fitted_epi_cases = mean_fitted_epi_cases,
                              fitted_epi_cases_hpdintervals = fitted_epi_cases_hpdintervals,
                              fitted_epi_cases_samples = fitted_epi_cases_posterior)
    } else {
      fitted_epi_cases = list(mean_fitted_epi_cases = mean_fitted_epi_cases,
                              fitted_epi_cases_hpdintervals = fitted_epi_cases_hpdintervals)
    }

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
