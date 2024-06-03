#' Extract a table with the mean and HPD intervals of infection and Rt trajectories from an EpiFusion posterior R object
#' This function extracts a table
#'
#' @param epifusion_posterior an extracted EpiFusion posterior object made with the extract_posterior_epifusion() function
#' @param start_date (optional) date you'd like the time series to count from, as a string with format YYYY-MM-DD
#' @return a table with a column for Time and a column for Mean, Upper and Lower 95%, 88% and 66% HPDs for inferred Infections and Rt
#' @import dplyr
#' @export
#'

trajectory_table <- function(epifusion_posterior, start_date = NA) {
  trajectorytable <- data.frame(Time = 1:length(epifusion_posterior$infection_trajectories$mean_infection_trajectory),
                                Mean_Infected = epifusion_posterior$infection_trajectories$mean_infection_trajectory,
                                Lower95_Infected = epifusion_posterior$infection_trajectories$infection_trajectory_hpdintervals$HPD0.95$Lower,
                                Upper95_Infected = epifusion_posterior$infection_trajectories$infection_trajectory_hpdintervals$HPD0.95$Upper,
                                Lower88_Infected = epifusion_posterior$infection_trajectories$infection_trajectory_hpdintervals$HPD0.88$Lower,
                                Upper88_Infected = epifusion_posterior$infection_trajectories$infection_trajectory_hpdintervals$HPD0.88$Upper,
                                Lower66_Infected = epifusion_posterior$infection_trajectories$infection_trajectory_hpdintervals$HPD0.66$Lower,
                                Upper66_Infected = epifusion_posterior$infection_trajectories$infection_trajectory_hpdintervals$HPD0.66$Upper,
                                Mean_Rt = epifusion_posterior$rt_trajectories$mean_rt_trajectory,
                                Lower95_Rt = epifusion_posterior$rt_trajectories$rt_trajectory_hpdintervals$HPD0.95$Lower,
                                Upper95_Rt = epifusion_posterior$rt_trajectories$rt_trajectory_hpdintervals$HPD0.95$Upper,
                                Lower88_Rt = epifusion_posterior$rt_trajectories$rt_trajectory_hpdintervals$HPD0.88$Lower,
                                Upper88_Rt = epifusion_posterior$rt_trajectories$rt_trajectory_hpdintervals$HPD0.88$Upper,
                                Lower66_Rt = epifusion_posterior$rt_trajectories$rt_trajectory_hpdintervals$HPD0.66$Lower,
                                Upper66_Rt = epifusion_posterior$rt_trajectories$rt_trajectory_hpdintervals$HPD0.66$Upper)
  if (!is.na(start_date)) {
    trajectorytable <- dplyr::mutate(trajectorytable, Time = ((Time - 1) + as.Date(start_date)))
  }
  return(trajectorytable)
}
