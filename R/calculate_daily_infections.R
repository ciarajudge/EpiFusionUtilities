#' Calculate means and HPD intervals of new infections per day from the cumulative infections posterior samples
#'
#' This function takes the trajectory samples of cumulative infections from an EpiFusion posterior object and performs a row-wise 'diff' to find the mean and HPD interval for new daily infections.
#'
#' @param cumulative_infection_samples the samples of cumulative infection trajectories from an Epifusion posterior object (generated using the `extract_posterior_epifusion()` function with `include_samples` set to `TRUE`)
#' @param index_date (optional) date you'd like the time series to count from, as a date object or as a string with format YYYY-MM-DD
#' @param include_median (default `FALSE`) set to `TRUE` if you would like the median trajectory to be included in the output table
#' @return a table with a column for Time and a column for Mean, Upper and Lower 95%, 88% and 66% HPDs for inferred Infections and Rt
#' @import dplyr
#' @import HDInterval
#' @importFrom matrixStats colMedians
#' @export
#'


calculate_daily_infections <- function(cumulative_infection_samples, index_date = NA, include_median = FALSE) {
  newinfections <- t(apply(cumulative_infection_samples, 1, diff))
  mean <- colMeans(newinfections)
  median <- matrixStats::colMedians(newinfections)
  hpd95 <- HDInterval::hdi(newinfections, 0.95)
  hpd88 <- HDInterval::hdi(newinfections, 0.88)
  hpd66 <- HDInterval::hdi(newinfections, 0.66)

  if (include_median) {
    newinfections_table <- data.frame(Time = seq(1, length(mean)),
                                      Mean = mean,
                                      Median = median,
                                      Upper95 = hpd95[2,],
                                      Lower95 = hpd95[1,],
                                      Upper88 = hpd88[2,],
                                      Lower88 = hpd88[1,],
                                      Upper66 = hpd66[2,],
                                      Lower66 = hpd66[1,])
  } else {
    newinfections_table <- data.frame(Time = seq(1, length(mean)),
                                      Mean = mean,
                                      Upper95 = hpd95[2,],
                                      Lower95 = hpd95[1,],
                                      Upper88 = hpd88[2,],
                                      Lower88 = hpd88[1,],
                                      Upper66 = hpd66[2,],
                                      Lower66 = hpd66[1,])
  }

  if (!is.na(index_date)) {
    newinfections_table <- dplyr::mutate(newinfections_table, Time = ((Time - 1) + as.Date(index_date)))
  }
  return(newinfections_table)
}





