#' Plots the likelihood trace of the chains of an EpiFusion analysis
#'
#' This function plots the likelihood trace of the chains of an EpiFusion analysis to assess convergence and help to select the burn-in proportion.
#'
#' @param raw_epifusion raw epifusion object
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @export


plot_likelihood_trace <- function(raw_epifusion) {
  likelihoods <- raw_epifusion$likelihoods
  likelihood_df <- data.frame(Chain = character(0),
                                Likelihood = numeric(0),
                                Sample = integer(0))
  for (i in 1:length(likelihoods)) {
    ll_df <- data.frame(Chain = names(likelihoods)[i], Likelihood = likelihoods[[i]], Sample = seq(1, length(likelihoods[[i]])))
    likelihood_df <- rbind(likelihood_df, ll_df)
  }

  ggplot2::ggplot(likelihood_df, aes(x = Sample, y = Likelihood, col = Chain)) +
    ggplot2::geom_step() +
    lshtm_theme()
}
