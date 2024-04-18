#' Plots the likelihood trace of the chains of an EpiFusion analysis
#' This function plots the likelihood trace of the chains of an EpiFusion analysis to assess convergence and help to select the burn-in proportion.
#'
#' @param raw_epifusion raw epifusion object
#' @importFrom graphics lines
#' @export


plot_likelihood_trace <- function(raw_epifusion) {
  likelihoods <- raw_epifusion$likelihoods
  l <- unlist(likelihoods)
  ymin <- min(l[l != -Inf])
  ymax <- max(l)
  plot(likelihoods[[1]], type = "s", ylim = c(ymin, ymax), col = 1, ylab = 'Likelihood', xlab = 'Sample')
  for (i in 2:length(likelihoods)) {
    lines(likelihoods[[i]], type = "s", col = i)
  }
}
