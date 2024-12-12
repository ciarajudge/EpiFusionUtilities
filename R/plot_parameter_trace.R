#' Plot the parameter traces from a raw EpiFusion object
#'
#' T
#'
#' @param raw_epifusion raw epifusion object
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @import ggpubr
#' @export
#'

plot_parameter_trace <- function(raw_epifusion) {
  sink(tempfile())
  params <- raw_epifusion$parameter_samples

  params_constant <- data.frame(Chain = character(0),
                                Parameter = character(0),
                                Sample = integer(0),
                                Value = numeric(0))

  params_timevar <- data.frame(Chain = character(0),
                               Parameter = character(0),
                               Sample = integer(0),
                               Time = integer(0),
                               Value = numeric(0))
  maxtime <- ncol(raw_epifusion$infection_trajectories$Chain1)

  for (i in 1:4) {
    constant_params_chain <- params[[i]] %>%
      dplyr::select(!contains("distrib")) %>%
      dplyr::select(!contains("changetime")) %>%
      dplyr::mutate(Sample = row_number()) %>%
      tidyr::pivot_longer(!Sample, names_to = "Parameter", values_to = "Value") %>%
      dplyr::mutate(Chain = names(params)[[i]])
    params_constant <- rbind(params_constant, constant_params_chain)

    timevar_params_chain_distribs <- params[[i]] %>%
      dplyr::select(contains("distrib")) %>%
      dplyr::mutate(Sample = row_number()) %>%
      tidyr::pivot_longer(!Sample, names_to = "Parameter", values_to = "Value") %>%
      tidyr::separate(Parameter, into = c("Parameter", "Interval"), sep = "_distribs_") %>%
      dplyr::mutate(Interval = as.integer(Interval))

    timevar_params_chain_changetimes <- params[[i]] %>%
      dplyr::select(contains("changetime")) %>%
      dplyr::mutate(Sample = row_number()) %>%
      tidyr::pivot_longer(!Sample, names_to = "Parameter", values_to = "Time") %>%
      tidyr::separate(Parameter, into = c("Parameter", "Interval"), sep = "_changetime_") %>%
      dplyr::mutate(Interval = as.integer(Interval)+1)

    timevar_params_chain <- timevar_params_chain_distribs %>%
      dplyr::left_join(timevar_params_chain_changetimes) %>%
      dplyr::mutate(Time = ifelse(is.na(Time), 0, Time)) %>%
      dplyr::select(!Interval) %>%
      dplyr::mutate(Chain = names(params)[[i]])

    timevar_params_chain_max <- timevar_params_chain %>%
      dplyr::group_by(Chain, Sample, Parameter) %>%
      dplyr::filter(Time == max(Time)) %>%
      dplyr::mutate(Time = maxtime)

    timevar_params_chain <- rbind(timevar_params_chain, timevar_params_chain_max)

    params_timevar <- rbind(params_timevar, timevar_params_chain)

  }
  sink()

  fixed_params <- ggplot2::ggplot(params_constant, aes(x = Sample, y = Value, col = Chain)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~Parameter, scales = "free")

  timevarying_params <- ggplot2::ggplot(params_timevar, aes(x = Time, y = Value, group = Sample)) +
    ggplot2::geom_step(aes(col = Sample)) +
    ggplot2::facet_grid(Parameter ~ Chain, scales = "free")

  ggpubr::ggarrange(fixed_params, timevarying_params, ncol = 2)
}
