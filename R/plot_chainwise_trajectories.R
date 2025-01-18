#' Plot the trajectory posteriors from a raw EpiFusion object separated by chain
#'
#' Plots the trajectory posteriors from a raw EpiFusion object, separated by chain to allow assessment of convergence. You can specify the trajectory type (infection, Rt, cumulative infection) using the `type` parameter, or the function will plot each type by default. You may specify a proportion of each chain to discard as burn-in with the `burn_in` parameter.
#'
#' @param raw_epifusion raw EpiFusion object
#' @param burn_in proportion (as a decimal) of each chain to discard as burn in
#' @param type (optional) "infection" "rt" or "cumulativeinfection"
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @export
#'

plot_chainwise_trajectories <- function(raw_epifusion, burn_in, type = NA) {
  trajectory_table <- data.frame(Time = numeric(0),
                                 Chain = character(0),
                                 Stuck = character(0),
                                 Mean_Infected = numeric(0),
                                 Lower95_Infected = numeric(0),
                                 Upper95_Infected = numeric(0),
                                 Lower88_Infected = numeric(0),
                                 Upper88_Infected = numeric(0),
                                 Lower66_Infected = numeric(0),
                                 Upper66_Infected = numeric(0),
                                 Mean_Rt = numeric(0),
                                 Lower95_Rt = numeric(0),
                                 Upper95_Rt = numeric(0),
                                 Lower88_Rt = numeric(0),
                                 Upper88_Rt = numeric(0),
                                 Lower66_Rt = numeric(0),
                                 Upper66_Rt = numeric(0),
                                 Mean_CumulativeInfections = numeric(0),
                                 Lower95_CumulativeInfections = numeric(0),
                                 Upper95_CumulativeInfections = numeric(0),
                                 Lower88_CumulativeInfections = numeric(0),
                                 Upper88_CumulativeInfections = numeric(0),
                                 Lower66_CumulativeInfections = numeric(0),
                                 Upper66_CumulativeInfections = numeric(0))

  ##### Discard burn in, extract HPDs #####
  chains <- seq(1, length(raw_epifusion$infection_trajectories))
  discard <- round(nrow(raw_epifusion$infection_trajectories[[1]])*burn_in)

  acceptance <- raw_epifusion$acceptance_rate
  infection_trajectories <- raw_epifusion$infection_trajectories
  rt_trajectories <- raw_epifusion$rt_trajectories
  cumulative_infections <- raw_epifusion$cumulative_infections

  for (i in chains) {
    tmp <- infection_trajectories[[i]]
    infection_trajectory_posterior <- tmp[discard:nrow(tmp),]
    Mean_Infected = colMeans(infection_trajectory_posterior)
    Lower95_Infected = HDInterval::hdi(infection_trajectory_posterior, 0.95)[1,]
    Upper95_Infected = HDInterval::hdi(infection_trajectory_posterior, 0.95)[2,]
    Lower88_Infected = HDInterval::hdi(infection_trajectory_posterior, 0.88)[1,]
    Upper88_Infected = HDInterval::hdi(infection_trajectory_posterior, 0.88)[2,]
    Lower66_Infected = HDInterval::hdi(infection_trajectory_posterior, 0.66)[1,]
    Upper66_Infected = HDInterval::hdi(infection_trajectory_posterior, 0.66)[2,]

    tmp <- rt_trajectories[[i]]
    rt_trajectory_posterior <- tmp[discard:nrow(tmp),]
    Mean_Rt = colMeans(rt_trajectory_posterior)
    Lower95_Rt = HDInterval::hdi(rt_trajectory_posterior, 0.95)[1,]
    Upper95_Rt = HDInterval::hdi(rt_trajectory_posterior, 0.95)[2,]
    Lower88_Rt = HDInterval::hdi(rt_trajectory_posterior, 0.88)[1,]
    Upper88_Rt = HDInterval::hdi(rt_trajectory_posterior, 0.88)[2,]
    Lower66_Rt = HDInterval::hdi(rt_trajectory_posterior, 0.66)[1,]
    Upper66_Rt = HDInterval::hdi(rt_trajectory_posterior, 0.66)[2,]

    tmp <- cumulative_infections[[i]]
    cuminfection_trajectory_posterior <- tmp[discard:nrow(tmp),]

    Mean_CumulativeInfections = colMeans(cuminfection_trajectory_posterior)
    Lower95_CumulativeInfections = HDInterval::hdi(cuminfection_trajectory_posterior, 0.95)[1,]
    Upper95_CumulativeInfections = HDInterval::hdi(cuminfection_trajectory_posterior, 0.95)[2,]
    Lower88_CumulativeInfections = HDInterval::hdi(cuminfection_trajectory_posterior, 0.88)[1,]
    Upper88_CumulativeInfections = HDInterval::hdi(cuminfection_trajectory_posterior, 0.88)[2,]
    Lower66_CumulativeInfections = HDInterval::hdi(cuminfection_trajectory_posterior, 0.66)[1,]
    Upper66_CumulativeInfections = HDInterval::hdi(cuminfection_trajectory_posterior, 0.66)[2,]

    stuck = ifelse(mean(acceptance[[i]][discard:raw_epifusion$samples_per_chain]) == 0, "YES", "NO")

    tempdf <- data.frame(Time = seq(1, length(Mean_Infected)),
                         Chain = names(raw_epifusion$infection_trajectories)[i],
                         Stuck = stuck,
                         Mean_Infected = Mean_Infected,
                         Lower95_Infected = Lower95_Infected,
                         Upper95_Infected = Upper95_Infected,
                         Lower88_Infected = Lower88_Infected,
                         Upper88_Infected = Upper88_Infected,
                         Lower66_Infected = Lower66_Infected,
                         Upper66_Infected = Upper66_Infected,
                         Mean_Rt = Mean_Rt,
                         Lower95_Rt = Lower95_Rt,
                         Upper95_Rt = Upper95_Rt,
                         Lower88_Rt = Lower88_Rt,
                         Upper88_Rt = Upper88_Rt,
                         Lower66_Rt = Lower66_Rt,
                         Upper66_Rt = Upper66_Rt,
                         Mean_CumulativeInfections = Mean_CumulativeInfections,
                         Lower95_CumulativeInfections = Lower95_CumulativeInfections,
                         Upper95_CumulativeInfections = Upper95_CumulativeInfections,
                         Lower88_CumulativeInfections = Lower88_CumulativeInfections,
                         Upper88_CumulativeInfections = Upper88_CumulativeInfections,
                         Lower66_CumulativeInfections = Lower66_CumulativeInfections,
                         Upper66_CumulativeInfections = Upper66_CumulativeInfections
    )

    trajectory_table <- rbind(trajectory_table, tempdf)


  }



  if (is.na(type)) {
    pivoted_table <- trajectory_table %>%
      pivot_longer(c(Mean_Infected,
                     Lower95_Infected,
                     Upper95_Infected,
                     Lower88_Infected,
                     Upper88_Infected,
                     Lower66_Infected,
                     Upper66_Infected,
                     Mean_Rt,
                     Lower95_Rt,
                     Upper95_Rt,
                     Lower88_Rt,
                     Upper88_Rt,
                     Lower66_Rt,
                     Upper66_Rt,
                     Mean_CumulativeInfections,
                     Lower95_CumulativeInfections,
                     Upper95_CumulativeInfections,
                     Lower88_CumulativeInfections,
                     Upper88_CumulativeInfections,
                     Lower66_CumulativeInfections,
                     Upper66_CumulativeInfections)) %>%
      separate(name, into = c("Statistic", "Characteristic"), sep = "_") %>%
      tidyr::pivot_wider(names_from = Statistic, values_from = value) %>%
      mutate(Characteristic = factor(Characteristic, levels = c("Infected", "Rt", "CumulativeInfections"))) %>%
      mutate(Rtline = ifelse(Characteristic == "Rt", 1, NA)) %>%
      mutate(Stuck = factor(Stuck, levels = c("NO", "YES")))

    p <- ggplot(pivoted_table, aes(x = Time, col = Chain, fill = Chain)) +
      geom_line(aes(y = Mean, linetype = Stuck), show.legend = F) +
      scale_linetype_manual(values = c(1,2)) +
      geom_line(aes(y = Rtline), col = "grey", linetype = 2) +
      geom_ribbon(aes(ymin = Lower95, ymax = Upper95), col = NA, alpha = 0.2, show.legend = F) +
      #geom_ribbon(aes(ymin = Lower88, ymax = Upper88), col = NA, alpha = 0.2, show.legend = F) +
      #geom_ribbon(aes(ymin = Lower66, ymax = Upper66), col = NA, alpha = 0.2, show.legend = F) +
      labs(y = "") +
      facet_wrap(~Characteristic, ncol = 1, scales = "free") +
      lshtm_theme()

    suppressWarnings(print(p))

  } else if (type == "infection") {
    p <- ggplot(trajectory_table, aes(x = Time, col = Chain, fill = Chain)) +
      geom_line(aes(y = Mean_Infected, linetype = Stuck)) +
      geom_ribbon(aes(ymin = Lower95_Infected, ymax = Upper95_Infected), col = NA, alpha = 0.2) +
      #geom_ribbon(aes(ymin = Lower88_Infected, ymax = Upper88_Infected), col = NA, alpha = 0.2) +
      #geom_ribbon(aes(ymin = Lower66_Infected, ymax = Upper66_Infected), col = NA, alpha = 0.2) +
      labs(y = "Infected Individuals") +
      lshtm_theme()
    suppressWarnings(print(p))

  } else if (type == "rt") {
    p <- ggplot(trajectory_table, aes(x = Time, col = Chain, fill = Chain)) +
      geom_line(aes(y = Mean_Rt, linetype = Stuck)) +
      geom_ribbon(aes(ymin = Lower95_Rt, ymax = Upper95_Rt), col = NA, alpha = 0.2) +
      #geom_ribbon(aes(ymin = Lower88_Rt, ymax = Upper88_Rt), col = NA, alpha = 0.2) +
      #geom_ribbon(aes(ymin = Lower66_Rt, ymax = Upper66_Rt), col = NA, alpha = 0.2) +
      labs(y = "Effective Reproduction Number") +
      lshtm_theme()
    suppressWarnings(print(p))

  } else if (type == "cumulativeinfection") {
    p <- ggplot(trajectory_table, aes(x = Time, col = Chain, fill = Chain)) +
      geom_line(aes(y = Mean_CumulativeInfections, linetype = Stuck)) +
      geom_ribbon(aes(ymin = Lower95_CumulativeInfections, ymax = Upper95_CumulativeInfections), col = NA, alpha = 0.2) +
      #geom_ribbon(aes(ymin = Lower88_CumulativeInfections, ymax = Upper88_CumulativeInfections), col = NA, alpha = 0.2, fill = cols[3]) +
      #geom_ribbon(aes(ymin = Lower66_CumulativeInfections, ymax = Upper66_CumulativeInfections), col = NA, alpha = 0.2, fill = cols[3]) +
      labs(y = "Cumulative Infections") +
      lshtm_theme()
    suppressWarnings(print(p))

  } else {
    print(paste0("ERROR: type provided (",type,") does not match any trajectory type (infection, rt, cumulativeinfection)"))
    return()
  }
}
