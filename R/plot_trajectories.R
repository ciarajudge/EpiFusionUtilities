#' Plot the trajectory posteriors from a parsed EpiFusion trajectory table
#'
#' Plots the trajectory posteriors from an EpiFusion trajectory table. You can specify the trajectory type (infection, Rt, cumulative infection) using the `type` parameter, or the function will plot each type by default. You may also specify any number of colours using the `plot_colours` parameter.
#'
#' @param trajectory_table a trajectory table extracted from an EpiFusion posterior object
#' @param type (optional) "infection" "rt" or "cumulativeinfection"
#' @param plot_colours (optional) a vector of colours to be used in the plots
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @export
#'

plot_trajectories <- function(trajectory_table, type = NA, plot_colours = NA) {
  if (any(is.na(plot_colours))) {
    cols <- c("#2aac6d", "#00abce", "#fbb800")
  } else {
    cols <- rep(plot_colours, length.out = 3)
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
      pivot_wider(names_from = Statistic, values_from = value) %>%
      mutate(Characteristic = factor(Characteristic, levels = c("Infected", "Rt", "CumulativeInfections"))) %>%
      mutate(Rtline = ifelse(Characteristic == "Rt", 1, NA))

    p <- ggplot(pivoted_table, aes(x = Time, col = Characteristic, fill = Characteristic)) +
      geom_line(aes(y = Mean), show.legend = F) +
      geom_line(aes(y = Rtline), col = "grey", linetype = 2) +
      geom_ribbon(aes(ymin = Lower95, ymax = Upper95), col = NA, alpha = 0.2, show.legend = F) +
      geom_ribbon(aes(ymin = Lower88, ymax = Upper88), col = NA, alpha = 0.2, show.legend = F) +
      geom_ribbon(aes(ymin = Lower66, ymax = Upper66), col = NA, alpha = 0.2, show.legend = F) +
      scale_color_manual(name = "", values = cols) +
      scale_fill_manual(name = "", values = cols) +
      labs(y = "") +
      facet_wrap(~Characteristic, ncol = 1, scales = "free") +
      lshtm_theme()

    suppressWarnings(print(p))

  } else if (type == "infection") {
    p <- ggplot(trajectory_table, aes(x = Time)) +
      geom_line(aes(y = Mean_Infected), col = cols[1]) +
      geom_ribbon(aes(ymin = Lower95_Infected, ymax = Upper95_Infected), col = NA, alpha = 0.2, fill = cols[1]) +
      geom_ribbon(aes(ymin = Lower88_Infected, ymax = Upper88_Infected), col = NA, alpha = 0.2, fill = cols[1]) +
      geom_ribbon(aes(ymin = Lower66_Infected, ymax = Upper66_Infected), col = NA, alpha = 0.2, fill = cols[1]) +
      labs(y = "Infected Individuals") +
      lshtm_theme()
    suppressWarnings(print(p))

  } else if (type == "rt") {
    p <- ggplot(trajectory_table, aes(x = Time)) +
      geom_line(aes(y = Mean_Rt), col = cols[2]) +
      geom_ribbon(aes(ymin = Lower95_Rt, ymax = Upper95_Rt), col = NA, alpha = 0.2, fill = cols[2]) +
      geom_ribbon(aes(ymin = Lower88_Rt, ymax = Upper88_Rt), col = NA, alpha = 0.2, fill = cols[2]) +
      geom_ribbon(aes(ymin = Lower66_Rt, ymax = Upper66_Rt), col = NA, alpha = 0.2, fill = cols[2]) +
      labs(y = "Effective Reproduction Number") +
      lshtm_theme()
    suppressWarnings(print(p))

  } else if (type == "cumulativeinfection") {
    p <- ggplot(trajectory_table, aes(x = Time)) +
      geom_line(aes(y = Mean_CumulativeInfections), col = cols[3]) +
      geom_ribbon(aes(ymin = Lower95_CumulativeInfections, ymax = Upper95_CumulativeInfections), col = NA, alpha = 0.2, fill = cols[3]) +
      geom_ribbon(aes(ymin = Lower88_CumulativeInfections, ymax = Upper88_CumulativeInfections), col = NA, alpha = 0.2, fill = cols[3]) +
      geom_ribbon(aes(ymin = Lower66_CumulativeInfections, ymax = Upper66_CumulativeInfections), col = NA, alpha = 0.2, fill = cols[3]) +
      labs(y = "Cumulative Infections") +
      lshtm_theme()
    suppressWarnings(print(p))

  } else {
    print(paste0("ERROR: type provided (",type,") does not match any trajectory type (infection, rt, cumulativeinfection)"))
    return()
  }
}

