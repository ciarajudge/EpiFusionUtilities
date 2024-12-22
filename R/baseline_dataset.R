#' Load baseline simulated dataset for trying EpiFusion
#'
#' This function loads a simple simulated outbreak dataset (a phylogenetic tree and weekly case incidence) to your global environment
#'
#' @export


baseline_dataset <- function() {
  baseline_tree <<- readRDS(system.file("extdata", "baseline_tree.RDS", package = "EpiFusionUtilities"))
  baseline_caseincidence <<- readRDS(system.file("extdata", "baseline_caseincidence.RDS", package = "EpiFusionUtilities"))
  baseline_treeposterior <<- readRDS(system.file("extdata", "baseline_treeposterior.RDS", package = "EpiFusionUtilities"))
}
