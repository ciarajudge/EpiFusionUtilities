#' Load step-change in sampling simulated dataset for trying EpiFusion
#'
#' This function loads a simple simulated outbreak dataset (a phylogenetic tree and weekly case incidence) to your global environment. In this dataset, sampling of both case incidence and genomic sequence data is initially low, followed by a sharp increase.
#'
#' @export


sampling_dataset <- function() {
  sampling_tree <<- readRDS(system.file("extdata", "sampling_tree.RDS", package = "EpiFusionUtilities"))
  sampling_caseincidence <<- readRDS(system.file("extdata", "sampling_caseincidence.RDS", package = "EpiFusionUtilities"))
}
