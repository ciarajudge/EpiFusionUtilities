#' EpiFusionUtilities: Tools for Implementing EpiFusion Joint Inference Models
#'
#' The EpiFusionUtilities package provides a suite of functions for preparing data
#' to run joint inference models using the EpiFusion particle filtering infrastructure.
#' It is also possible to run EpiFusion from within you R session using the package, and
#' parse the results.
#'
#' @name EpiFusionUtilities
#' @keywords internal
#'
#' @section Main Functions:
#' - `prepare_epifusion_tree`: For preparing time rooted phylogenetic tree(s).
#' - `generate_epifusion_XML`: For generating EpiFusion XML input files.
#' - `run_epifusion`: For running EpiFusion within R.
#' - `load_raw_epifusion`: For loading raw results of EpiFusion analysis.
#' - `extract_posterior_epifusion`: For discarding MCMC chain burnin and extracting parameter posteriors.
#'
#' @section Installation:
#' You can install the development version from GitHub with:
#' ```
#' devtools::install_github("https://github.com/ciarajudge/EpiFusionUtilities")
#' ```
#'
#'
#' @section Maintainer:
#' Ciara Judge <ciara.judge@lshtm.ac.uk>
#'
#' @examples
#' # Example usage
#' library(EpiFusionUtilities)
#' run_epifusion("path/to/EpiFusion/XML/file")
"_PACKAGE"
