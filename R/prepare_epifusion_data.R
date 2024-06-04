#' Prepare a tree (in S3 Phylo Object form) and case incidence (in data.frame form) for EpiFusion
#' This function prepares a tree, given as an S3 phylo object, for EpiFusion analysis by adding 'time during outbreak' to the node and leaf labels. It also assigns 'time during outbreak' to the case incidence data making it easier to form the <incidence> node in EpiFusion XML.
#'
#' @param tree phylogenetic tree (in S3 Phylo Object format) with branch lengths in terms of days
#' @param case_incidence a data frame with case incidence and their date of occurrence. The data frame should consist of two columns: 'Date', a <Date> column with the observation dates, and 'Cases', a numeric column with reported cases on the corresponding day.
#' @param start_date the date, in <date> format, that you want to start modelling the outbreak from. Should be some time before
#' @importFrom castor get_all_distances_to_root
#' @importFrom stringr str_remove_all
#' @importFrom ape write.tree
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom stats rnorm
#' @export


prepare_epifusion_data <- function(tree, case_incidence, start_date) {
  filePath <- system.file("extdata", "template.xml", package = "EpiFusionUtilities")

  # Get epi information
  incidencetimes <- as.numeric(as.Date(case_incidence$Date) - start_date)
  incidencevals <- case_incidence$Cases
  # Get the latest tip date on the tree
  tip_dates <- as.Date(stringr::str_remove_all(tree$tip.label, "(.*\\|)"))
  distances <- castor::get_all_distances_to_root(tree) + rnorm(length(castor::get_all_distances_to_root(tree)), 0.001, 0.001)
  nib <- (as.numeric(max(tip_dates) - start_date) - max(distances)) + 0.5
  distances <- distances + nib
  node_distances <- distances[(length(tree$tip.label)+1):(length(tree$tip.label)+tree$Nnode)]
  tip_distances <- distances[1:length(tree$tip.label)]
  treecopy <- tree
  treecopy$node.label <- paste0("X[", node_distances, "]")
  treecopy$tip.label <- paste0("X[", tip_distances, "]")
  treestring <- write.tree(treecopy)

  doc <- xml2::read_xml(filePath)

  # Replace the desired elements
  reaction_node2 <- xml2::xml_find_all(doc, "//incidenceVals")
  xml2::xml_text(reaction_node2) <- paste0(incidencevals, collapse = " ")
  reaction_node1 <- xml2::xml_find_all(doc, "//incidenceTimes")
  xml2::xml_text(reaction_node1) <- paste0(incidencetimes, collapse = " ")
  reaction_node3 <- xml2::xml_find_all(doc, "//tree")
  xml2::xml_text(reaction_node3) <- paste0(treestring, collapse = " ")

  # Save the modified XML to a new file
  xml2::write_xml(doc, "input.xml")
}


