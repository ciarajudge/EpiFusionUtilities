#' Prepare a tree (in S3 Phylo Object form) and case incidence (in data.frame form) for EpiFusion
#'
#' This function prepares a tree, given as an S3 phylo object, for EpiFusion analysis by adding 'time during outbreak' to the node and leaf labels. It also assigns 'time during outbreak' to the case incidence data making it easier to form the <incidence> node in EpiFusion XML.
#'
#' @param tree phylogenetic tree (in S3 Phylo Object format) with branch lengths in terms of days, or path to a file with the formatted tree(s)
#' @param case_incidence a data frame with case incidence and their date of occurrence. The data frame should consist of two columns: 'Date', a <Date> column with the observation dates, and 'Cases', a numeric column with reported cases on the corresponding day.
#' @param index_date the date, in <date> format, that you want to start modelling the outbreak from. Should be some time before
#' @param loggers ...
#' @param analysis ...
#' @param model ....
#' @param parameters ....
#' @param priors ...
#' @importFrom castor get_all_distances_to_root
#' @importFrom stringr str_remove_all
#' @importFrom ape read.tree
#' @importFrom ape write.tree
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom stats rnorm
#' @export


generate_epifusion_XML <- function(tree = NA, case_incidence = NA, index_date, loggers = NA, analysis = NA, model = NA, parameters = NA, priors = NA, xml_filepath = "input.xml") {
  #filePath <- system.file("extdata", "template.xml", package = "EpiFusionUtilities")
  filePath <- "../EpiFusionUtilities/inst/extdata/template.xml"
  doc <- xml2::read_xml(filePath)

  ### DATA
  # Check that some sort of data is provided
  if (any(is.na(tree)) && is.na(case_incidence)) {
    stop("ERROR: You have not passed any data! Please provide either a tree or case_incidence argument.")
  }
  # Do incidence (if provided)
  if (!all(is.na(case_incidence))) {
    incidencetimes <- as.numeric(as.Date(case_incidence$Date) - index_date)
    incidencevals <- case_incidence$Cases

    reaction_node2 <- xml2::xml_find_all(doc, "//incidenceVals")
    xml2::xml_text(reaction_node2) <- paste0(incidencevals, collapse = " ")
    reaction_node1 <- xml2::xml_find_all(doc, "//incidenceTimes")
    xml2::xml_text(reaction_node1) <- paste0(incidencetimes, collapse = " ")
  }
  # Do tree (if provided)
  if (!all(is.na(tree))) {
    if (class(tree) == "character") { #it's a file path
      tree_object <- read.tree(tree)
      if (class(tree_object) == "phylo") {
        phylouncertainty <- "false"
        reaction_node3 <- xml2::xml_find_all(doc, "//tree")
        xml2::xml_add_child(reaction_node3, "treeFile", tree)
      } else if (class(tree_object) == "multiPhylo") {
        phylouncertainty <- "true"
        reaction_node3 <- xml2::xml_find_all(doc, "//tree")
        xml2::xml_add_child(reaction_node3, "treeFile", tree)
      } else {
        stop("ERROR: The contents of the tree file path passed to the function do not contain newick tree strings.")
      }
    } else if (class(tree) == "phylo") { #it's a single tree
      phylouncertainty <- "false"
      treestring <- ape::write.tree(tree)
      reaction_node3 <- xml2::xml_find_all(doc, "//tree")
      xml2::xml_add_child(reaction_node3, "treeString", treestring)
    }
    phylouncertainty_node <- xml2::xml_find_all(doc, "//treePosterior")
    xml2::xml_text(phylouncertainty_node) <- phylouncertainty
  }

  ### LOGGERS
  if (!any(is.na(loggers))) {
    reaction_node2 <- xml2::xml_find_all(doc, "//fileBase")
    xml2::xml_text(reaction_node2) <- loggers$fileBase
    reaction_node2 <- xml2::xml_find_all(doc, "//logEvery")
    xml2::xml_text(reaction_node2) <- as.character(loggers$logEvery)
  }

  ### ANALYSIS
  if (!any(is.na(analysis))) {
    reaction_node2 <- xml2::xml_find_all(doc, "//type")
    xml2::xml_text(reaction_node2) <- analysis$fileBase
    reaction_node2 <- xml2::xml_find_all(doc, "//startTime")
    xml2::xml_text(reaction_node2) <- as.character(analysis$startTime)
    reaction_node2 <- xml2::xml_find_all(doc, "//endTime")
    xml2::xml_text(reaction_node2) <- as.character(analysis$endTime)
  }

  ### MODEL
  if (!any(is.na(model))) {
    reaction_node2 <- xml2::xml_find_all(doc, "//epiObservationModel")
    xml2::xml_text(reaction_node2) <- model$epiObservation
  }

  ### PARAMETERS
  if (!any(is.na(parameters))) {
    for (i in 1:length(parameters)) {
      reaction_node2 <- xml2::xml_find_all(doc, paste0("//", names(parameters)[i]))
      xml2::xml_text(reaction_node2) <- as.character(parameters[[i]])
    }
  }

  ### PRIORS
  if (!any(is.na(priors))) {
    prior_node <- xml2::xml_find_all(doc, "//priors")
    xml2::xml_remove(xml2::xml_children(prior_node))
    for (i in 1:length(priors)) {
      name <- names(priors)[i]
      xml2::xml_add_child(prior_node, name)
      new_node <- xml2::xml_find_all(doc, paste0("//", name))
      for (j in 1:length(priors[[i]])) {
        xml2::xml_add_child(new_node, names(priors[[i]])[j], as.character(priors[[i]][[j]]))
      }
    }
  }

  # Save the modified XML to a new file
  xml2::write_xml(doc, xml_filepath)
}


