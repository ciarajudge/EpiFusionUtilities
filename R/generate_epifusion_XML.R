#' Generate an EpiFusion XML file
#'
#' This function takes a phylogenetic tree and/or case incidence data, in addition to some other information (analysis index date etc) and creates a parameter file for EpiFusion in eXtensible Markup Language.
#'
#' @param tree phylogenetic tree (in S3 Phylo Object format) with branch lengths in terms of days, or path to a file with the formatted tree(s)
#' @param case_incidence a data frame with case incidence and their date of occurrence. The data frame should consist of two columns: 'Date', a <Date> column with the observation dates, and 'Cases', a numeric column with reported cases on the corresponding day.
#' @param index_date the date, in <date> format, that you want to start modelling the outbreak from. Should be some time before
#' @param loggers list of logging information (frequency, where to log the results files)
#' @param analysis (optional) list of analysis instructions (e.g. how you wish to fit beta), if you wish to deviate from the default
#' @param model (optional) list of model instructions (e.g. epidemiological observation model), if you wish to deviate from the default
#' @param parameters (optional) list of parameters (e.g. how many MCMC steps), if you wish to deviate from the default
#' @param priors (optional) list of prior distributions for the pMCMC
#' @param xml_filepath filepath to write the complete XML file
#' @importFrom castor get_all_distances_to_root
#' @importFrom stringr str_remove_all
#' @importFrom ape read.tree
#' @importFrom ape write.tree
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_remove
#' @importFrom stats rnorm
#' @export


generate_epifusion_XML <- function(tree = NA, case_incidence = NA, index_date, loggers = NA, analysis = NA, model = NA, parameters = NA, priors = NA, xml_filepath = "input.xml") {
  filePath <- system.file("extdata", "template.xml", package = "EpiFusionUtilities")

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
  indexdate_node <- xml2::xml_find_all(doc, "//indexdate")
  xml2::xml_text(indexdate_node) <- as.character(index_date, format = "%Y-%m-%d")

  ### LOGGERS
  if (!any(is.na(loggers))) {
    reaction_node2 <- xml2::xml_find_all(doc, "//fileBase")
    xml2::xml_text(reaction_node2) <- loggers$fileBase
    reaction_node2 <- xml2::xml_find_all(doc, "//logEvery")
    xml2::xml_text(reaction_node2) <- as.character(loggers$logEvery)
  }

  ### ANALYSIS
  if (!any(is.na(analysis))) {
    for (i in 1:length(analysis)) {
      reaction_node2 <- xml2::xml_find_all(doc, paste0("//", names(analysis)[i]))
      xml2::xml_text(reaction_node2) <- as.character(analysis[[i]])
    }
  }

  ### MODEL
  if (!any(is.na(model))) {
    for (i in 1:length(model)) {
      reaction_node2 <- xml2::xml_find_all(doc, paste0("//", names(model)[i]))
      xml2::xml_text(reaction_node2) <- as.character(model[[i]])
    }
  }

  ### PARAMETERS
  if (!any(is.na(parameters))) {
    for (i in 1:length(parameters)) {
      reaction_node2 <- xml2::xml_find_all(doc, paste0("//", names(parameters)[i]))
      xml2::xml_text(reaction_node2) <- as.character(parameters[[i]])
    }
  }

  ### PRIORS
  if (!any(is.na(priors))) { #Priors
    prior_node <- xml2::xml_find_all(doc, "//priors")
    prior_node <- xml2::xml_find_first(doc, "//priors")
    generate_XML_chunk(priors, prior_node) #Add to the xml, fine makes sense
  }

  # Save the modified XML to a new file
  xml2::write_xml(doc, xml_filepath)
}


