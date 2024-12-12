#' Run EpiFusion from R
#'
#' This function runs EpiFusion from R, assuming you have java installed and callable from your system using the `java` command.
#'
#' @param xml_filepath path to the xml input file
#' @param output_folder_name the name of the folder you'd like created for the output
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @export


run_epifusion <- function(xml_filepath, output_folder_name = NA) {
  filePath <- system.file("java", "EpiFusion.jar", package = "EpiFusionUtilities")

  if (!is.na(output_folder_name)) {
    doc <- xml2::read_xml(xml_filepath)

    # Replace the desired elements
    reaction_node2 <- xml2::xml_find_all(doc, "//fileBase")
    xml2::xml_text(reaction_node2) <- output_folder_name

    # Save the modified XML to a new file
    xml2::write_xml(doc, xml_filepath)
  }


  system(paste0("java -jar ",filePath," ",xml_filepath))
}

