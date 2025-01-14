#' Generate an XML chunk from a list
#'
#'
#' @param plist ...
#' @param node ...
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_remove
#' @keywords internal


generate_XML_chunk <- function(plist, node) {
  for (i in 1:length(plist)) {
    name <- names(plist)[i]
    value <- plist[[i]]

    if (name == "pairedPsi") {
      node_to_remove <- xml2::xml_find_first(node, paste0("./psi"))
      if (!is.na(node_to_remove)) {
        xml_remove(node_to_remove)
      }
    }

    #Remove the node if it exists already
    node_to_remove <- xml2::xml_find_first(node, paste0("./", name))
    if (!is.na(node_to_remove)) {
      xml_remove(node_to_remove)
    }

    parent <- xml2::xml_add_child(node, name) #Add to the xml, fine makes sense
    if (is.list(value)) {
      generate_XML_chunk(value, parent)
    } else {
      xml2::xml_text(parent) <-  as.character(value)
    }
  }
}

