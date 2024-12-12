#' Prepare a tree (or trees) for EpiFusion
#'
#' This function prepares a tree or tree posterior, given as an S3 phylo object, for EpiFusion analysis by adding 'time during outbreak' to the node and leaf labels. You must provide an index date (in date form) which is day `0` of your analysis, i.e. the earliest point from which you will model trajectories.
#'
#' @param tree phylogenetic tree or tree posterior with branch lengths in terms of years (in S3 Phylo Object format)
#' @param index_date analysis index date (day 0 of your analysis)
#' @param final_sequence_date date of sampling of the final sequence in the tree or tree posterior
#' @param treefile_path path to the file where the processed tree should be stored (default `processedtree.tree`)
#' @importFrom ape write.tree
#' @importFrom castor get_all_distances_to_root
#' @return a tree (in S3 Phylo Object format) with the node and leaf labels in terms of time from the index date. If a tree posterior is passed to the function, nothing is returned to the function and the trees are simply written to the `treefile_path`
#' @export


prepare_epifusion_tree <- function(tree, index_date, final_sequence_date, treefile_path = "processedtree.tree") {
  if (class(tree) == "phylo") { #this means the tree is a single, fixed tree
    all_distances <- (get_all_distances_to_root(tree)*365) + rnorm(length(get_all_distances_to_root(tree)), 0.001, 0.001)
    offset <- abs(rnorm(1, mean = 0, sd = 0.001))
    root <- as.numeric(final_sequence_date - index_date) - max(all_distances)
    node_distances <- all_distances[(length(tree$tip.label)+1):(length(tree$tip.label)+tree$Nnode)]
    tip_distances <- all_distances[1:length(tree$tip.label)]
    tree$node.label <- paste0("node[", (node_distances + root - offset), "]")
    tree$tip.label <- paste0("leaf[", (tip_distances + root - offset), "]")
    write.tree(tree, treefile_path)
    return(tree)
  } else if (class(tree) == "multiPhylo") { #this means we are working with a tree posterior
    trees <- tree
    treefile <- file(treefile_path, "w")
    offset <- abs(rnorm(1, mean = 0, sd = 0.00001))
    for (i in 1:length(trees)) {
      tree <- trees[[i]]
      all_distances_plain <- (get_all_distances_to_root(tree)*365)
      root <- as.numeric(final_sequence_date - index_date) - max(all_distances_plain)
      if (root < 0) {
        stop("ERROR: A tree sampled from the posterior has a root node that is earlier than your index date!")
      }
      node_raw_distances <- all_distances_plain[(length(tree$tip.label)+1):(length(tree$tip.label)+tree$Nnode)] + root - offset
      tip_raw_distances <- all_distances_plain[1:length(tree$tip.label)] + root - offset
      node_distances <- node_raw_distances + rnorm(length(node_raw_distances), 0, 0.001)

      tip_max_distances <- ceiling(tip_raw_distances) - abs(rnorm(length(tip_raw_distances), 0, 0.001))
      tip_distances_jittered <- tip_raw_distances + abs(rnorm(length(tip_raw_distances), 0, 0.001))
      tip_distances <- pmin(tip_distances_jittered, tip_max_distances)
      tree$node.label <- paste0("node[", (node_distances), "]")
      tree$tip.label <- paste0("leaf[", (tip_distances), "]")
      writeLines(write.tree(tree), con = treefile)
    }
    on.exit(close(treefile))
  } else {
    stop("ERROR: Unable to read the tree - please ensure you have passed a phylo or multiPhylo object.")
  }
}




