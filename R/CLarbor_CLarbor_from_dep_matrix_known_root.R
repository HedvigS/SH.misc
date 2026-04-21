#' Find the maximum spanning arborescence over a graph of feature dependencies,
#' given a root node
#'
#' Prunes a dependency graph according to the Chu-Liu/Edmonds algorithm given a
#' particular node as the root.
#'
#' @param dep_matrix A square numeric matrix of pairwise dependency strengths,
#'   output by [dependency_matrix_cond_MI()].
#' @param root An integer giving the index of the root node. Must be between 1
#'   and the number of rows/columns of `dep_matrix`.
#' @returns A list with two elements:
#'
#'   * `tree`: An `igraph` tree representing the maximum spanning arborescence
#'   over the dependency graph defined by `dep_matrix`, with the root at the
#'   node with index `root`.
#'   * `redundant`: The average percentage of the entropy of each node that is
#'   accounted for by its incoming dependency.
#' @references Hammarström, H., & O’Connor, L. (2013). Dependency-sensitive
#'   typological distance. In Lars Borin & Anju Saxena (eds.) Approaches to
#'   measuring linguistic differences. De Gruyter.
#' @seealso [CLarbor_from_dep_matrix_check_all_roots()] for a version that
#'   checks all possible roots and returns the result with the highest
#'   `redundant` value.
#' @export

CLarbor_from_dep_matrix_known_root <- function(dep_matrix = NULL, root){
  if(is.null(dep_matrix)){
    stop("No dependency matrix supplied.")
  }
  
  
  value_vars <- colnames(dep_matrix)
  
  if(is.character(root)) {
    root <- which(value_vars == root)
  }
  
  G <- 1 - dep_matrix
  message(paste0(round(100*(sum(G == 0) - nrow(G))/(length(G) - nrow(G)),2), "% of the dependencies are perfect. The perfect dependencies will be replace them with epsilons so that the edge still exists. If there were no perfect dependencies, nothing happens."))
  G[G == 0] <- .Machine$double.eps
  
  #a epsilon needs to be added because in cases where a trait is entirely predictable by another trait, the weight of the edge would be zero and for igraph that means that there isn't an edge at all, and the Chu-Liu/Edmons algorithm needs to be set loose on a completely connected graph. So, we introduce a tiny tiny weight to all edges in order to make sure that we get a connected network.
  
  diag(G) <- 0
  dimnames(G) <- NULL
  CLarbor <- chuliu(G,root)
  V(CLarbor)$name <- value_vars
  pruned_weights <- data.frame(feature = as_edgelist(CLarbor)[,2],weight = E(CLarbor)$weight) %>%
    distinct()
  redundant_percentage<- 100*sum(1 - pruned_weights$weight)/length(pruned_weights$weight)
  message(paste("Your dataset contains", round(redundant_percentage,2), "% mass that can be considered as redundant."))
  list(tree = CLarbor, redundant = redundant_percentage) #return both the Chu-Liu tree as well as how much of the dependencies it accounts for.
}