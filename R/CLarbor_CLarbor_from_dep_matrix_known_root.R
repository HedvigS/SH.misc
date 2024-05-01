#' Prunes the dependency graph according to the Chu-Liu/Edmonds algorithm given a particular node as the root.
#' @param dep_matrix the output of the function dependency_matrix_cond_MI()
#' @param roor .
#' @author Siva Kalyan and Hedvig Skirgård
#' @value
#' @note We would like to thank Harald Hammarström for his invaluable guidance and support in the creation of these functions. See Hammarström, H., & O’Connor, L. (2013). Dependency-sensitive typological distance. In Approaches to measuring linguistic differences. De Gruyter.
#' @export


CLarbor_from_dep_matrix_known_root <- function(dep_matrix = NULL root){
  source("chuliu.R") #This script contains the Chu-Liu/Edmonds algorithm. There are also functions in the optrees package that does this (msArborEdmonds()), but in this case this one is better because it's more transperent and more talky. It takes a long time to run, so verobosity is preferable.


    if(dep_matrix == NULL){
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
  pruned_weights <- data.frame(feature = get.edgelist(CLarbor)[,2],weight = E(CLarbor)$weight) %>%
    distinct()
  redundant_percentage<- 100*sum(1 - pruned_weights$weight)/length(pruned_weights$weight)
  message(paste("Your dataset contains", round(redundant_percentage,2), "% mass that can be considered as redundant."))
  list(tree = CLarbor, redundant = redundant_percentage) #return both the Chu-Liu tree as well as how much of the dependencies it accounts for.
}