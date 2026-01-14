#' Convert a minimum spanning arborescence into a tidy data frame
#'
#' Takes a Chu-Liu/Edmonds arborescence output by [chuliu()] and converts it
#' into a data frame with one row per edge.
#'
#' @param weighted_CLarbor An `igraph` weighted tree output by [chuliu()].
#'
#' @returns A data frame with one row per edge, and with the following columns:
#'   * __Source feature__: The label of the source node of the edge.
#'   * __Target feature__: The label of the target node of the edge.
#'   * __Weight__: The weight of the edge.
#'   * __Dependency__: 1 minus the weight of the edge, corresponding to dependency 
#'   strength in the context of this package.
#' @export
make_CLarbor_df <- function(weighted_CLarbor){

    weighted_CLarbor_df <- cbind(get.edgelist(weighted_CLarbor),E(weighted_CLarbor)$weight)
    weighted_CLarbor_df_weights <- cbind(weighted_CLarbor_df, 1-as.numeric(weighted_CLarbor_df[,3]))
    colnames(weighted_CLarbor_df_weights) <- c("Source feature", "Target feature", "Weight", "Dependency")
    weighted_CLarbor_df_weights
}