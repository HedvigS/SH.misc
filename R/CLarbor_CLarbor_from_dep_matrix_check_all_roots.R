#' Find the maximum spanning arborescence over a graph of feature dependencies
#'
#' Applies the function [CLarbor_from_dep_matrix_known_root()] over every
#' possible choice of root, and returns either the best result or all results.
#' If multiple cores are available on the user's device, the function is applied
#' in parallel over different roots.
#'
#' @param dep_matrix A square numeric matrix of pairwise dependency strengths,
#'   output by [dependency_matrix_cond_MI()].
#' @param return.all A logical value indicating whether to return all possible
#'   trees/forests, or just the best one. Defaults to FALSE.
#' @returns A list consisting of up to three elements:
#'   * `all.arbors`: The full set of maximum spanning arborescences, one for each
#'   possible root node. Only returned when `return.all` is TRUE.
#'   * `best.arbor`: The optimal maximum spanning arborescence among all possible
#'   choices of root node, as determined by the `redundant` attribute of the
#'   output of [CLarbor_from_dep_matrix_known_root()].
#'   * `root`: The index of the optimal root node.
#' @references Hammarström, H., & O’Connor, L. (2013). Dependency-sensitive
#'   typological distance. In Lars Borin & Anju Saxena (eds.) Approaches to
#'   measuring linguistic differences. De Gruyter.
#' @export

CLarbor_from_dep_matrix_check_all_roots <- function(dep_matrix = NULL, return.all = FALSE){
  
  if(is.null(dep_matrix)){
    stop("No dependency matrix supplied.")
  }
  
  value_vars <- colnames(dep_matrix)
  future::plan(future::multicore)
  all_CLarbor_results <- furrr::future_map(value_vars, function(r){CLarbor_from_dep_matrix_known_root(dep_matrix, r)},
                                           .options = furrr::furrr_options(seed = T)) %>%
    purrr::transpose()
  future::plan(future::sequential)
  which_best_CLarbor <- which.max(unlist(all_CLarbor_results$redundant))
  result <- list(all.arbors = all_CLarbor_results, best.arbor = all_CLarbor_results$tree[[which_best_CLarbor]], root = value_vars[which_best_CLarbor])
  if (return.all){
    result
  } else {
    result[[c("best.arbor", "root")]]
  }
}