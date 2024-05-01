#' Applies the function CLarbor::from_dep_matrix_known_root to each node in the graph, then evaluates which one was indeed the best root.
#' @param dep_matrix the output of the function dependency_matrix_cond_MI()
#' @param return.all logical. If TRUE, all possible trees/forests are returend. If FALSE, only the best is returned. Defaults to FALSE.
#' @author Siva Kalyan and Hedvig Skirgård
#' @value
#' @note We would like to thank Harald Hammarström for his invaluable guidance and support in the creation of these functions. See Hammarström, H., & O’Connor, L. (2013). Dependency-sensitive typological distance. In Approaches to measuring linguistic differences. De Gruyter.
#' @export

CLarbor_from_dep_matrix_check_all_roots <- function(dep_matrix = NULL, return.all = FALSE){

if(dep_matrix == NULL){
    stop("No dependency matrix supplied.")
}

      value_vars <- colnames(dep_matrix)
  plan(multicore)
  all_CLarbor_results <- future_map(value_vars, function(r){CLarbor_from_dep_matrix_known_root(dep_matrix, r)}) %>%
    transpose()
  plan(sequential)
  which_best_CLarbor <- which.max(unlist(all_CLarbor_results$redundant))
  result <- list(all.arbors = all_CLarbor_results, best.arbor = all_CLarbor_results$tree[[which_best_CLarbor]], root = value_vars[which_best_CLarbor])
  if (return.all){
    result
  } else {
    result[[c("best.arbor", "root")]]
  }
}