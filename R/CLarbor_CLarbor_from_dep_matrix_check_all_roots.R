CLarbor_from_dep_matrix_check_all_roots <- function(dep_matrix, return.all = F){

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