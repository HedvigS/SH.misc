##this script defines functions that create an asymmetrical dependency matrix.
##The dependency is measured as the conditional mutual information of 2 traits
##divided by the conditional entropy of one of those traits.
##If all the observations for 2 traits are the same (or the same and the rest missing), the dependency will be zero

dependency_matrix_cond_MI <- function(value_df, external_variables_df = tibble(rowname = value_df[[1]], external_variables_united = "null")){
  #This function expects 2 dataframes, one with the observations as rows and traits as columns
  #and the ID for the observations as the first column on the df,
  #the second df should also have the exact same IDs as the first df in the first column,
  #the rest of the columns are filled with meta-information about the observations,
  #such as language family, area, age etc (whatever the user wants to control for).

  #this package contains the functions for computing conditional mutual information and entropy

  #Check if there are any observations that have missing IDs.
  if (sum(is.na(external_variables_df)) > 0) {
    stop("There is missing data in your dataframe for external variables. Please make sure that all rows have complete entries.")
  }

  #Check if any observation IDs are duplicates
  number_of_duplicates <- sum(duplicated(value_df[,1]))
  if (number_of_duplicates > 0){
    stop("Not all of the IDs are unique! Please recheck.")
  }

  #Check if the observation IDs in both dataframes are the same
  if (!all(value_df[,1] == external_variables_df[,1])){
    stop("External variables IDs and value IDs are not the same! Please recheck.")
  }

  #Comibine all the columns with external variables into one column
  external_variables_df_united <- external_variables_df %>%
    unite("external_variables_united", -1)

  #All the values in both dfs need to be factors, not characters, numbers or other types.
  #This is a function that makes all values factors and puts the first column, the IDs, as rownames.
  # Note (SK): This can actually be moved outside.
  make_features_factors <- function (df){
    df %>%
      modify_at(-1, as.factor) %>%
      rename_at(1, function(x){"rowname"}) %>%
      column_to_rownames()
  }


  value_df_for_depfun <- make_features_factors(value_df) %>%
    rownames_to_column("ID")

  external_variables_df_for_depfun <- make_features_factors(external_variables_df_united)

  #For the functions later, we need to store the names of the traits
  #in a separate vector and combine the dataframes
  value_vars <- colnames(value_df_for_depfun)[-1]

  dfs_joined <- external_variables_df_for_depfun %>%
    rownames_to_column("ID") %>%
    full_join(value_df_for_depfun)

  #This function computes the dependency between two features given the external variables,
  #and only for combinations of trait observations where there isn't missing data.
  depfun <- function(x,y){
    if (x == y){
      1
    } else {
      d <- dfs_joined %>%
        dplyr::select(Var1 = x, Var2 = y, external_variables_united) %>%
        drop_na()
      condinformation(d$Var1, d$Var2, d$external_variables_united)/condentropy(d$Var2, d$external_variables_united)
    }
  }

  #This does the dependency computation for all pairs of features, all while showing a nifty progress bar.
  cat("Computing dependency matrix:\n")
  value_vars_grid <- expand.grid(value_vars, value_vars, stringsAsFactors = F)
  pb <- progress_estimated(nrow(value_vars_grid))
  dependencys_vector <- map2_dbl(value_vars_grid$Var1, value_vars_grid$Var2,
                                 function(x,y){
                                   pb$tick()$print()
                                   depfun(x,y)
                                 })
  dependencys <- matrix(pmax(dependencys_vector,0), nrow = length(value_vars), ncol = length(value_vars), dimnames = list(value_vars, value_vars))

  message(paste("\nThe adjacency matrix for the weighted directed graph, the maximum spanning tree, is now created. There were", sum(is.nan(dependencys)), "cells with missing dependency values. These are the result of too few observations given the pair of features."))

  dependencys[is.nan(dependencys)] <- 0

  dependencys
}

###Function for computing the Chu-Liu tree given the dependency matrix, when the root is known.

CLarbor_from_dep_matrix_known_root <- function(dep_matrix, root){
#  source("chuliu.R") #This script contains the Chu-Liu/Edmonds algorithm. There are also functions in the optrees package that does this (msArborEdmonds()), but in this case this one is better because it's more transperent and more talky. It takes a long time to run, so verobosity is preferable.

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

make_CLarbor_df <- function(weighted_CLarbor){

  weighted_CLarbor_df <- cbind(get.edgelist(weighted_CLarbor),E(weighted_CLarbor)$weight)
  weighted_CLarbor_df_weights <- cbind(weighted_CLarbor_df, 1-as.numeric(weighted_CLarbor_df[,3]))
  colnames(weighted_CLarbor_df_weights) <- c("Source feature", "Target feature", "Weight", "Dependency")
  weighted_CLarbor_df_weights
}