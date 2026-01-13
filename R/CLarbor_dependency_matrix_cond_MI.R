#' Create a matrix of directional dependencies between pairs of features
#'
#' Computes a directional measure of dependency between every pair of features
#' in a dataset, optionally controlling for one or more grouping variables.
#' Features are assumed to be discrete.
#'
#' Following Hammarström & O'Connor (2013: 337), we measure the strength of the
#' dependency from \eqn{A} to \eqn{B} by computing how precisely feature \eqn{B}
#' can be predicted from the value of feature \eqn{A}. We do this by measuring
#'
#' \deqn{\frac{MI(A,B)}{H(B)},}
#'
#' where \eqn{MI(A,B)} is the mutual information of \eqn{A} and \eqn{B}, and
#' \eqn{H(B)} is the Shannon entropy of \eqn{B}. This is a value that ranges from
#' \eqn{0} (\eqn{A} is of no use in predicting \eqn{B}) to \eqn{1} (\eqn{A} perfectly predicts \eqn{B}).
#' We allow the user to specify one or more grouping variables (typically
#' language family, area, etc.) which are then combined into a single grouping
#' variable \eqn{C}. In this case, we compute dependency strength as
#'
#' \deqn{\frac{MI(A,B|C)}{H(B|C)}.}
#'
#' In either case, we use [infotheo::condinformation()] for (conditional) mutual
#' information, and [infotheo::condentropy()] for (conditional) entropy.
#'
#' @param value_df Data frame where the rows are observations and the (second
#'   and subsequent) columns are features. The first column needs to contain a
#'   unique identifier for each observation, but the name of this column is
#'   irrelevant. The remaining columns should all be discrete, as they will be
#'   converted into factors.
#' @param obs_group_df Optional data frame where the rows are observations and
#'   the (second and subsequent) columns contain metadata on the observations
#'   (such as regions, families etc.) that group the observations meaningfully.
#'   The first column needs to contain a unique identifier for each observation,
#'   but the name of this column is irrelevant. The identifiers in this data
#'   frame should be the same as the ones in `value_df`.
#'
#'   The default value of this argument is a data frame whose first column is
#'   the same as that of `value_df`, and whose second column is an unvarying
#'   "null"; conditioning by this column is equivalent to not conditioning at
#'   all.
#' @returns A square matrix whose side length is equal to the number of features
#'   in `value_df` and whose values give the strength of the dependency between
#'   each (ordered) pair of features.
#' @references Hammarström, H., & O’Connor, L. (2013). Dependency-sensitive
#'   typological distance. In Lars Borin & Anju Saxena (eds.) Approaches to
#'   measuring linguistic differences. De Gruyter.
#' @export
#' 
dependency_matrix_cond_MI <- function(value_df, 
                                      obs_group_df = tibble(rowname = value_df[[1]],
                                                            external_variables_united = "null")){
  #This function expects 2 dataframes, one with the observations as rows and traits as columns
  #and the ID for the observations as the first column on the df,
  #the second df should also have the exact same IDs as the first df in the first column,
  #the rest of the columns are filled with meta-information about the observations,
  #such as language family, area, age etc (whatever the user wants to control for).
  
  #this package contains the functions for computing conditional mutual information and entropy
  
  #Check if there are any observations that have missing IDs.
  if (sum(is.na(obs_group_df)) > 0) {
    stop("There is missing data in your dataframe for external variables. Please make sure that all rows have complete entries.")
  }
  
  #Check if any observation IDs are duplicates
  number_of_duplicates <- sum(duplicated(value_df[,1]))
  if (number_of_duplicates > 0){
    stop("Not all of the IDs are unique! Please recheck.")
  }
  
  #Check if the observation IDs in both dataframes are the same
  if (!all(value_df[,1] == obs_group_df[,1])){
    stop("External variables IDs and value IDs are not the same! Please recheck.")
  }
  
  #Comibine all the columns with external variables into one column
  obs_group_df_united <- obs_group_df %>%
    tidyr::unite("external_variables_united", -1)
  
  #All the values in both dfs need to be factors, not characters, numbers or other types.
  #This is a function that makes all values factors and puts the first column, the IDs, as rownames.
  # Note (SK): This can actually be moved outside.
  make_features_factors <- function (df){
    df %>%
      purrr::modify_at(-1, as.factor) %>%
      rename_at(1, function(x){"rowname"}) %>%
      tibble::column_to_rownames()
  }
  
  
  value_df_for_depfun <- make_features_factors(value_df) %>%
    tibble::rownames_to_column("ID")
  
  obs_group_df_for_depfun <- make_features_factors(obs_group_df_united)
  
  #For the functions later, we need to store the names of the traits
  #in a separate vector and combine the dataframes
  value_vars <- colnames(value_df_for_depfun)[-1]
  
  dfs_joined <- obs_group_df_for_depfun %>%
    tibble::rownames_to_column("ID") %>%
    full_join(value_df_for_depfun,  by = "ID")
  
  #This function computes the dependency between two features given the external variables,
  #and only for combinations of trait observations where there isn't missing data.
  depfun <- function(x,y){
    if (x == y){
      1
    } else {
      d <- dfs_joined %>%
        dplyr::select(Var1 = x, Var2 = y, external_variables_united) %>%
        tidyr::drop_na()
      infotheo::condinformation(d$Var1, d$Var2, d$external_variables_united)/infotheo::condentropy(d$Var2, d$external_variables_united)
    }
  }
  
  #This does the dependency computation for all pairs of features, all while showing a nifty progress bar.
  cat("Computing dependency matrix:\n")
  value_vars_grid <- expand.grid(value_vars, value_vars, stringsAsFactors = F)
  pb <- dplyr::progress_estimated(nrow(value_vars_grid))
  dependencys_vector <- purrr::map2_dbl(value_vars_grid$Var1, value_vars_grid$Var2,
                                        function(x,y){
                                          pb$tick()$print()
                                          depfun(x,y)
                                        })
  dependencys <- matrix(pmax(dependencys_vector,0), nrow = length(value_vars), ncol = length(value_vars), dimnames = list(value_vars, value_vars))
  
  message(paste("\nThe adjacency matrix for the weighted directed graph, the maximum spanning tree, is now created. There were", sum(is.nan(dependencys)), "cells with missing dependency values. These are the result of too few observations given the pair of features."))
  
  dependencys[is.nan(dependencys)] <- 0
  
  dependencys
}
