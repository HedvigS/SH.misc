#' Creates the complete dependency graph of the features, optionally taking into account groupings of observations
#' @param value_df data-frame where the rows are observations and the columns features. The first column needs to contain a unique identified for each observation. The name of the first column is irrelevant.
#' @param external_variables_df data-frame where the rows are observations and the columns contain meta-data on the observations, such as regions, families etc that group the observations meaningfully. The first column needs to contain a unique identified for each observation. The name of the first column is irrelevant.
#' @author Siva Kalyan and Hedvig Skirgård
#' @value
#' @note We would like to thank Harald Hammarström for his invaluable guidance and support in the creation of these functions. See Hammarström, H., & O’Connor, L. (2013). Dependency-sensitive typological distance. In Approaches to measuring linguistic differences. De Gruyter.

#' @export

#library(infotheo)
#value_df <- read_tsv("../tests/test_that/fixtures/Sahul_structure_wide.tsv")


##this script defines functions that create an asymmetrical dependency matrix.
##The dependency is measured as the conditional mutual information of 2 traits
##divided by the conditional entropy of one of those traits.
##If all the observations for 2 traits are the same (or the same and the rest missing), the dependency will be zero


#tst <- dependency_matrix_cond_MI(value_df = value_df)


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
    full_join(value_df_for_depfun,  by = "ID")

  #This function computes the dependency between two features given the external variables,
  #and only for combinations of trait observations where there isn't missing data.
  depfun <- function(x,y){
    if (x == y){
      1
    } else {
      d <- dfs_joined %>%
        dplyr::select(Var1 = x, Var2 = y, external_variables_united) %>%
        drop_na()
      infotheo::condinformation(d$Var1, d$Var2, d$external_variables_united)/condentropy(d$Var2, d$external_variables_united)
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
