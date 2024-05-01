
make_CLarbor_df <- function(weighted_CLarbor){

    weighted_CLarbor_df <- cbind(get.edgelist(weighted_CLarbor),E(weighted_CLarbor)$weight)
    weighted_CLarbor_df_weights <- cbind(weighted_CLarbor_df, 1-as.numeric(weighted_CLarbor_df[,3]))
    colnames(weighted_CLarbor_df_weights) <- c("Source feature", "Target feature", "Weight", "Dependency")
    weighted_CLarbor_df_weights
}