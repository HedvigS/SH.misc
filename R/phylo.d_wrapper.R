#' Wrapped function to caper::phylo.d which performs some important sanity checks.
#'
#' @param data A list of the class "comparative.data". Created by caper::comparative.data
#' @param binvar Character vector of length one. Name of one of the columns in the data-frame inside the comparative.data-list.
#' @returns If no error occurs, the function returns an object of class 'phylo.d' just as caper::phylo.d. See ?caper.phylo.d for more details.
#' @example phylo.d_wrapper_example.R
#' @export

phylo.d_wrapper <- function(data, binvar){

#  data <- comp_data
#  binvar = "cluster"

 if(!(binvar %in%    colnames(data$data))){
    message <- paste0("Binvar not column in data.\n")
    stop(message)
 }

counts <- data$data[[binvar]] %>% table() %>% as.matrix()

#count the percentage of the smallest value out of the sum
min_percent <- min(counts[1], counts[2]) / (counts[1] + counts[2])

if(min_percent < 0.05){
 message <- paste0("The distribution of tips over the two binary values is very skewed, fewer than 5% of tips are in the minority state. Applying D-estimate caluclation to so skewed data can generate unreliable results.\n"

                   )
 stop(message)
 }


  result <- eval(substitute(caper::phylo.d(data = comp_data, binvar = this_var), list(this_var=as.name(binvar))))


if(  result$Pval0 > 0.05 &
  result$Pval1 > 0.05){

  message <- paste0("Brownian and random simulations are not suﬀiciently distinct from each
other to get a meaningful D-estimate, observed values appear to be similar
to both (pval0 > 0.05 & pval1 > 0.05).")
stop(message)
}


  if(  result$Pval0 > 0.05 &
       result$Pval1 < 0.05){
      message <- paste0("Observed values definitely on the Brownian/clumped end of the spectrum
(pval0 > 0.05 & pval1 < 0.05).")
    cat(message)}

    if(  result$Pval0 < 0.05 &
         result$Pval1 > 0.05){
      message <- paste0("Observed values definitely on the random/overdispersed end of the spectrum
(pval0 < 0.05 & pval1 > 0.05).")
      cat(message)
    }

    if(  result$Pval0 < 0.05 &
         result$Pval1 < 0.05){

  message <-    paste0(" Observed values definitely between Brownian/clumped and random/over-
        dispersed (pval0 < 0.05 & pval1 < 0.05)")
}


  if(result$DEstimate < -7){
message <- paste0("The resulting D-estimate is lower than 7. This isn't impossible, but unusual.")
warning(message)
  }

  if(result$DEstimate > 7){
    message <- paste0("The resulting D-estimate is higher than 7. This isn't impossible, but unusual.")
    warning(message)
  }

result

}