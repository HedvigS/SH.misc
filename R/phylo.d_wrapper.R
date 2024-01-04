#' Wrapped function to caper::phylo.d which performs some important sanity checks.
#' @param data A list of the class "comparative.data". Created by `caper::comparative.data`.
#' @param var.name Character vector. The name of the variable in `data` holding the binary variable of interest. This will be passed to phylo.d and matched to the argument "binvar". ' Unlike the `binvar` argument of `phylo.d`, this must be a string, rather than a symbol.
#' @param ... Additional parameters passed to `phylo.d`.
#' @author wrapper: Hedvig Skirgård and Siva Kalyan; original caper::phylo.d-function: Susanne Fritz and David Orme.
#' @returns If no error occurs, the function returns an object of class 'phylo.d'
#' just as `caper::phylo.d`. See `?caper::phylo.d` for more details. If the wrapper-function finds problem, it will trigger error messages.
#' @example phylo.d_wrapper_example.R
#' @export

phylo.d_wrapper <- function(data, var.name, ...) {
    print(match.call())
    if ("binvar" %in% names(match.call())) {
        stop(paste0("Use the 'var.name' argument instead of 'binvar'. Note that 'var.name' must be a string, not a symbol."))
    }

    if (!(var.name %in% colnames(data$data))) {
        stop(paste0("Cannot find a column named '", var.name, "' in the data.\n"))
    }

    #count the proportion of the less frequent value in the column
    min_prop <- data$data[[var.name]] %>%
        table() %>% `/`(sum(.)) %>% min()

    if (min_prop < 0.05) {
        stop(
            paste0(
                "The distribution of tips over the two binary values is too skewed; fewer than 5% of tips are in the minority state. Applying D-estimate calculation to such skewed data can generate unreliable results.\n"
            )
        )
    }

    result <-
        eval(substitute(
            caper::phylo.d(data = data, binvar = this_var, ...),
            list(this_var = as.name(var.name))
        ))

    if (result$Pval0 > 0.05 &
        result$Pval1 > 0.05) {
        stop(
            paste0(
                "Brownian and random simulations are not sufficiently distinct from each other to provide a meaningful D-estimate; observed values appear to be explainable under either model (pval0 > 0.05 & pval1 > 0.05)."
            )
        )
    }


    if (result$Pval0 > 0.05 &
        result$Pval1 < 0.05) {
        cat(
            paste0(
                "Observed values are definitely on the Brownian/clumped end of the spectrum (pval0 > 0.05 & pval1 < 0.05)."
            )
        )
    }
    else if (result$Pval0 < 0.05 &
             result$Pval1 > 0.05) {
        cat(
            paste0(
                "Observed values are definitely on the random/overdispersed end of the spectrum (pval0 < 0.05 & pval1 > 0.05)."
            )
        )
    }
    else if (result$Pval0 < 0.05 &
             result$Pval1 < 0.05) {
        cat(
            paste0(
                "Observed values are definitely between Brownian/clumped and random/over-dispersed (pval0 < 0.05 & pval1 < 0.05)"
            )
        )
    }


    if (result$DEstimate < -7) {
        warning("The resulting D-estimate is lower than 7. This isn't impossible, but unusual.\n")
    }
    else if (result$DEstimate > 7) {
        warning("The resulting D-estimate is higher than 7. This isn't impossible, but unusual.\n")
    }

    result
}