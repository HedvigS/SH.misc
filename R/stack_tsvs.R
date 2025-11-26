#' Stacks content of multiple tsv-files on-top of each other.
#'
#' @param fns character vector of file names.
#' @param verbose logical. If TRUE, function will report on which file it is at etc.
#' @param delim character vector for delim, defaults to "\t".
#' @return data-frame with content of all tsvs stacked and a column with filename.
#' @note All content will be turned into character to facilitate joining and then the col-types will be converted back to appropriate class for the whole data-frame.
#' @author Hedvig Skirgård.
#' @importFrom dplyr mutate
#' @importFrom purrr map_df
#' @importFrom readr type_convert
#' @import readr
#'

#dir = "../../Glottobank/Grambank/original_sheets/"

stack_delim <- function(fns = fns,
                       verbose= TRUE, delim = "\t"){



    if(verbose == TRUE){
        cat("I'm reading in ", length(fns), " files.\n"
            , sep = "")

    }

    read_for_map_df <- function(x){
        if(verbose == TRUE){
            cat("I'm at file ", x, ".\n")
        }
        df <- read.delim(file = x, sep = delim, header = TRUE, encoding = 'UTF-8', na.strings = "") %>%
            #    data.table::fread(x ,
            #                                encoding = 'UTF-8', header = TRUE,
            #                                fill = TRUE, blank.lines.skip = TRUE,
            #                                sep = "\t", na.strings = "",
            # ) %>%

            dplyr::mutate(across(everything(), as.character)) %>%
            dplyr::mutate(filename =x)
        df
    }

    All_raw <-    purrr::map_df(.x = fns, .f = read_for_map_df)

    All_raw <- suppressMessages(readr::type_convert(All_raw, trim_ws = TRUE, guess_integer = FALSE))

    All_raw
}
