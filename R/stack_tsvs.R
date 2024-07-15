#' Stacks content of multiple tsv-files on-top of each other.
#'
#' @param dir character vector. file.path to directory the function is to search in.
#' @param pattern character vector. Optional, if specified the function will only deal with the tsvs:s that match the pattern. Do not specify "tsv", that is already specified.
#' @param recursive logical. If TRUE, tsvs in dirs inside the dir specified will also be matched.
#' @param range numeric vector specifying a range to limit the vector of file-names to read in by. Default is NULL, i.e. all files are read in. Sometimes if can be desirable to subset the files to test, if so the range can be set to c(1:10) for the first 10 files.
#' @param verbose logical. If TRUE, function will report on which file it is at etc.
#' @return data-frame with content of all tsvs stacked.
#' @note All content will be turned into character to facilitate joining and then the col-types will be converted back to appropriate class for the whole data-frame.
#' @author Hedvig Skirgård.
#' @import dplyr
#' @import purrr
#' @import data.table
#' @import readr
#'

#dir = "../../Glottobank/Grambank/original_sheets/"

stack_tsvs <- function(dir = NULL,
                       pattern = NULL,
                       recursive = FALSE,
                       range = NULL,
                       verbose= TRUE){


  fns <- list.files(path = dir, pattern = paste0(pattern, ".*tsv$"), full.names = T, recursive = recursive)

  if(!is.null(range)){
    fns <-   fns[range]
    }

  if(verbose == TRUE){
    cat("I'm reading in ", length(fns), " files.\n"
      , sep = "")

      }

read_for_map_df <- function(x){
  if(verbose == TRUE){
  cat("I'm at file ", x, ".\n")
  }
  df <- data.table::fread(x ,
                                encoding = 'UTF-8', header = TRUE,
                                fill = TRUE, blank.lines.skip = TRUE,
                                sep = "\t", na.strings = "",
  )   %>%
      dplyr::mutate(across(everything(), as.character))
    df
  }

  All_raw <-    purrr::map_df(.x = fns, .f = read_for_map_df)

All_raw <- suppressMessages(readr::type_convert(All_raw, trim_ws = TRUE, guess_integer = FALSE))


All_raw
}

library(tidyverse)