#' Stacks content of multiple tsv-files on-top of each other.
#'
#' @param dir character vector. file.path to directory the function is to search in.
#' @param pattern character vector. Optional, if specified the function will only deal with the tsvs:s that match the pattern. Do not specify "tsv", that is already specified.
#' @param recursive logical. If TRUE, tsvs in dirs inside the dir specified will also be matched.
#' @return data-frame with content of all tsvs stacked and a column with filename. 
#' @note All content will be turned into character to facilitate joining and then the col-types will be converted back to appropriate class for the whole data-frame.
#' @author Hedvig Skirgård.
#' @import dplyr
#' @import purrr
#' @import data.table
#' @import readr
#' 

dir = "../../Glottobank/Grambank/original_sheets/"
pattern = "HS"

stack_tsvs <- function(dir = NULL, 
                       pattern = NULL, 
                       recursive = recursive){


  fns <- list.files(path = dir, pattern = paste0(pattern, ".*tsv$"), full.names = T, recursive = recursive)
  

  All_raw <- fns %>% 
    purrr::map_df(
      function(x) data.table::fread(x ,
                                    encoding = 'UTF-8', header = TRUE, 
                                    fill = TRUE, blank.lines.skip = TRUE,
                                    sep = "\t", na.strings = "",
      )   %>% 
        dplyr::mutate(across(everything(), as.character)) %>% 
        dplyr::mutate(filename =x)
               
      
    ) 
  
All_raw <- suppressMessages(readr::type_convert(All_raw, trim_ws = TRUE, guess_integer = FALSE))
  

All_raw  
}
