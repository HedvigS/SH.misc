#' Fetch text content from internet or other location and write to new location.
#' 
#' @param url character vector. File-path or url pointing to a text file.
#' @param out_dir character vector. File-path of folder where the file is to be copied to
#' @param add_date_fetch logical. If TRUE, the file will have an added line to the top that indicates when it was fetched and from where
#' @param check_exists logical. If TRUE, the function will check if the file already exists at the out_dir, and if so not proceed
#' @param comment_char character vector. Specifies character used for comments in code. Defaults to '#'.
#' @author Hedvig Skirgård
#' @examples
#' # fetch_lines(url = "https://github.com/HedvigS/R_grambank_cookbook/raw/main/functions/make_theo_scores.R", out_dir = ".")
#' 

fetch_lines <- function(url = NULL,
                       out_dir = NULL, 
                       add_date_fetched = TRUE, 
                       check_exists = TRUE, 
                       comment_char = "#"){

out_fn <- file.path(out_dir, basename(url))  

if(file.exists(out_fn) & check_exists == TRUE) {
  stop("File already exists. Delete file or set `check_exists` to FALSE if you want to fetch again.")
}

lines <- readLines(url, warn = F)
  
if(add_date_fetched == TRUE){
  date_line <- paste0(comment_char, " This file was fetched at ",
                      date(),
                      " from ", url, ".\n") 
  
  lines <- c(date_line, lines)
}  
  writeLines(text = lines, out_fn)
}
