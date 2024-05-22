#' Takes a vector of filenames, reads in (assuming tsv) and stackrs them all on top of each other, aligning columns where they have the same name. Unique columns are kept.
#'
#' @param make_all_charinput_dir logical. Unless specified, all columns will be treated as character vectors
#' @param fn_col_basename logical. If set to TRUE, the column with filenames will be basename. If FALSE, the full relative filepath will be used.
#' @return Data-frame with all tables stacked.
#' @export
#'

stack_tsvs <- function(fns = NULL, make_all_char = TRUE, fn_col_basename = FALSE){

df <- fns %>%
  map_df(
    function(x) data.table::fread(x ,
                                  encoding = 'UTF-8', header = TRUE,
                                  fill = TRUE, blank.lines.skip = TRUE,
                                  sep = "\t", na.strings = "",
    )   %>%
    mutate(across(everything(), as.character)) %>%
        mutate(filename = x)
    )

    if(make_all_char == FALSE){
    df <- type_convert(df)
    }

    if(fn_col_basename == TRUE){
    df$filename <- basename(df$filename)
    }


df
}