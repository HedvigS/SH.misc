#' Lists used packages in a set of R-script and generates citation keys.
#'
#' @param input_dir character vector. Name of directory of R-scripts. Will be serched downwards recursively, i.e. also go through sub-directories.
#' @param output_dir character vector. Name of directory to print bibTeX file and citation-keys. Necessary if print_bibtex == TRUE and/or print_tex_citation_string == TRUE.
#' @param print_bibtex logical. If TRUE, a bibTeX file is written with entries for the packages found to be used. File will be written to output_dir as "used_pkgs.bib"
#' @param print_tex_citation_string logical. If TRUE, a txt-file will be written with a string of all the TeX citation keys in a string. This txt-file can be included in a TeX documnet with "\\input{}". It will be in output_dir with the name "citation_keys.txt".
#' @param compare_loaded_with_used logical. If TRUE, the set of packages that the scripts used are compared to those loaded currently in the environment and reported in terminal.
#' @param report_most_used_pkgs logical. If TRUE, the function will report which are the 5 packages you use the most functions from
#' @param report_script_with_most_funs logical. If TRUE, the function will report which script has the most function calls.
#' @return Depending on the arguments, the function returns output to the terminal and/or files written to the output directory.
#' @export
#'
credit_packages <- function(input_dir = NULL,
                            output_dir = NULL,
                            print_bibtex = TRUE,
                            print_tex_citation_string = TRUE,
                            compare_loaded_with_used = TRUE,
                            report_most_used_pkgs = TRUE,
                            report_script_with_most_funs = TRUE,
                            verbose = TRUE
                            ){

#input_dir = "../../Oceanic_computational_ASR/code/"
#output_dir <- "."

r_fns <- list.files(path = input_dir, pattern = "*.[R|r]$", full.names = T, recursive = T)

library(dplyr)
# df to bind to
df <- data.frame("packages" = as.character(),
                 "functions" = as.character(),
                 "scripts" = as.character())

for(fn in r_fns){

    if(verbose == TRUE){
  cat(paste0("I'm on ", fn, "\n"))
    }

    #fn <- r_fns[3]
x <- list.functions.in.file_SH(filename = fn) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("packages") %>%
    dplyr::rename("functions" = 2) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "package:", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "c\\(", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "\\)", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "\\\"", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "character\\(0", "")) %>%
    dplyr::mutate(packages = stringr::str_split(packages, ",")) %>%
    tidyr::unnest(cols = "packages") %>%
    tidyr::unnest(cols = "functions") %>%
    dplyr::mutate(scripts = fn)

df <- dplyr::full_join(x, df, by = c("packages", "functions", "scripts"))
}

used_packages <- df %>%
  dplyr::mutate(used = "TRUE")

# dealing with instances where a package wasn't found. in pipe above this was listed as "" but it should be a proper NA
used_packages <- naniar::replace_with_na(data = used_packages, replace= list(packages = ""))
used_packages$packages <- trimws(used_packages$packages)

#df with loaded packages
if(compare_loaded_with_used == TRUE){

loaded_packages <- data.frame(packages = (.packages())) %>%
  dplyr::mutate(loaded = "TRUE")

joined_df <- dplyr::full_join(used_packages, loaded_packages, by = "packages")

unused_but_loaded <- joined_df %>%
    dplyr::filter(is.na(used)) %>%
    dplyr::filter(!is.na(loaded)) %>%
    dplyr::distinct(packages)

cat("There are ", nrow(unused_but_loaded), "packages that it seems like you're not using, but that are loaded.\n They are: ", unused_but_loaded$packages, ".\n" )

}

if(report_most_used_pkgs  == TRUE){
most_used <- used_packages %>%
    dplyr::group_by(packages) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::filter(packages != ".GlobalEnv") %>%
    dplyr::filter(!is.na(packages))

cat("The top 5 packages you used the most:\n ")
print(as.matrix(
most_used[1:5,]))
}

if(report_script_with_most_funs == TRUE){
script_with_most_functions <-  used_packages %>%
    dplyr::group_by(scripts) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(desc(n))

cat("The top 5 scripts which use the most functions:\n ")
print(as.matrix(
script_with_most_functions [1:5,])
)
}

if(print_bibtex == TRUE){

    output_fn <- paste0(output_dir, "/used_pkgs.bib")

    knitr::write_bib(as.character(unique(most_used$packages)), file = output_fn)

readLines(output_fn) %>%
  stringr::str_replace_all("\\&", "\\\\&") %>% #sorting out issues with ampersand
  stringr::str_replace_all("\\\\\\\\&", "\\\\&") %>%
    writeLines(output_fn)

if(verbose == TRUE){

cat(paste0("Wrote citations for packages you've used to ", output_fn, ".\n There were ", length(!is.na(used_packages$packages %>% unique()))
, " entries.\n" ))
    }
}

#optional part, this generates a text string with the bibtex citation KEYS from earlier that you can then paste into LaTeX in order to cite all

if(print_tex_citation_string == TRUE){
bibdf <- suppressWarnings(bib2df::bib2df(output_fn))

#solution to getting an "and" on the last line from SO
# https://stackoverflow.com/questions/42456452/changing-vector-to-string-special-separator-for-last-element
fPaste <- function(vec) sub(",\\s+([^,]+)$", " and \\1", toString(vec))

vec <- paste0("\\citet{", bibdf$BIBTEXKEY, "}")

fPaste(vec)   %>%
  writeLines(con = paste0(output_dir, "/citation_keys.txt"))
}
}


## Function taken from NCmisc, but removing unique()

library(NCmisc)

list.functions.in.file_SH <- function (filename, alphabetic = TRUE)
{
    if (!file.exists(filename)) {
        stop("couldn't find file ", filename)
    }
    if (!get.ext(filename) == "R") {
        warning("expecting *.R file, will try to proceed")
    }
    tmp <- getParseData(parse(filename, keep.source = TRUE))
    nms <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")]
    funs <- if (alphabetic) {
        sort(nms)
    }
    else {
        nms
    }
    src <- paste(as.vector(sapply(funs, find)))
    outlist <- tapply(funs, factor(src), c)
    return(outlist)
}