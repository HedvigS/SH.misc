#' Lists used packages in a set of R-script and generates citation keys.
#'
#' @param input_dir character vector. Name of directory of R-scripts. Will be serched downwards recursively, i.e. also go through sub-directories.
#' @param pkgs_vec character vector. Names of R-packages, either instead of input_dir or in addition. Sometimes some packages aren't missed when searching through the scripts, if you add their names here they'll be included in the output.
#' @param output_dir character vector. Name of directory to print bibTeX file and citation-keys. Necessary if print_bibTeX == TRUE and/or print_tex_citation_string == TRUE.
#' @param print_bibTeX logical. If TRUE, a bibTeX file is written with entries for the packages found to be used. File will be written to output_dir as "used_pkgs.bib"
#' @param print_LaTeX_table logical. If TRUE, a LaTeX table will be rendered with each pagkage as a row and a column for version loaded.
#' @param print_df logical. If TRUE, a table of packages will be printed to a tsv-file in the output_dir, with a column for version loaded.
#' @param print_tex_citation_string logical. If TRUE, a txt-file will be written with a string of all the TeX citation keys in a string. This txt-file can be included in a TeX document with "\\input{}". It will be in output_dir with the name "citation_keys.txt".
#' @param compare_loaded_with_used logical. If TRUE, the set of packages that the scripts used are compared to those loaded currently in the environment and reported in terminal.
#' @param report_most_used_pkgs logical. If TRUE, the function will report which are the 5 packages you use the most functions from to the terminal.
#' @param report_script_with_most_funs logical. If TRUE, the function will report which script has the most function calls to the terminal.
#' @param verbose logical. If TRUE, the function will be more talkative.
#' @return Data-frame of all used functions. Depending on the arguments, the function also returns output to the terminal and/or files written to the output directory.
#' @import dplyr
#' @import magrittr
#' @import tidyr
#' @import naniar
#' @import xtable
#' @note In cases where it is not clear which specific package a function is from several packages are returned for that function. This is for example the case with filter().
#' @export
#'
credit_packages <- function(input_dir = NULL,
                            pkgs_vec = NULL,
                            output_dir = NULL,
                            print_bibTeX = TRUE,
                            print_tex_citation_string = TRUE,
                            compare_loaded_with_used = TRUE,
                            report_most_used_pkgs = TRUE,
                            print_LaTeX_table = TRUE,
                            print_df = TRUE,
                            report_script_with_most_funs = TRUE,
                            verbose = TRUE
                            ){

#input_dir = "../../Oceanic_computational_ASR/code/"
#output_dir <- "."

if(all(is.null(input_dir), is.null(pkgs_vec))){
    stop("Neither input_dir nor pkgs_vec has been supplied.")
}

    if(is.null(output_dir)){
        stop("output_dir not supplied.")
    }



    if(!is.null(input_dir)){

r_fns <- list.files(path = input_dir, pattern = "*.[R|r]$", full.names = T, recursive = T)

# df to bind to
df <- data.frame("packages" = as.character(),
                 "functions" = as.character(),
                 "scripts" = as.character())

for(fn in r_fns){

    if(verbose == TRUE){
  cat(paste0("I'm on ", fn, "\n"))
    }

    #fn <- r_fns[3]
x <- .list.functions.in.file_SH(filename = fn) %>%
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

used_packages <- used_packages %>%
    dplyr::filter(packages != ".GlobalEnv") %>%
    dplyr::filter(!is.na(packages))

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
        dplyr::arrange(desc(n))

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
    }

    if(is.null(input_dir) & !is.null(pkgs_vec)){
        pkgs_to_cite <- pkgs_vec %>% unique()
            }
    if(!is.null(input_dir) & !is.null(pkgs_vec)){
        pkgs_to_cite <- unique(c(used_packages$packages, pkgs_vec))
    }

    if(!is.null(input_dir) & is.null(pkgs_vec)){
        pkgs_to_cite <- unique(c(used_packages$packages))
    }




if(print_bibTeX == TRUE){

    output_fn <- paste0(output_dir, "/used_pkgs.bib")

    knitr::write_bib(as.character(pkgs_to_cite), file = output_fn)

readLines(output_fn) %>%
  stringr::str_replace_all("\\&", "\\\\&") %>% #sorting out issues with ampersand
  stringr::str_replace_all("\\\\\\\\&", "\\\\&") %>%
    writeLines(output_fn)

##adding in citation for R itself

#https://stackoverflow.com/questions/46179997/r-missing-bib-key-in-citation-output
R_bib = citation()
R_bib$key = paste0("R_", R.version$major, ".", R.version$minor)
R_bib$note <- R.version.string
R_bib_string <- utils::toBibtex(R_bib)

bib <- readLines(output_fn)
bib <- c(bib, R_bib_string)
bib %>% write_lines(output_fn)

if(verbose == TRUE){

cat(paste0(
        "There were ", length(pkgs_to_cite)
, " packages to cite.\n" ))

cat(paste0("Wrote ", output_fn, ".\n"))
    }
}

#optional part, this generates a text string with the bibTeX citation KEYS from earlier that you can then paste into LaTeX in order to cite all

if(print_tex_citation_string == TRUE){
bibdf <- suppressWarnings(bib2df::bib2df(output_fn))

#solution to getting an "and" on the last line from SO
# https://stackoverflow.com/questions/42456452/changing-vector-to-string-special-separator-for-last-element
fPaste <- function(vec) sub(",\\s+([^,]+)$", " and \\1", toString(vec))

vec <- paste0("\\citet{", bibdf$BIBTEXKEY, "}")

fPaste(vec)   %>%
  writeLines(con = paste0(output_dir, "/citation_keys.txt"))

if(verbose == TRUE){
    cat(paste0("Wrote ", output_dir, "/citation_keys.txt") )
}

}

    pkgs_to_cite_df <- installed.packages()[pkgs_to_cite, "Version"] %>%
        as.data.frame() %>%
        rownames_to_column("Package") %>%
        rename("Version" = "." )

    if(print_df == TRUE ){
    pkgs_to_cite_df %>%
        write_tsv(paste0(output_dir,"/pkgs_versions_table.tsv"))

        if(verbose == TRUE){
            cat(paste0("Wrote ", output_dir,"/pkgs_versions_table.tsv \n"))
        }



    }

    if(print_LaTeX_table == TRUE ){

        fn_out = paste0(output_dir, "/used_packages_table.tex")
        cap <- "Table of R-packages used."
        lbl <- "r_package_table"
        align <- c("r","p{2.5cm}","p{2.5cm}")

        pkgs_to_cite_df %>%
            xtable::xtable(caption = cap, label = lbl,
                   align = align) %>%
            xtable::print.xtable(file = fn_out,
                                 sanitize.colnames.function = function(x){x},
                                 sanitize.text.function = function(x){x},
                                 include.rownames = FALSE, math.style.negative = F,tabular.environment = "longtable",
                                 booktabs = TRUE, floating = F)

        if(verbose == TRUE){
            cat(paste0("Wrote ", fn_out, "\n" ))
        }


    }


}


.list.functions.in.file_SH <- function (filename, alphabetic = TRUE)
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

