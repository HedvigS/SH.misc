#' Lists used packages in a set of R-script and generates citation keys, bibtex-file, tables etc.
#'
#' @param fns character vector of file-names to R-scripts to read in.
#' @param pkgs_vec_manual character vector. Names of R-packages, either instead of fns or in addition to it. Sometimes some packages are missed when searching through the scripts, if you add their names here they'll be included in the output despite this. You can also set fns to NULL and only use this argument to generate the references etc.citation
#' @param output_dir character vector. Name of directory to print bibTeX file and citation-keys. Necessary if print_bibTeX == TRUE and/or print_tex_citation_string == TRUE.
#' @param print_bibTeX logical. If TRUE, a bibTeX file is written with entries for the packages found to be used. File will be written to output_dir as "used_pkgs.bib"
#' @param print_LaTeX_table logical. If TRUE, a LaTeX table will be rendered with each pagkage as a row and a column for version loaded.
#' @param print_tsv logical. If TRUE, a table of packages will be printed to a tsv-file in the output_dir, with a column for version loaded.
#' @param print_tex_citation_string logical. If TRUE, a txt-file will be written with a string of all the TeX citation keys in a string. This txt-file can be included in a TeX document with "\\input{}". It will be in output_dir with the name "citation_keys.txt".
#' @param compare_loaded_with_used logical. If TRUE, the set of packages that the scripts used are compared to those loaded currently in the environment and reported in terminal.
#' @param report_most_used_pkgs logical. If TRUE, the function will report which are the 5 packages you use the most functions from to the terminal.
#' @param report_script_with_most_funs logical. If TRUE, the function will report which script has the most function calls to the terminal.
#' @param verbose logical. If TRUE, the function will be more talkative.
#' @return Data-frame of all used functions. Depending on the arguments, the function also returns output to the terminal and/or files written to the output directory.
#' @author Hedvig Skirgård
#' @import dplyr (>= 1.1.4)
#' @import bib2df (>= 1.1.1)
#' @import tibble (>= 1.8-4)
#' @import knitr
#' @import magrittr
#' @import tidyr
#' @importFrom get.ext reader (>= 1.0.6)
#' @import xtable (>= 1.8-4)
#' @note In cases where it is not clear which specific package a function is from several packages are returned for that function. This can for example be the case with filter(). This function uses an adapted version of 'list.functions.in.file' from the package NC.misc. That function was written by Nicholas Cooper 'njcooper (at) gmx.co.uk'. The adjustment consist of removing unique() such that all instances are reported.
#' @export
#'
credit_packages <- function(fns = NULL,
                            pkgs_vec_manual = NULL,
                            output_dir = NULL,
                            print_bibTeX = TRUE,
                            print_tex_citation_string = TRUE,
                            compare_loaded_with_used = TRUE,
                            report_most_used_pkgs = TRUE,
                            print_LaTeX_table = TRUE,
                            print_tsv = TRUE,
                            report_script_with_most_funs = TRUE,
                            verbose = TRUE
                            ){

#fns <- list.files(path = "../../../Nextcloud/Hedvigs_academia/2024/emergent_interface/Emergent_interface_Hedvig/", pattern = "*.[R|r]$", full.names = T, recursive = F)
#    output_dir = "."
#    pkgs_vec_manual = NULL
# verbose = TRUE
#  compare_loaded_with_used = TRUE
  #  report_most_used_pkgs = TRUE
#  report_script_with_most_funs = TRUE

if(all(is.null(fns), is.null(pkgs_vec_manual))){
    stop("Neither fns nor pkgs_vec_manual has been supplied.")
}

    if(is.null(output_dir)){
        stop("output_dir not supplied.")
    }else{
          if(!dir.exists(output_dir)){dir.create(output_dir)}
      }

    if(!is.null(fns)){

# df to bind to
df <- data.frame("packages" = as.character(),
                 "functions" = as.character(),
                 "script" = as.character())

for(fn in fns){

        if(verbose == TRUE){
  cat(paste0("I'm on ", fn, "\n"))
    }

    #fn <- fns[3]
x <- .list.functions.in.file_SH(filename = fn) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "package:", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "c\\(", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "\\)", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "\\\"", "")) %>%
    dplyr::mutate(packages = stringr::str_replace_all(packages, "character\\(0", "")) %>%
    dplyr::mutate(packages = stringr::str_split(packages, ";")) %>%
    tidyr::unnest(cols = "packages") %>%
    dplyr::mutate(script = fn)

df <- dplyr::full_join(x, df, by = c("packages", "functions", "script"))
}

used_packages <- df %>%
  dplyr::mutate(used = "TRUE") %>%
  filter(packages != "") %>%
  filter(!is.na(packages) )


# removing instances where a package wasn't found
used_packages$packages <- trimws(used_packages$packages)

used_packages <- used_packages %>%
    dplyr::filter(packages != ".GlobalEnv") %>%
    dplyr::filter(!is.na(packages)|
                    packages != "")

#df with loaded packages
if(compare_loaded_with_used == TRUE){

loaded_packages <- data.frame(packages = (.packages())) %>%
  dplyr::mutate(loaded = "TRUE")

joined_df <- dplyr::full_join(used_packages, loaded_packages, by = "packages")

unused_but_loaded <- joined_df %>%
    dplyr::filter(is.na(used)) %>%
    dplyr::filter(!is.na(loaded)) %>%
    dplyr::distinct(packages) %>% .[,1] %>% as.vector()

warning("There are packages that it seems like you're not using, but that are still loaded in the environment. Please note that these could be crucial depdencies of other packages, but they don't contain functions that you are calling directly. Do not remove from loading without careful inspection. They loaded but seemingly not used packages are: \n ", unused_but_loaded, "\n If you don't want to check this, set 'compare_loaded_with_used' to FALSE.\n\n" )

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
    dplyr::group_by(script) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(desc(n))

cat("The top 5 scripts which use the most functions:\n ")
print(as.matrix(
script_with_most_functions [1:5,])
)
}



if("" %in% df$packages & verbose == TRUE){

  not_matched <-   df %>%
    filter(packages == ""|is.na(packages)) %>%
    distinct("function") %>% .[,1] %>% as.vector()

  warning("There were some functions that couldn't be matched to packages. This could be because the package isn't loaded in this session. Run requirements.R or similar and run the function again. Another possible cause is that the functions don't belong to packages at all but were defined elsewhere. The functions that cannot be matched to packages are:.\n ", not_matched, "\n\n" )

}
  }

      if(is.null(fns) & !is.null(pkgs_vec_manual)){
        pkgs_to_cite <- pkgs_vec_manual %>% unique()
            }
    if(!is.null(fns) & !is.null(pkgs_vec_manual)){
        pkgs_to_cite <- unique(c(used_packages$packages, pkgs_vec_manual))
    }

    if(!is.null(fns) & is.null(pkgs_vec_manual)){
        pkgs_to_cite <- unique(c(used_packages$packages))
    }


if(print_bibTeX == TRUE){

    output_fn <- paste0(output_dir, "/used_pkgs.bib")

    knitr::write_bib(as.character(pkgs_to_cite), file = output_fn, tweak = T)

#readLines(output_fn) %>% View()
#  stringr::str_replace_all("\\&", "\\\\&") %>% #sorting out issues with ampersand
#  stringr::str_replace_all("\\\\\\\\&", "\\\\&") %>%
#    writeLines(output_fn)

##adding in citation for R itself

#https://stackoverflow.com/questions/46179997/r-missing-bib-key-in-citation-output
R_bib = utils::citation(package = "base")
R_bib$key = paste0("R_", R.version$major, ".", R.version$minor)
R_bib$note <- R.version.string
R_bib_string <- utils::toBibtex(R_bib)

bib <- readLines(output_fn)
bib <- c(bib, R_bib_string)
bib %>% writeLines(output_fn)

if(verbose == TRUE){

cat(paste0(
        "There were ", length(pkgs_to_cite)
, " packages to cite. They are:\n" ))
        print(pkgs_to_cite)

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

#    install.packages("pkgsearch")
#    library(pkgsearch)
#    pkg_info <- pkgsearch::cran_package("dplyr", version = "1.1.4")
#    pkg_info$`Date/Publication`


    pkgs_to_cite_df <- installed.packages()[pkgs_to_cite, "Version"] %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Package") %>%
        dplyr::rename("Version" = "." )

    if(print_tsv == TRUE ){
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


.list.functions.in.file_SH <- function (filename)
{
    if (!file.exists(filename)) {
        stop("couldn't find file ", filename)
    }
    if (!reader::get.ext(filename) == "R") {
        warning("expecting *.R file, will try to proceed")
    }
    tmp <- getParseData(parse(filename, keep.source = TRUE))
    funs <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")]

    df <- data.frame("functions" = funs)
    df[,2] <- NA #make empty col to fill in for-loop
    colnames(df) <- c("functions", "packages")

    for(i in 1:nrow(df)){

        funs <- utils::find(df[i,1])
        df[i,2] <- paste0(funs, collapse = ";")
    }





    return(df)
}

#credit_packages(fns = fns <- list.files(path = "../../../Nextcloud/Hedvigs_academia/2024/emergent_interface/Emergent_interface_Hedvig/", pattern = "*.[R|r]$", full.names = T, recursive = F), output_dir = "../../../Nextcloud/Hedvigs_academia/2024/emergent_interface/Emergent_interface_Hedvig/output/package_versions/")

