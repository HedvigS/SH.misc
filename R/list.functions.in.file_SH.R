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