#' Read the results of a benchmark performed with the grigrid tools.
#'
#'
#'
#' @param x a vector of pathnames.
#' @param colA name of the column with the algorithm identifiers.
#' @param recursive logical. Should the listing of results files recurse into directories?
#' @param pattern an optional regular expression.  Only file names which match the regular expression will be read.
#' @param skipLastCol logical. Should we skip the last column while reading a results file ?
#' @param skipExtension logical. Should we remove the extension when building the defaut algorithm name.

#' @return A data frame of benchmark results
#'
#' @section References:
#' https://github.com/arnaud-m/grigrid
#'
#' @export
ReadGrigrid<- function(x, colA = "a", recursive = TRUE, pattern = '*.res', skipLastCol = TRUE, skipExtension = TRUE) {

    ListFiles <- function(x) {
        if(dir.exists(x)) return(list.files(x, pattern = pattern, full.names=TRUE))
        else if(file.exists(x)) return(x)
        else {
            warning(paste(x, "is not a valid path to a file or directory"))
            return(NULL)
        }
    }

    ReadFile <- function(x) {
        df <- read.table(x, sep='|', fill = TRUE, strip.white = TRUE, header=TRUE)
        if(skipLastCol) df <- df[-ncol(df)]
        if(! colA %in% colnames(df) ) {
            name <- basename(x)
            if(skipExtension) name <- fs::path_ext_remove(name)
            df[ , colA] <- name
        }
        return(df)
    }

    TryReadFile <- function(x) {
        tryCatch(
            expr = ReadFile(x),
            error = function(e){
                warning(paste("ignored - failed to read", x, "\n", e))
                NULL
            },
            warning = function(w){
                warning(paste("ignored - failed to read", x, "\n", w))
                NULL
            }
        )
    }

    stopifnot(is.character(x))
    ## Find all files
    y <- unlist(unname(sapply(x, ListFiles)))
    ## Read all files
    y <- lapply(y, TryReadFile)
    ## Bind all data frames
    y <- dplyr::bind_rows(y)
    return(y)}
