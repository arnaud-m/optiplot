#' Summary of columns by group filtered by status
#'
#' @param data data frame of benchmarks results.
#' @param cols names of columns to summarize.
#' @param colG name of columns for grouping.
#' @param valS names of the status column for filtering.
#' @param colS name of the status column.
#'
#' @return a list of summary data frames.
#'
#' @export
#' @examples
#' data <- GenerateOptiData(n = 5, m = 10, p = 2)
#' ColSummary(data, cols = c("o", "m1"))
#' ColSummary(data, cols = c("o", "m1"), valS = list(c("SAT", "OPT"), "OPT"))
#' ColSummary(data, cols = c("o", "m1", "m2"), colG = c("a", "s"), valS = list("OPT", c("SAT", "OPT")))
ColSummary <-function(data, cols, colG = "a", valS = "OPT", colS = "s") {
    Table <- function(colG) table( data[ c(colG, colS)])
    ucolG <- unique(colG)
    res <- sapply(unique(ucolG), Table, simplify = FALSE)

    ## So that .data can be found
    if(!require(dplyr)) return(res)

    GeomMean <- function(x) exp(mean(log(x), na.rm = TRUE))
    GetSummaryVar <- function(valS, colG, colV) {
        data  %>% filter( .data[[colS]] %in% valS) %>%
            ##      group_by(.data[[colG]]) %>% ## Do not accept vector colG, only scalar
            group_by(across(all_of(colG))) %>%
             summarise(
                 n = n(),
                 na = sum(is.na(.data[[colV]])),
                 median = median(.data[[colV]], na.rm = TRUE),
                 mean = mean(.data[[colV]], na.rm = TRUE),
                 sd = sd(.data[[colV]], na.rm = TRUE),
                 gmean = GeomMean(.data[[colV]])
            )
    }

    Recycle <- function(x) rep(x, length = length(cols))
    colG <- Recycle(colG)
    valS <- Recycle(valS)

    for(i in seq_along(cols)) {
        cname <- paste(
            paste(cols[[i]], collapse = "_"),
            paste(valS[[i]], collapse = "_"),
            paste(colG[[i]], collapse = "_"),
            sep = "-"
        )
        res[[cname]] <- GetSummaryVar(valS[[i]], colG[[i]], cols[[i]])
    }
    return(res)
}
