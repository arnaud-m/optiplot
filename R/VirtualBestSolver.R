#' Computes the virtual best solver
#'
#' A Virtual Best Solver (VBS) is a hypothetical algorithm that selects the best solver from a given portfolio of alternatives on a per-instance basis.
#' The VBS is determined by comparing the measures of algorithms with respect to the lexicographic order.
#'
#' @param data data frame of benchmarks results.
#' @param colA name of the column with the algorithm identifiers.
#' @param colI name of the column with the instance identifiers.
#' @param cols names of the columns with the measures.
#' @param withCols indicates if the VBS columns are added for each row.
#' @param withRows indicates if the VBS rows added
#'
#' @return the extended data with VBS rows and columns or the raw VBS data
#'
#' @export
#' @examples
#' data <- GenerateOptiData(n = 4, m = 10, p = 2)
#' VirtualBestSolver(data)
#' VirtualBestSolver(data, cols = c("s", "o", "m1"))
VirtualBestSolver <- function(data, colA = "a", colI = "i", cols = c("s", "o"), withRows = TRUE, withCols = TRUE) {
    ## Compute the VBS
    vbs <- data %>%
        group_by(across(all_of(colI))) %>%
        arrange(across(all_of(cols))) %>%
        select(all_of(c(colI, cols))) %>%
        slice_head()

    if(!withRows && !withCols) {
        ## Only return VBS data
        return(vbs)
    }
    rdata <- data
    if(withRows) {
        ## Add new rows
        vbs1 <- vbs
        vbs1[ , colA] = "VBS"
        rdata <- bind_rows(data, vbs1)
    }
    if(withCols) {
        ## Add new columns
        rdata <- rdata %>% dplyr::left_join(vbs, by = "i",  suffix = c("", ".VBS"))
        colvbs <- paste0(cols, ".VBS")
        isVBS <- rep(TRUE, nrow(rdata))
        IsEqual <- function(x,y) sapply(seq_along(x), function(i) identical(x[i], y[i]))
        isVBS <- sapply(seq_along(cols), function(i) IsEqual(rdata[,cols[i]], rdata[,colvbs[i]]))
        rdata$VBS <- apply(isVBS, 1, all)
        rdata <- rdata %>% dplyr::relocate(VBS, .before = colvbs[1])
    }
    ## Return updated data
    return(rdata)
}
