#' The function uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels for two subsets of the data.
#'
#' @param data data frame of benchmarks results.
#' @param x the identifier of an algorithm.
#' @param y the identifier of an algorithm (optional).
#' @param colA name of the column with the algorithm identifiers.
#' @param colI name of the column with the instance identifiers.
#' @param colS name of the column with the execution status.
#' @param cols logical, or column names for configuring the frame content.
#' @param tableGrob logical. Configure the table grob result.
#'
#' @export
#' @examples
#' Draw <- function(x) {grid::grid.newpage();grid::grid.draw(x)}
#'
CrossTable <- function(data, x, y = NA, colA = "a", colI = "i", colS = "s", cols = FALSE, tableGrob = FALSE) {
    ## Select columns for merging
    ind <- c(colA, colI, colS)
    if(is.character(cols)) {
        ind <- union(ind, cols)
    }
    ## Merge the extracted data frames
    dx <- subset (data [ , ind ], data[ , colA] == x)
    if(is.character(y)) dy <- subset (data [ , ind ], data[ , colA] == y)
    else dy <- subset (data [ , ind ], data[ , colA] != x)
    df <- dplyr::full_join(dx, dy, by = colI)

    ## Compute the cross table
    ct <- droplevels(df[, paste0(colS, c(".x", ".y"))])
    ct <- table(ct)
    namey <- ifelse(is.character(y), y, paste0("non-", x))
    names(attributes(ct)$dimnames) <- c(x, namey)

    ## Add the cross table to the results
    res <-list(table = ct)
    ## Add the merged data frame to the results if requested
    if(!is.logical(cols) || cols) {
        ## TODO frame -> data ?
        res <- append(res, list(frame = df))
    }

    ## Create the table grob if requested
    if(tableGrob) {
        ## https://stackoverflow.com/questions/12318120/adding-table-within-the-plotting-region-of-a-ggplot-in-r
        ## Customize theme : row heads are identical to col heads
        th <- gridExtra::ttheme_default()
        th$rowhead$bg_params <- th$colhead$bg_params
        th$rowhead$fg_params <- th$colhead$fg_params
        ## Build the table
        content <- gridExtra::tableGrob(ct, theme = th)
        ## Add additional headers
        rheader <- gridExtra::tableGrob(data.frame(ct[1, 1], ct[1, 2]), rows=NULL, cols=c(x, namey))
        tg <- gridExtra::gtable_combine(rheader[1, ], content, along=2)
        ## Update the layout so that the new row header spans multiple columns
        tg$widths <- rep(max(tg$widths), length(tg$widths))
        ## tg$layout[1:4 , c("l", "r")] <- list( c(1, 2), c(1, 4))
        tg$layout[1:4 , c("l", "r")] <- list( c(1, 2), c(1, max(tg$layout[ , "r"])))
        tg$layout[c(1,3), c("t", "b")] <- c(2, 2)
        ## Add to results
        res <- append(res, list(grob = tg))
    }
    ## Simplify the results if possible
    if(length(res) == 1) return(res[[1]])
    else return(res)
}
