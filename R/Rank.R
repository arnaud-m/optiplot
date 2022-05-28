#' Returns the sample fractional ranks of the values in a vector, list or data frame.
#'
#' Rank the argument in increasing order.
#' Missing values (‘NA’s) are put last, but their ranks are not averaged.
#'
#' @param x a vector, list, or data frame.
#' @param decreasing logical. Should the ranking be increasing or decreasing?
#'
#' @return A numeric vector of the same length as ‘x’ without names.
#' The vector is of double type because the ranking use the average method (whether or not there are any ties).
#' @section References:
#' https://en.wikipedia.org/wiki/Ranking
#' https://stackoverflow.com/a/27036534
#'
#' @export
#' @examples
#' df <- data.frame(x = c(1, 2, NA), y = c(12:1, NA, NA, NA))
#' df$r <- Rank(df)
#' df[ order(df$r), ]
#'
#' df <- rbind(df, df)
#' df$r <- NULL
#' df$r <- Rank(df)
#' df[ order(df$r), ]
Rank<- function(x, decreasing = FALSE) {
    if(is.list(x)) {
        ranks <- order(do.call("order", c(x, na.last = TRUE)))
        ranks <-  do.call("ave", list(x=ranks, x))
    } else if(is.vector(x) || is.factor(x)) {
        ranks <- rank(unname(x), ties.method = "average", na.last = TRUE)
    } else {
        warning("Returning NULL - cannot rank ", toString(x))
        return(NULL)
    }
    if(decreasing) {
        ranks <- length(ranks) + 1 - ranks
    }
    return(ranks)
}
