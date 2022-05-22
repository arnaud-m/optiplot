#' Returns the sample fractional ranks of the values in a vector, list or data frame.
#'
#' Missing values (‘NA’s) are put last, but their ranks are not averaged.
#'
#' @param x a vector, list, or data frame.
#'
#' @return A numeric vector of the same length as ‘x’ without names.
#' The vector is of double type because the ranking use the average method (whether or not there are any ties).
#'
#' @seealso https://stackoverflow.com/a/27036534
#'
#' @export
#' @examples
#' df <- data.frame(x = c(NA, 1, 2), y = c(12:1, NA, NA, NA))
#' df$r <- Rank(df)
#' df[ order(df$r), ]
#'
#' df <- rbind(df, df)
#' df$r <- NULL
#' df$r <- Rank(df)
#' df[ order(df$r), ]
Rank<- function(x) {
    if(is.list(x)) {
        rank <- order(do.call("order", c(x, na.last = TRUE)))
        rank <-  do.call("ave", list(x=rank, x))
        return(rank)
    } else if(is.vector(x)) return(rank(unname(x), ties.method = "average", na.last = TRUE))
    return(NULL)
}
