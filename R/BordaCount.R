#' Apply the Borda count that is a ranked voting system
#'
#' @param data data frame of benchmarks results.
#' @param colA name of the column with the algorithm identifiers.
#' @param colI name of the column with the instance identifiers.
#' @param cols names of columns used for ranking based on lexicographic comparison.
#'
#' @section References:
#' * https://en.wikipedia.org/wiki/Borda_count
#'
#' @export
#' @examples
#' data.frame(
#' voter = c("u", "u", "u", "u", "v", "v", "v", "v", "w", "w", "w", "w"),
#' candidate = c("a", "b", "c", "d", "a", "b", "c", "d", "a", "b", "c", "d"),
#' rank = c(1, 2, 3, 4, 1, 2, 3, 4, 4, 1, 2, 3)
#' )
#' Bordacount(data, "instance", "algorithm", "rank")
#'
BordaCount <- function(data, colI, colA, cols) {
    ## Rank according to the cols
    Ranking <- function(x) cbind(x[ , colA], rank = Rank(x[ , cols], decreasing = TRUE))

    ## Compute the ranking for each colI
    x <- data %>%
        group_split(across(all_of(colI))) %>%
        purrr::map_dfr( ~  Ranking(.x))
    ## print(x)

    ## Sum the ranks for each algorithm
    y <- x %>%
        group_by(across(all_of(colA))) %>%
        summarise(count = sum(rank)) %>%
        arrange(desc(count))

    return(y)
}
