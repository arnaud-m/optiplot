#' Horowitz and Sahni (HS) algorithm for the partition problem.
#'
# #' @inheritParams PartGetCapa
#'
#'  http://www.sc-square.org/CSA/workshop2-papers/RP3-FinalVersion.pdf
# #' @section References:
# #' Horowitz, E., and Sahni, S. 1974. Computing partitions with applications to the knapsack problem.
#' Journal of the ACM 21(2):277-292.
#'
BordaCount <- function(data, instance, algorithm, measures) {
    ## Rank according to the measures
    Ranking <- function(x) cbind(x[ , algorithm], rank = Rank(x[ , measures], decreasing = TRUE))

    ## Compute the ranking for each instance
    x <- data %>%
        dplyr::group_split(across(all_of(instance))) %>%
        purrr::map_dfr( ~  Ranking(.x))
    ## print(x)

    ## Sum the ranks for each algorithm
    y <- x %>%
        dplyr::group_by(across(all_of(algorithm))) %>%
        dplyr::summarise(count = sum(rank)) %>%
        dplyr::arrange(desc(count))

    return(y)
}
