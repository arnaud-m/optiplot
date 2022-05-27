#' Horowitz and Sahni (HS) algorithm for the partition problem.
#'
#' @export
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
