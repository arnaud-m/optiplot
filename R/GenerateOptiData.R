#' Generate fake data for testing
#'
#' Each row of the data represents the answer of an algorithm for a given instance of an optimization problem.
#' * `a` the algorithm identifier
#' * `i` the instance identifier
#' * `s` the solving status (answer of the algorithm for the instance)
#' * `o` the objective value
#' * `mi` the i-th measures for the execution of the algorithm.
#'
#' @param n number of algorithms
#' @param m number of instances
#' @param p number of measures
#' @param t maximal measure value
#' @param o maximal objective value (optimum is always 0)
#' @param pSat probability of a SAT answer
#' @param pOpt probability of an OPT answer if the instance is SAT
#' @param pUnknown probability of an UNK answer
#'
#' @return a data frame with the specified columns.
#'
#' @export
#' @examples
#' GenerateOptiData()
#' GenerateOptiData( p = 0, pSat = 1)
GenerateOptiData <- function(n = 5, m = 5, p = 1, t = 100, o = 100, pSat = 0.75, pOpt = 0.25, pUnknown = 0.25) {
    ## Generate pairs (algorithm, instance)
    df <- expand.grid(
        a = head(LETTERS, n),
        i = head(letters, m)
    )
    ## Algorithm answers
    s <- factor(c("OPT", "SAT", "UNSAT", "UNK"), levels = c("OPT", "SAT", "UNSAT", "UNK"))

    df <- data.frame(
        a = rep(head(LETTERS, n), each = m), # algorithms
        i = head(letters, m), #instances
        s = sample (s[2:3], m , prob = c(pSat, 1- pSat), replace = TRUE) # sat answer
    )
    ind <- sample ( c(TRUE , FALSE), m * n , prob = c(pUnknown, 1- pUnknown), replace = TRUE)
    if(sum(ind) > 0) df$s[ind] <- s[4]

    sat <- which(df$s == "SAT")
    ind <- sample ( c(TRUE , FALSE), length(sat), prob = c(pOpt, 1- pOpt), replace = TRUE)
    if(sum(ind) > 0) df$s[sat[ind]] <- s[1]

    ## Generate objective

    df$o <- NA

    df$o[ df$s == s[1] ] <- 0
    sat <- sat[!ind]
    if(length(sat) > 0) df$o[ sat ] <- sample.int(o+1, length(sat) , replace = TRUE) - 1

    ## Generate measures
    if(p >= 1) {
        mat <- matrix(sample.int(t, nrow(df) * p, replace = TRUE), ncol = p)
        colnames(mat) <- paste0("m", seq(p))
        df <- cbind(df, mat)
    }
    return(df)
}

## GenerateOptiData <- function(n = 5, m = 5, p = 1, t = 100, o = 100, pSat = 0.5, pUnknown = 0.2, pOpt = 0.5) {
##     s <- as.factor(c("OPT", "SAT", "UNK"))
##     ## Generate pairs (algorithm, instance)
##     df <- expand.grid(
##         a = head(LETTERS, n),
##         i = head(letters, m)
##     )

##     ## Generate sat answer
##     df$s <- sample.int(s, nrow(df) , replace = TRUE)

##     ## df <- data.frame(
##     ##     a = rep(head(LETTERS, n), each = m), # algorithms
##     ##     i = head(letters, m), #instances
##     ##     s = sample (s[2:3], m , prob = c(pSat, 1- pSat), replace = TRUE) # sat answer
##     ## )

##     ## Generate objective
##     df$o <- sample.int(o+1, nrow(df) , replace = TRUE) - 1
##     df$o[ df$s == s[1] ] <- 0
##     df$o[ df$s == s[3] ] <- NA

##     ## Generate measures
##     mat <- matrix(sample.int(t, nrow(df) * p, replace = TRUE), ncol = p)
##     colnames(mat) <- paste0("t", seq(p))

##     return(cbind(df, mat));
## }
