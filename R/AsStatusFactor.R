#' Transform character strings into status factors
#'
#' @export
#' @examples
#' status <- c("OPT", "OPTIMUM", "SAT", "SATISFIABLE", "UNSAT", "UNSATISFIABLE", "UNKNWON", "TIMEOUT")
#' AsStatusFactor(status)
#' AsStatusFactor(status,  statusOPT = "OPT", statusSAT = "SAT", statusUNSAT = "UNSAT"))
AsStatusFactor <- function(x,
                           statusOPT = c("OPT", "OPTIMUM"),
                           statusSAT = c("SAT", "SATISFIABLE"),
                           statusUNSAT = c("UNSAT", "UNSATISFIABLE")
                           ) {
    ## TODO Add an option for dropping levels ?
    s <- rep("UNK", length(x))
    s [ x %in% statusOPT] <- "OPT"
    s [ x %in% statusSAT] <- "SAT"
    s [ x %in% statusUNSAT] <- "UNSAT"
    return(factor(s, levels = c("OPT", "SAT", "UNSAT", "UNK")))
}
