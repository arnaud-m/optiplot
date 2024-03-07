#' The function uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels for two subsets of the data.
#'
#' @param data data frame of benchmarks results.
#' @param colI name of the column with the instance identifiers.
#' @param colS name of the column with the execution status.
#' @param cols logical, or column names for configuring the frame content.
#'
#' @return the cross table or a list containing the cross table, the merged data frame, and a table grob.
#'
#' @examples
#'
#' df <- GenerateOptiData()
#' CheckResults(df)
CheckResults <- function(data, colI = "i", colS = "s", colO = FALSE,
                         statusOPT = c("OPT", "OPTIMUM"),
                         statusSAT = c("SAT", "SATISFIABLE"),
                         statusUNSAT = c("UNSAT", "UNSATISFIABLE")
                         ) {
    MakeDiagnostics <- function(df){
       # print(df)
        df$diagnostic <- TRUE
        browser()
        s <-  dplyr::pull(df[ , colS])
        opt <- s %in% statusOPT
        sat <- s %in% statusSAT
        unsat <- s %in% statusUNSAT
        if(any(opt)) {

        } else if(any(sat)) {

        }
        return(df)


    }
    print(data)
    x <- data %>%
        group_split(across(all_of(colI))) %>%
        purrr::map( ~ MakeDiagnostics(.x))
    x
}
