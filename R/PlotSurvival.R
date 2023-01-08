#' Survival Plot
#'
#'
#' @param data data frame of benchmarks results.
#' @param colG name of the column used for grouping the measures
#' @param colM name of the column with the measure
#' @param useBW logical that indicates if the plot is in black and white
#' @param useDefaultTheme logical that indicates if the plot uses the default theme
#' @param lwd integer, linewidth of the plot
#' @return graphics object
#'
#' @section References:
#'  http://www.sc-square.org/CSA/workshop2-papers/RP3-FinalVersion.pdf
#'
#' @export
#' @examples
#' df <- data.frame( algorithm = c("a", "b", "c"), time = sample.int(100, 120, replace = TRUE))
# #' PlotSurvival(df, "algorithm", "time")
PlotSurvival <- function(data, colG, colM, useBW = FALSE, useDefaultTheme = TRUE, lwd = 1) {
    x <- data %>%
        group_by(across(all_of(colG))) %>%
        arrange(across(all_of(colM))) %>%
        mutate(Count = row_number())

    if(useBW) plot <- ggplot(x) + geom_step(aes_string(x= colM, y="Count", linetype=colG), lwd = lwd)
    else plot <- ggplot(x) + geom_step(aes_string(x=colM, y="Count", linetype=colG, color=colG), lwd = lwd)
    if(useDefaultTheme) {
        plot <- plot + theme_bw() +
            theme(text = element_text(size=30), legend.position="bottom") +
            scale_x_continuous(trans='log10')
    }
    return(plot)
}
