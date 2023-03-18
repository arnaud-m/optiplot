#' Scatter plot that displays the ratio of two measures from the execution of algorithms over instances.
#'
#'
#' The data are displayed as a collection of points, each having the value of a ratio of one variable for two algorithms determining the position on the horizontal axis, and the value of the ratio of the other variable determining the position on the vertical axis.
#'
#'
#' @inheritParams CrossTable
#' @param colX name of the column with the measure used for the x-axis.
#' @param colY name of the column with the measure used for the y-axis.
#' @param minXY the minimal value of the numerator and denominator of a ratio.
#'
#' @return graphics object
#' @export
#' @examples
#' data <- GenerateOptiData(n = 3, m = 10, p = 2)
#' PlotScatterRatios(data, "A", colX = "m1", colY = "m2")
PlotScatterRatios <- function(data, x, y = NA, colX, colY, colA = "a", colI = "i", colS = "s", minXY = -Inf)  {
    ## Merge data and compute the cross table
    li <- CrossTable(data, x, y, colA, colI, colS, cols = c(colA, colX, colY), tableGrob = TRUE)
    df <- li$frame

    ## Compute ratio for each dimension of the plot
    GetRatios <- function(colM) pmax(df[ , paste0(colM, ".x")], minXY) / pmax(df[ , paste0(colM, ".y")], minXY)
    df[ , colX] <- GetRatios(colX)
    df[ , colY] <- GetRatios(colY)
    df[ , colA] <- df[ , paste0(colA, ".y")]


    ## Compute axis limits and round it for log scale
    GetLimits <- function(colM) {
        y <- log10(range(df[ , colM]))
        y <- c( 10**floor(y[1]), 10**ceiling(y[2]))
    }
    xlim <- GetLimits(colX)
    ylim <- GetLimits(colY)
    ## Create the plot
    if(is.character(y)) {
        plot <- ggplot(df, aes_string(x= colX, y= colY))  #, shape=namei, color=namej)) +
        namey <- y
    } else {
        plot <- ggplot(df, aes_string(x= colX, y= colY, shape = colA))
        namey <- paste0("non-", x)
    }
    ## Decorate the plot
    plot + ggtitle(sprintf('%s / %s', x, namey)) +
        geom_point(size=5)  + theme(text = element_text(size=20)) +
        scale_x_continuous(trans='log10' , limits = xlim) +
        scale_y_continuous(trans='log10', limits = ylim) +
        annotation_custom(li$grob, xmin = log10(xlim[2])/10, ymax =  log10(ylim[1])/10)
}
