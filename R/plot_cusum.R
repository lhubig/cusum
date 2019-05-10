#' Plot CUSUM chart for a cusum Object
#'
#' Produces a CUSUM chart.
#' @export
#' @import utils
#' @param x An object of class cusum
#' @param signal If TRUE, signals are plotted (default)
#' @method plot cusum
#' @usage ## S3 method for class 'cusum' plot(x, signal = TRUE, ...)


plot.cusum <- function(x, signal = TRUE, ...) {
  plot(
    x = x$t,
    y = x$ct,
    type = "n",
    xlim = c(0, max(x$t)),
    ylim = c(0, max(x$ct)),
    ylab = expression(CUSUM[t]), xlab = "t",
    ...
  )

  if (signal == TRUE) {
    points(
      x = x$t[x$signal == 1],
      y = x$ct[x$signal == 1],
      col = "red",
      cex = 2,
      pch = 8,
      lwd = 5
    )
  }

  lines(
    x = x$t,
    y = x$ct
  )

  abline(h = unique(x$limit), col = "Blue")
  points(
    x = x$t,
    y = x$ct,
    cex = .5,
    pch = 16
  )
}
