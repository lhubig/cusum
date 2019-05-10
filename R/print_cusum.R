#' summary for a cusum Object
#'
#' summary cusum results
#'
#' @import utils
#' @param x An object of class cusum
#' @method summary cusum
#' @usage ## S3 method for class 'cusum' summary(x, ...)


summary.cusum <- function(object, ...) {
  signals <- object[object$signal == 1, ]
  z1 <- paste0(nrow(object), " observations are monitored, and ", nrow(signals), " signals are triggered.")

  if (length(nrow(signals)) > 0) {
    z2 <- paste0("First signal is triggered at observation ", min(signals$t), ".")
  } else {
    z2 <- NULL
  }


  return(c(z1, z2))
}
