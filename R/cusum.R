#' Non-risk-adjusted CUSUM Charts
#'
#' Calculate non-risk-adjusted CUSUM charts for performance data
#' @export
#' @import checkmate
#' @import stats
#' @import graphics
#' @param failure_probability Accepted failure probability of process
#' @param patient_outcomes A vector containing patient outcomes in logical format (TRUE = event, FALSE = no event)
#' @param limit Control limit to signal process deterioration. 
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases); defaults to 2
#' @param reset Resets the CUSUM after a signal to 0 if TRUE; defaults to TRUE
#' @examples
#'
#' # control limit can be obtained with cusum_limit_sim(),
#' # here it is set to an arbitrary value (2.96)
#' 
#' # CUSUM of in-control process
#' # simulate patient outcomes
#' set.seed(2046)
#' patient_outcomes <- as.logical(rbinom(n = 100, size = 1, prob = 0.05))
#'
#'
#' cs_ic <- cusum(failure_probability = 0.05,
#'     patient_outcomes,
#'     limit = 2.96)
#'
#' # CUSUM of out-of-control process
#' # simulate patient outcome
#' set.seed(2046)
#' patient_outcomes <- as.logical(rbinom(n = 100, size = 1, prob = 0.2))
#'
#' cs_oc <- cusum(failure_probability = 0.05,
#'     patient_outcomes,
#'     limit = 2.96)

cusum <- function(failure_probability, patient_outcomes, limit, odds_multiplier = 2, reset = TRUE) {

  ## Check user input ####
  assert_numeric(failure_probability, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE, len = 1)
  if (failure_probability > 0.5) {
    failure_probability <- 1 - failure_probability
    warning("Accepted failure probability failure_probability will be recoded to 1-failure_probability when > 0.5.")
  }

  assert_logical(patient_outcomes, any.missing = FALSE)

  assert_numeric(limit, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(odds_multiplier, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  if (odds_multiplier < 1) {
    message("CUSUM is set to detect process improvements (odds_multiplier < 1). ")
  }
  if (odds_multiplier == 1) {
    warning("CUSUM is set to detect no process change (odds_multiplier = 1).")
  }

  assert_logical(reset, any.missing = FALSE, len = 1)
  
  ## Calculate CUSUM Chart ####
  npat <- length(patient_outcomes)

  p.0 <- failure_probability
  o.0 <- p.0 / (1 - p.0)
  o.1 <- odds_multiplier * o.0
  p.1 <- o.1 / (1 + o.1)
  ca <- p.1

  ct <- 0
  cs <- matrix(0, nrow = npat, ncol = 5)

  wf <- log(ca / failure_probability) # CS weight for failure
  ws <- log((1 - ca) / (1 - failure_probability)) # CS weight for success

  w <- ifelse(patient_outcomes == 1, wf, ws) # weights based on outcome

  for (ii in 1:npat) {
    ct <- max(0, ct + w[ii]) # update CUSUM value
    cs[ii, 1] <- ii # store patient id
    cs[ii, 2] <- failure_probability
    cs[ii, 3] <- ct # store CUSUM value
    if (ct >= limit) {
      # test for signal
      cs[ii, 4] <- 1 # store signal

      if (reset == TRUE) ct <- 0
    }
    else {
      cs[ii, 4] <- 0
    }
  }

  cs[, 5] <- limit

  cs <- as.data.frame(cs)
  names(cs) <- c("t", "failure_probability", "ct", "signal", "limit")

  class(cs) <- "cusum"
  
  return(cs)
}
