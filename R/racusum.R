#' Risk-adjusted CUSUM Charts
#'
#' Calculate risk-adjusted CUSUM charts for performance data
#' @export
#' @import checkmate
#' @import stats
#' @import graphics
#' @param patient_risks A vector containing patient risk scores
#' @param patient_outcomes A vector containing patient outcomes in logical format (TRUE = event, FALSE = no event)
#' @param limit Control limit to signal process deterioration
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases); defaults to 2
#' @param reset Resets the CUSUM after a signal to 0; defaults to TRUE
#' @param limit_method "constant" or "dynamic"
#' @examples
#' # Patients risks are usually known from Phase I.
#' # If not, these risk scores can be simulated.
#'
#' # define possible patient risk scores
#' risks <- c(0.001, 0.01, 0.1, 0.002, 0.02, 0.2)
#'
#' # sample risk population of size n = 100
#' set.seed(2046)
#' patient_risks <- sample(x = risks, size = 100, replace = TRUE)
#'
#' # control limit can be obtained with racusum_CL_sim(),
#' # here it is set to an arbitrary value (2.96)
#'
#' ##### RA-CUSUM of in-control process
#' # simulate patient outcome for performace as expected
#' set.seed(2046)
#' patient_outcomes <- as.logical(rbinom(
#'   n = 100,
#'   size = 1,
#'   prob = patient_risks
#' ))
#'
#' racusum(patient_risks,
#'   patient_outcomes,
#'   limit = 2.96
#' )
#'
#' #### RA-CUSUM of out-of-control process
#' # simulate patient outcome for deviating performance
#'
#' set.seed(2046)
#' patient_outcomes <- as.logical(rbinom(n = 100, size = 1, prob = patient_risks * 2))
#' #'
#' racusum(patient_risks,
#'   patient_outcomes,
#'   limit = 2.96
#' )
racusum <- function(patient_risks, patient_outcomes, limit, odds_multiplier = 2, reset = TRUE, limit_method = c("constant", "dynamic")) {
  npat <- length(patient_risks)
  
  ## Check user input ####
  assert_numeric(patient_risks, lower = 0, upper = 1, min.len = 1, finite = TRUE, any.missing = FALSE)
  if (length(patient_risks) != length(patient_outcomes)){
    stop("Length patient_risks and patient_outcomes of unequal size.")
  }

  assert_logical(patient_outcomes, any.missing = FALSE)

  limit_method <- match.arg(limit_method)
  if (length(limit) == 1 & npat > 1 & limit_method == "dynamic"){
    stop("Provide vector of limit if limit_method = dynamic. Else change to constant")
  }
  
  if (limit_method == "constant"){
    limit <- rep(limit, length.out = npat)
  }
  
 # assert_numeric(limit, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(odds_multiplier, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  if (odds_multiplier < 1) {
    message("CUSUM is set to detect process improvements (odds_multiplier < 1). ")
  }
  if (odds_multiplier == 1) {
    warning("CUSUM is set to detect no process change (odds_multiplier = 1).")
  }
  

  assert_logical(reset, any.missing = FALSE, len = 1)

  ## Calculate RA-CUSUM Chart ####
  npat <- length(patient_risks)

  p <- patient_risks

  ct <- 0 # initial CUSUM value
  cs <- matrix(0, nrow = npat, ncol = 5) # storage matrix for alarms

  # CUSUM weights
  ws <- log((1 - p + 1 * p) / (1 - p + odds_multiplier * p))
  wf <- log(((1 - p + 1 * p) * odds_multiplier) / ((1 - p + odds_multiplier * p) * 1))

  w <- ifelse(patient_outcomes == 1, wf, ws) # weights based on outcome

  for (ii in 1:npat) {
    ct <- max(0, ct + w[ii]) # update CUSUM value
    cs[ii, 1] <- ii # store patient id
    cs[ii, 2] <- p[ii] # store patient risk
    cs[ii, 3] <- ct # store CUSUM value
    if (ct >= limit[ii]) {
      # test for signal
      cs[ii, 4] <- 1 # store signal
      cs[, 5] <- limit[ii]
      if (reset == 1) ct <- 0
    }
    else {
      cs[ii, 4] <- 0
    }
  }



  cs <- as.data.frame(cs)
  names(cs) <- c("t", "p", "ct", "signal", "limit")

  class(cs) <- c("cusum", "data.frame")


  return(cs)
}

