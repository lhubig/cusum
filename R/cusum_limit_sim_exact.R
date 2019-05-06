#' Calculate exact control limit given false signal probability alpha for CUSUM charts for very small sample sizes
#' 
#' This function only works for very small sample sizes (<= 15), 
#' as it permutes through all possible outcome sequences and estimates the percentage of runs that reach a specific CUSUM values.
#'
#' @export
#' @import checkmate
#' @param failure_probability Accepted failure probability of process
#' @param n_patients Sample size
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases)
#' @param alpha False signal probability
#' @return Returns the CL for small sample sizes
#' @examples
#'
#' # calculate exact control limits for alpha = 0.05
#' cusum_limit_sim_exact(failure_probability = 0.1,
#'     n_patients = 10,
#'     odds_multiplier = 2,
#'     alpha = 0.05)


cusum_limit_sim_exact <- function(n_patients,
                                  failure_probability,
                                  odds_multiplier,
                                  alpha) {
  #
  # This function calculates the exact distribution of the CUSUM
  # for a hospital with n_patients patients, the in control failure probability failure_probability
  # and the smallest inacceptable failure probability pA
  #
  
  assert_numeric(failure_probability, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE, len = 1)
  if (failure_probability > 0.5) {
    failure_probability <- 1 - failure_probability
    warning("Accepted failure probability failure_probability will be recoded to 1-failure_probability when > 0.5.")
  }
  
  n_patients <- as.integer(n_patients)
  assert_integer(n_patients, lower = 1, any.missing = FALSE, len = 1)
  if (n_patients > 15){
    message("Exact calculation only works for very small sample sizes of around <= 10 (check ?cusum_limit_sim_exact for more information). \nPlease abort if calculation takes to long. ")
  }
  
  assert_numeric(odds_multiplier, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  if (odds_multiplier < 1) {
    message("CUSUM detects process improvements (odds_multiplier < 1). ")
  }
  if (odds_multiplier == 1) {
    warning("CUSUM detects no process change (odds_multiplier = 1).")
  }
  
  assert_numeric(alpha, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  
  
  p.0 <- failure_probability
  o.0 <- p.0 / (1 - p.0)
  o.1 <- o.0 * odds_multiplier
  p.1 <- o.1 / (1 + o.1)
  
  outcome <- make_all_outcomes(npat_outcome = n_patients)
  
  p_failure <- apply(outcome, 1, function(kk, pp) {
    return(prod(ifelse(kk == 1, pp, 1 - pp)))
  }, pp = failure_probability)
  
  
  cs <- apply(outcome, 1, calc_cusum, c0 = failure_probability, cA = p.1)
  limit <- unique(as.vector(cs))
  which.res <- lapply(limit, which_rfc, mm = cs)
  
  cs_distr <- lapply(which.res, function(yy, pp) {
    return(sum(pp[yy]))
  }, pp = p_failure)
  
  res <- cbind(limit, unlist(cs_distr))
  res <- res[sort.list(res[, 1]), ]
  return(res[which.min(abs(alpha - res[,2])),1])
}


make_all_outcomes <- function(npat_outcome) {
  
  #
  # 	This function creates all possible sequences of outcomes
  #
  
  m <- matrix(0:1, ncol = 1)
  
  for (ii in 2:npat_outcome) {
    m <- cbind(rbind(m, m), rep(0:1, c(1, 1) * 2^(ii - 1)))
  }
  
  return(m)
}

calc_cusum <- function(x, c0, cA) {
  
  #
  # 	This function calculates the CUSUM chart
  # 	for the given sequence of successes and failures
  # 	provided by the vector x: x=0 no failure, x=1 failure
  #
  
  wt <- ifelse(x == 0, log((1 - cA) / (1 - c0)), log(cA / c0))
  
  j <- length(wt)
  ct <- rep(NA, j)
  ct[1] <- max(c(0, wt[1]))
  
  for (ii in 2:j) {
    ct[ii] <- max(c(0, ct[ii - 1] + wt[ii]))
  }
  return(ct)
}

which_rfc <- function(xx, mm) {
  res <- apply(mm, 2, function(yy, cc) {
    return(sum(yy >= cc) > 0)
  }, cc = xx)
  
  return(res)
}