#' Dynamic Probability Control Limits (DPCL)
#'
#'
#' Set DPCL for risk-adjusted Bernoulli CUSUM Charts
#' @export
#' @import stats
#' @import data.table
#' @param patient_risks A vector containing patient risk scores
#' @param N Number of simulations
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases)
#' @param alpha False signal probability
#' @param seed An optional seed for simulation
#' @references Zhang, Xiang & Woodall, William. (2016). Dynamic Probability Control Limits for Lower and Two-Sided Risk-Adjusted Bernoulli CUSUM Charts. Quality and Reliability Engineering International. 10.1002/qre.2044.
#' @return Returns the control limit
#'
#' @examples
#' patient_risks <- runif(100, min = 0.1, max = 0.8)
#'
#' racusum_limit_dpcl(
#'   patient_risks = patient_risks,
#'   N = 1000,
#'   odds_multiplier = 2,
#'   alpha = 0.05,
#'   seed = 32423
#' )
racusum_limit_dpcl <- function(patient_risks, N = 100000, odds_multiplier = 2, alpha, seed = NULL) {
  ## Check user input ####
  assert_numeric(patient_risks, lower = 0, upper = 1, min.len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(N, lower = 1, len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(odds_multiplier, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  if (odds_multiplier < 1) {
    message("CUSUM is set to detect process improvements (odds_multiplier < 1). ")
  }
  if (odds_multiplier == 1) {
    warning("CUSUM is set to detect no process change (odds_multiplier = 1).")
  }

  assert_numeric(alpha, lower = 0, upper = 1, len = 1, finite = TRUE, any.missing = FALSE)

  # assert_numeric(as.numeric(seed), lower = 0, len = 1)


  cs <- 0

  M <- (N * (1 - alpha))

  h <- vector(length = length(patient_risks))

  set.seed(seed)
  for (i in 1:length(patient_risks)) {
    pi <- patient_risks[i]
    yi <- rbinom(N, 1, pi)
    ws <- log((1 - pi + 1 * pi) / (1 - pi + odds_multiplier * pi)) # survival
    wf <- log(((1 - pi + 1 * pi) * odds_multiplier) / ((1 - pi + odds_multiplier * pi) * 1)) # failure
    w <- ifelse(yi == 1, wf, ws)

    cs_i <- NULL
    for (j in 1:N) {
      ct_i <- sample(cs, 1)
      cs_i[j] <- max(0, ct_i + w[j])
    }
    cs <- sort(cs_i)
    h[i] <- cs[M]
    cs <- cs[cs <= h[i]]
  }


  return(h)
}
