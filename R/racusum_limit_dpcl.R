#' Setting the Dynamic Probability Control Limits
#'
#'
#'
#' @export
#' @import stats
#' @import data.table
#' @param patient_risks A vector containing patient risk scores
#' @param N Number of simulations
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases)
#' @param alpha False signal probability
#' @param seed An optional seed for simulation
#' @references Zhang, Xiang & Woodall, William. (2016). Dynamic Probability Control Limits for Lower and Two-Sided Risk-Adjusted Bernoulli CUSUM Charts: DPCLs for Lower and Two-Sided Risk-Adjusted Bernoulli CUSUM. Quality and Reliability Engineering International. 10.1002/qre.2044.
#' @return Returns the control limit
#'
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

  assert_numeric(as.numeric(seed), lower = 0, len = 1)


  delta_o <- 1
  n_patients <- length(patient_risks)

  hs <- matrix(0, nrow = n_patients, ncol = 2)

  cs <- 0 # initial cusum value

  set.seed(seed)

  # loop over patients
  for (j in 1:n_patients) {
    p1 <- patient_risks[j] # in control failure rate p
    y1 <- rbinom(N, 1, p1) # generate N Bernoulli random variables with ic-failure p1

    # define weights
    ws <- log((1 - p1 + delta_o * p1) / (1 - p1 + odds_multiplier * p1)) # survival
    wf <- log(((1 - p1 + delta_o * p1) * odds_multiplier) / ((1 - p1 + odds_multiplier * p1) * delta_o)) # failure

    css <- matrix(0, nrow = N, ncol = 2)

    # generate N CUSUM statistics
    for (i in 1:N) {
      if (j != 1) {
        cs <- sample(c, 1)
      } else {
        cs <- 0
      }
      w <- ifelse(y1[i] == 0, ws, wf)

      ct <- max(0, cs + w)
      css[i, ] <- c(i, ct)
    }

    css <- data.table(css, key = "V2") # sort CUSUM statistics in ascending order

    # take upper percentile as h
    m <- floor(N * (1 - alpha))

    h <- css$V2[m]
    hs[j, ] <- c(j, h)

    # remove all cusum statistics with higher value
    c <- css$V2[css$V2 <= h]
  }

  return(hs)
}
