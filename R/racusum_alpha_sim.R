#' Simulate false signal probability alpha given control limit for RA-CUSUM charts
#'
#' @export
#' @import checkmate
#' @import stats
#' @param patient_risks A vector containing patient risk scores
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases)
#' @param n_simulation Number of simulations
#' @param limit Control limit to signal process deterioration
#' @param seed An optional seed for simulation
#' @return Returns false signal probability
#' @examples
#'
#' # Patients risks are usually known from Phase I.
#' # If not, these risk scores can be simulated.
#'
#' # define possible patient risk scores
#' risks <- c(0.001,0.01,0.1,0.002,0.02,0.2)
#'
#' # sample risk population of size n = 100
#' set.seed(2046)
#' patient_risks <- sample(x = risks, size = 100, replace = TRUE)
#'
#' # control limit can be obtained with racusum_limit_sim(),
#' # here it is set to an arbitrary value (2.96)
#'
#' # simulate false positive probability of RA-CUSUM
#' racusum_alpha_sim(patient_risks,
#'     odds_multiplier = 2,
#'     n_simulation = 1000,
#'     limit = 2.96,
#'     seed = 2046)

racusum_alpha_sim <- function(patient_risks, odds_multiplier, n_simulation, limit, seed = NULL) {

  ## Check user input ####
  assert_numeric(patient_risks, lower = 0, upper = 1, min.len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(odds_multiplier, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  if (odds_multiplier < 1) {
    message("CUSUM detects process improvements (odds_multiplier < 1). ")
  }
  if (odds_multiplier == 1) {
    warning("CUSUM detects no process change (odds_multiplier = 1).")
  }

  assert_numeric(n_simulation, lower = 1, len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(limit, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(as.numeric(seed), lower = 0, max.len = 1)

  ## Calculate risk distribution ####
  n <- length(patient_risks)

  freq_score <- table(patient_risks)
  freq_score <- as.data.frame(freq_score)


  freq <- round(as.numeric(freq_score$Freq / sum(freq_score$Freq)), digits = 1000)
  risk <- sort(unique(patient_risks)) # corresponding risk scores


  if (sum(freq) != 1) {
    warning("Sum of risk frequence != 1.")
  }

  ## Simulate CUSUM runs ####
  cs_sim <- function(i) {
    p.0 <- sample(risk, size = n, replace = T, prob = freq)


    y <- rbinom(n, 1, p.0)
    ws <- log(1 / (1 + (odds_multiplier - 1) * p.0)) # success (non death) case
    wf <- log(odds_multiplier / (1 + (odds_multiplier - 1) * p.0)) # failure (death) case

    w.t <- ifelse(y == 1, wf, ws)
    c.t <- vector(mode = "numeric", length = n)
    c.t[1] <- max(c(0, c.t[1] + w.t[1]))
    for (i in 2:n) c.t[i] <- max(c(0, c.t[i - 1] + w.t[i]))
    return(max(c.t))
  }

  set.seed(seed)
  rl <- lapply(1:n_simulation, cs_sim)

  ## Estimate false signal probability ####
  x <- ecdf(unlist(rl))
  return(1 - x(limit))
}