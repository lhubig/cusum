#' Simulate false signal probability alpha given control limit for CUSUM charts
#'
#' @export
#' @import checkmate
#' @import stats
#' @param failure_probability Accepted failure probability of process
#' @param n_patients Sample size
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases)
#' @param n_simulation Number of simulations
#' @param limit Control limit to signal process deterioration
#' @param seed Add an optional seed for simulation
#' @return Returns false signal probability alpha
#' @examples
#'
#
#' # control limit can be obtained with cusum_limit_sim(),
#' # here it is set to an arbitrary value (2.96)
#'
#' # simulate false positive probability of CUSUM
#' cusum_alpha_sim(failure_probability = 0.05,
#'     n_patients = 100,
#'     odds_multiplier = 2,
#'     n_simulation = 10000,
#'     limit = 2.96,
#'     seed = 2046)

cusum_alpha_sim <- function(failure_probability, n_patients, delta, n_simulation, limit, seed = NULL) {

  ## Check user input ####
  assert_numeric(failure_probability, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE, len = 1)
  if (failure_probability > 0.5) {
    failure_probability <- 1 - failure_probability
    warning("Accepted failure probability failure_probability will be recoded to 1-failure_probability when > 0.5.")
  }

  n <- as.integer(n_patients)
  assert_integer(n, lower = 1, any.missing = FALSE, len = 1)

  assert_numeric(delta, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)
  if (delta < 1) {
    message("CUSUM detects process improvements (delta < 1). ")
  }
  if (delta == 1) {
    warning("CUSUM detects no process change (delta = 1).")
  }

  assert_numeric(n_simulation, lower = 1, len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(limit, lower = 0, len = 1, finite = TRUE, any.missing = FALSE)

  assert_numeric(as.numeric(seed), lower = 0, max.len = 1)

  ## Simulate CUSUM Charts ####
  cs_sim <- function(i, npat = n, p = failure_probability, or = delta) {
    p.0 <- p
    o.0 <- p.0 / (1 - p.0)
    o.1 <- o.0 * or
    p.1 <- o.1 / (1 + 0.1)

    y <- rbinom(npat, 1, p.0)
    w.t <- y * log(p.1 / p.0) + (1 - y) * log((1 - p.1) / (1 - p.0))
    c.t <- vector(mode = "numeric", length = npat)
    c.t[1] <- max(c(0, c.t[1] + w.t[1]))
    for (i in 2:npat) c.t[i] <- max(c(0, c.t[i - 1] + w.t[i]))
    return(max(c.t))
  }

  set.seed(seed)
  rl <- lapply(1:n_simulation, cs_sim)

  ## Estimate false signal probability ####
  x <- ecdf(unlist(rl))
  return(1 - x(limit))
}
