context("cusum")

set.seed(1500)
outcomes <- as.logical(rbinom(n = 10, size = 1, prob = 0.05))

test_that("Output of Control limit simulation", {
  expected_results <- c(0.00, 0.64, 0.60, 0.55, 0.50, 0.45, 0.40, 1.04, 1.00, 0.95)
  works <- cusum(
    failure_probability = 0.05,
    patient_outcomes = outcomes,
    limit = 2.96,
    odds_multiplier = 2,
    reset = TRUE
  )
  result <- round(works$ct, 2)
  expect_equal(result, expected_results)
})
