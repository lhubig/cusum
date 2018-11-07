context("racusum")

risks <- c(0.001, 0.01, 0.1, 0.002, 0.02, 0.2)

set.seed(2046)
patient_risks <- sample(x = risks, size = 10, replace = TRUE)

set.seed(2046)
outcomes <- as.logical(rbinom(10, 1, patient_risks))

test_that("Output of Control limit simulation", {
  expected_results <- c(0, 0, 0, 0, 0, 0, 0.51, 1.02, 1.53, 1.53)
  works <-
    racusum(patient_risks,
      patient_outcomes = outcomes,
      limit = 2.96,
      odds_multiplier = 2,
      reset = TRUE
    )
  result <- round(works$ct, 2)
  expect_equal(result, expected_results)
})
