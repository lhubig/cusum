context("cusum_limit_sim")

test_that("Output of Control limit simulation", {
  expected_results <- 2.87
  works <- round(
    cusum_limit_sim(
      failure_probability = 0.05,
      n_patients = 100,
      odds_multiplier = 2,
      n_simulation = 1000,
      alpha = 0.05,
      seed = 2046
    ),
    2
  )
  expect_equal(works, expected_results)
})
