context("cusum_alpha_sim")


test_that("Output of alpha simulation", {
  expected_results <- 0.039
  works <- round(
    cusum_alpha_sim(
      failure_probability = 0.05,
      n_patients = 100,
      odds_multiplier = 2,
      n_simulation = 10000,
      limit = 2.96,
      seed = 2046
    ),
    3
  )
  expect_equal(works, expected_results)
})
