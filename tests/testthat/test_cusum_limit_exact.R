context("cusum_limit_exact")

test_that("Exact calculation of CL",{
  x <- unname(round(cusum_limit_exact(failure_probability = 0.1,
                                      n_patients  = 10,
                                      odds_multiplier = 2,
                                      alpha = 0.05),5))
  expect_equal(x,
               1.41227)
}
  )
