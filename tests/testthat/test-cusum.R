context("cusum")

set.seed(1500)
outcomes <- as.logical(rbinom(n = 10, size = 1, prob = 0.05))

test_that("Output of CUSUM", {
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
}
)

test_that("Warning for recoding failure_prob",
          expect_that(cusum(failure_probability = .8, 
                            patient_outcomes = outcomes, 
                            limit = 2.96,
                            odds_multiplier = 2, 
                            reset = TRUE), 
                      gives_warning()
                      )
          )

test_that("Error if number of patients and weights don't match",
          expect_that(cusum(failure_probability = .1, 
                            patient_outcomes = outcomes, 
                            limit = 2.96,
                            odds_multiplier = 2, 
                            reset = TRUE,
                            weights = rep(1000, 0.1)), 
                      throws_error()
                      )
          )

test_that("Warning if CL is not same direction as OM 1",
          expect_that(cusum(failure_probability = .1, 
                            patient_outcomes = outcomes, 
                            limit = 2.96,
                            odds_multiplier = 0.5, 
                            reset = TRUE), 
                      gives_warning())
)

test_that("Warning if CL is not same direction as OM 2",
          expect_that(cusum(failure_probability = .1, 
                            patient_outcomes = outcomes, 
                            limit = -2.96,
                            odds_multiplier = 2, 
                            reset = TRUE), 
                      gives_warning())
)

test_that("Error
 if OM = 1",
          expect_that(cusum(failure_probability = .1, 
                            patient_outcomes = outcomes, 
                            limit = 2.96,
                            odds_multiplier = 1, 
                            reset = TRUE), 
                      throws_error())
)

# 
# test_that("Number of patients", {
#   
#   x <- cusum(failure_probability = 0.1,
#              patient_outcomes = rbinom(n = 100, size = 1, prob = .1),
#              limit = 2.5,
#              odds_multiplier = 2,
#              reset = FALSE)
#   
#   expect_equal(nrow(x), 100)
# }
# )
# 
# test_that("Class of cusum", {
#   expect_class(cusum(failure_probability = 0.1,
#                      patient_outcomes = rbinom(n = 100, size = 1, prob = .1),
#                      limit = 2.5,
#                      odds_multiplier = 2,
#                      reset = FALSE), c("cusum", "data.frame"))
#   
# })