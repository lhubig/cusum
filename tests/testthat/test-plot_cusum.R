context("plot_cusum")

cs <- cusum(failure_probability = 0.1,
            patient_outcomes = rbinom(100,1,0.1),
            limit = 2,
            odds_multiplier = 2,
            reset = FALSE)
cs <- cs[,-1]

test_that("plot asks for t",
          expect_error(plot(cs))
)

cs <- cusum(failure_probability = 0.1,
            patient_outcomes = rbinom(100,1,0.1),
            limit = 2,
            odds_multiplier = 2,
            reset = FALSE)
cs <- cs[,-3]

test_that("plot asks for ct",
          expect_error(plot(cs))
)
