context("racusum_limit_sim")

# risks <- c(0.001, 0.01, 0.1, 0.002, 0.02, 0.2)
# 
# set.seed(2046)
# patient_risks <- sample(x = risks, size = 100, replace = TRUE)
# 
# test_that("Output of Control limit simulation", {
#   expected_results <- 2.59
#   works <- round(
#     racusum_limit_sim(patient_risks,
#       odds_multiplier = 2,
#       n_simulation = 1000,
#       alpha = 0.05,
#       seed = 2046
#     ),
#     2
#   )
#   expect_equal(works, expected_results)
# })
