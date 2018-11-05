library(readr)
qidata <- read_csv("data-raw/qidata.csv")

# risk-adjusted data
ra_perf <- qidata[qidata$qi == 11724, ]
ra_perf$t <- rownames(ra_perf)
ra_perf <- ra_perf[, c("t", "y", "score", "year")]
ra_perf$t <- as.integer(ra_perf$t)
ra_perf$y <- as.logical(ra_perf$y)

# define data for 2016
ra_perf_p1 <- ra_perf[ra_perf$year == 2016, ]
set.seed(2046)
index <- sample(1:nrow(ra_perf_p1), 1000)
ra_perf_p1 <- ra_perf_p1[index, ]
set.seed(2046)
ra_perf_p1$y <- as.logical(rbinom(500, 1, ra_perf_p1$score))

# define data for 2017

## IC-Data for 2017
ra_perf_p2_1 <- ra_perf[ra_perf$year == 2017, ]
set.seed(2046)
index <- sample(1:nrow(ra_perf_p2_1), 500)
ra_perf_p2_1 <- ra_perf_p2_1[index, ]
set.seed(2046)
ra_perf_p2_1$y <- as.logical(rbinom(500, 1, ra_perf_p2_1$score))

## OC-Data for 2017
ra_perf_p2_2 <- ra_perf[ra_perf$year == 2017, ]
set.seed(2046)
index <- sample(1:nrow(ra_perf_p2_2), 500)
ra_perf_p2_2 <- ra_perf_p2_2[index, ]
set.seed(2046)
ra_perf_p2_2$y <- as.logical(rbinom(500, 1, ra_perf_p2_2$score * 2))

ra_perf <- rbind(ra_perf_p1, ra_perf_p2_1, ra_perf_p2_2)

ra_perf$t <- rownames(ra_perf)

racusum_example_data <- ra_perf
devtools::use_data(racusum_example_data, overwrite = TRUE)


# non-risk-adjusted data
nra_perf <- qidata[qidata$qi == "54030", ]
nra_perf$t <- rownames(nra_perf)
nra_perf <- nra_perf[, c("t", "y", "year")]
nra_perf$t <- as.integer(nra_perf$t)
nra_perf$y <- as.logical(nra_perf$y)

nra_perf_p1 <- nra_perf[nra_perf$year == 2016, ]
set.seed(2046)
index <- sample(1:nrow(nra_perf_p1), 1000)
nra_perf_p1 <- nra_perf_p1[index, ]
set.seed(2046)
nra_perf_p1$y <- as.logical(rbinom(500, 1, mean(nra_perf$y[nra_perf$year == 2016])))

nra_perf_p2_1 <- nra_perf[nra_perf$year == 2017, ]
set.seed(2046)
index <- sample(1:nrow(nra_perf_p2_1), 500)
nra_perf_p2_1 <- nra_perf_p2_1[index, ]
set.seed(2046)
nra_perf_p2_1$y <- as.logical(rbinom(500, 1, mean(nra_perf_p1$y)))

nra_perf_p2_2 <- nra_perf[nra_perf$year == 2017, ]
set.seed(2046)
index <- sample(1:nrow(nra_perf_p2_2), 500)
nra_perf_p2_2 <- nra_perf_p2_2[index, ]
set.seed(2046)
nra_perf_p2_2$y <- as.logical(rbinom(500, 1, mean(nra_perf_p1$y) * 2))



nra_perf <- rbind(nra_perf_p1, nra_perf_p2_1, nra_perf_p2_2)
nra_perf$t <- rownames(nra_perf)

cusum_example_data <- nra_perf
devtools::use_data(cusum_example_data, overwrite = TRUE)
