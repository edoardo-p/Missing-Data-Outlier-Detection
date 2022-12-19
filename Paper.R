rm(list = ls())
cat("\014")

library(mvtnorm)
library(mclust)
library(ggplot2)

set.seed(42)

ampute <- function(data, prop) {
  missing <- sample(1:nrow(data), nrow(data) * prop)
  for (i in missing) {
    select <- sample(1:2, 1)
    data[i, select] <- NA
  }
  return(data)
}

generate_tm <- function(n, prop, mu1, mu2, sigma1, sigma2, df1, df2) {
  u <- runif(n)
  tm <- matrix(nrow = n, ncol = 2)
  for (i in 1:n) {
    if (u[i] < prop) {
      tm[i, ] <- mu1 + rmvt(1, sigma1, df = df1)
    } else {
      tm[i, ] <- mu2 + rmvt(1, sigma2, df = df2)
    }
  }
  return(tm)
}

generate_mnm <- function(n, prop, mu1, mu2, sigma1, sigma2) {
  u <- runif(n)
  mnm <- matrix(nrow = n, ncol = 2)
  for (i in 1:n) {
    if (u[i] < prop) {
      mnm[i, ] <- rmvnorm(1, mu1, sigma1)
    } else {
      mnm[i, ] <- rmvnorm(1, mu2, sigma2)
    }
  }
  return(mnm)
}

# Generate two datasets with 100 samples and two with 500 samples.
# Datasets are generated from mixture models and are bidimensional.
n_small <- 100
n_large <- 500
pi1 <- 0.3
mu1_far <- c(0, -3)
mu1_close <- c(0, -1)
mu2 <- c(0, 3)
sigma1 <- matrix(c(1, -0.5, -0.5, 1), nrow = 2)
sigma2 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

tm_small <- generate_tm(n_small, pi1, mu1_far, mu2, sigma1, sigma2, 4, 10)
tm_large <- generate_tm(n_large, pi1, mu1_far, mu2, sigma1, sigma2, 4, 10)

mnm_small <- generate_mnm(n_small, pi1, mu1_far, mu2, sigma1, sigma2)
mnm_large <- generate_mnm(n_large, pi1, mu1_far, mu2, sigma1, sigma2)

par(mfrow = c(2, 2))
plot(tm_small, pch = 19, xlab = "d1", ylab = "d2")
plot(tm_large, pch = 19, xlab = "d1", ylab = "d2")
plot(mnm_small, pch = 19, xlab = "d1", ylab = "d2")
plot(mnm_small, pch = 19, xlab = "d1", ylab = "d2")

# t.mix <- pi1 * (rmvt(N, sigma = sigma1, df = 4) + rep(mu, N)) +
# pi2 * (rmvt(N, sigma = sigma2, df = 10) + rep(mu2, N))

# multi.contam.norm <- pi1 * rCN(N, mu, sigma1, 0.9, 20) + pi2 * rCN(N, mu2, sigma2, 0.8, 30)

# multi.norm.one <- pi1 * rmvnorm(N, mu1.close, sigma1) + pi2 * rmvnorm(N, mu2, sigma2)
# for (i in sample(1:nrow(multi.norm.one), nrow(multi.norm.one) * 0.01)) {
#   multi.norm.one[i, ] <- c(0, runif(1, min = 10, max = 15))
# }

# multi.norm.five <- pi1 * rmvnorm(N, mu1.close, sigma1) + pi2 * rmvnorm(N, mu2, sigma2)
# for (i in sample(1:nrow(multi.norm.five), nrow(multi.norm.five) * 0.05)) {
#   multi.norm.five[i, ] <- runif(2, min = -10, max = 10)
# }

# multi.norm.thirty <- pi1 * rmvnorm(N, mu, sigma1) + pi2 * rmvnorm(N, mu2, sigma2)
# for (i in sample(1:nrow(multi.norm.thirty), nrow(multi.norm.thirty) * 0.30)) {
#   multi.norm.thirty[i, ] <- runif(2, min = -10, max = 10)
# }





#++++ With no missing values ++++#
X <- nm_5_noise_close_100[, 1:2]
mod <- MCNM(X, G = 2, init_method = "kmedoids", max_iter = 10)
summary(mod)
plot(mod)
#++++ With missing values ++++#
set.seed(1234)
X <- hide_values(nm_5_noise_close_100[, 1:2], prop_cases = 0.1)
mod <- MCNM(X, G = 2, init_method = "kmedoids", max_iter = 10)
summary(mod)
plot(mod)
