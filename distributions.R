library(mvtnorm)

rmix <- function(sample_size, prior, comp1, comp2) {
  uniform <- runif(sample_size)
  mixture <- matrix(nrow = sample_size, ncol = 3)
  for (i in 1:sample_size) {
    if (uniform[i] < prior) {
      mixture[i, ] <- comp1()
    } else {
      mixture[i, ] <- comp2()
    }
  }
  return(mixture)
}

contaminate <- function(data, eta, criterion) {
  for (i in sample(1:nrow(data), nrow(data) * eta)) {
    data[i, ] <- c(criterion(), 3)
  }
  return(data)
}

set.seed(42)

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

tm1_far <- function() c(mu1_far + rmvt(1, sigma1, 4), 1)
tm1_close <- function() c(mu1_close + rmvt(1, sigma1, 4), 1)
tm2 <- function() c(mu2 + rmvt(1, sigma2, 10), 2)

mcn_good_1_far <- function() c(rmvnorm(1, mu1_far, sigma1), 1)
mcn_bad_1_far <- function() c(rmvnorm(1, mu1_far, 20 * sigma1), 3)
mcn_good_1_close <- function() c(rmvnorm(1, mu1_close, sigma1), 1)
mcn_bad_1_close <- function() c(rmvnorm(1, mu1_close, 20 * sigma1), 3)
mcn_good_2 <- function() c(rmvnorm(1, mu2, sigma2), 2)
mcn_bad_2 <- function() c(rmvnorm(1, mu2, 30 * sigma2), 3)

mcn1_far <- function() rmix(1, 0.9, mcn_good_1_far, mcn_bad_1_far)
mcn1_close <- function() rmix(1, 0.9, mcn_good_1_close, mcn_bad_1_close)
mcn2 <- function() rmix(1, 0.8, mcn_good_2, mcn_bad_2)

mnm1_far <- function() c(rmvnorm(1, mu1_far, sigma1), 1)
mnm1_close <- function() c(rmvnorm(1, mu1_close, sigma1), 1)
mnm2 <- function() c(rmvnorm(1, mu2, sigma2), 2)

tm_small <- rmix(n_small, pi1, tm1_close, tm2)
tm_large <- rmix(n_large, pi1, tm1_close, tm2)

mcnm_small <- rmix(n_small, pi1, mcn1_close, mcn2)
mcnm_large <- rmix(n_large, pi1, mcn1_close, mcn2)

mnm_small <- rmix(n_small, pi1, mnm1_close, mnm2)
mnm_large <- rmix(n_large, pi1, mnm1_close, mnm2)

mnm_small_one <- contaminate(mnm_small, 0.01, function() c(0, runif(1, 10, 15)))
mnm_large_one <- contaminate(mnm_large, 0.01, function() c(0, runif(1, 10, 15)))

mnm_small_five <- contaminate(mnm_small, 0.05, function() runif(2, -10, 10))
mnm_large_five <- contaminate(mnm_large, 0.05, function() runif(2, -10, 10))

mnm_small_thirty <- contaminate(mnm_small, 0.3, function() runif(2, -10, 10))
mnm_large_thirty <- contaminate(mnm_large, 0.3, function() runif(2, -10, 10))

distros_small <- list(tm_small, mcnm_small, mnm_small_one, mnm_small_five, mnm_small_thirty)
distros_small_names <- c("tm_small", "mcnm_small", "mnm_small_one", "mnm_small_five", "mnm_small_thirty")
distros_large <- list(tm_large, mcnm_large, mnm_large_one, mnm_large_five, mnm_large_thirty)
distros_large_names <- c("tm_large", "mcnm_large", "mnm_large_one", "mnm_large_five", "mnm_large_thirty")
