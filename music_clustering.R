library(MixtureMissing)
library(MixGHD)
cat("\014")
rm(list = ls())


ampute <- function(data, prop) {
  missing <- sample(1:nrow(data), nrow(data) * prop)
  data$missing <- FALSE
  for (i in missing) {
    select <- sample(1:(ncol(data) - 1), 1)
    data[i, select] <- NA
    data$missing[i] <- TRUE
  }
  return(data)
}

contaminate <- function(data, eta) {
  for (i in sample(1:nrow(data), nrow(data) * eta)) {
    noisy <- runif(8, min = -1, max = 1)
    data[i, 1:8] <- noisy
  }
  return(data)
}

# Data preprocessing
music_data <- read.csv("music_genre.csv")

# Remove all rows with missing values
music_data$tempo[which(music_data$tempo == "?")] <- NA
music_data$tempo <- as.double(music_data$tempo)
music_data$duration_ms[which(music_data$duration_ms == -1)] <- NA
music_data <- music_data[-which(is.na(music_data$duration_ms)), ]
music_data <- music_data[-which(is.na(music_data$tempo)), ]

# Remove categorical variables and the label
music_data <- music_data[-c(1, 2, 3, 7, 10, 11, 13, 14, 16)]

# Ampute dataset to randomly insert NAs
music_data <- ampute(music_data, 0.15)
genres <- c("Electronic", "Classical")
sub <- music_data[which(music_data$music_genre %in% genres), ]

# K <- 4
# folds <- vector(mode = "list", length = K)

# for (k in 1:K) {
#     fold_size <- nrow(sub) / K
#     folds[[k]] <- train[((k - 1) * fold_size + 1): (k * fold_size), ]
# }

samp <- sample(1:nrow(sub), 500)
noises <- c(0, 1, 5, 10, 30)

train <- scale(sub[samp, -10:-9])
label <- sub[samp, 9]
missing <- sub[samp, 10]
n_clusts <- length(genres)

ari_values <- array(dim = c(length(noises), 3))
priors <- array(dim = c(length(noises), 2))
alphas <- array(dim = c(length(noises), 2))
etas <- array(dim = c(length(noises), 2))
means <- array(dim = c(length(noises), ncol(train), 2))

for (i in 1:length(noises)) {
  train <- contaminate(train, noises[i] / 100)
  mtm_model <- MtM(train, n_clusts, max_iter = 20, identity_cov = TRUE)
  mnm_model <- MNM(train, n_clusts, max_iter = 20, identity_cov = TRUE)
  mcnm_model <- MCNM(train, n_clusts, max_iter = 20, identity_cov = TRUE)

  ari_values[i, 1] <- ARI(mtm_model$clusters, label)
  ari_values[i, 2] <- ARI(mnm_model$clusters, label)
  ari_values[i, 3] <- ARI(mcnm_model$clusters, label)

  priors[i, ] <- mcnm_model$pi
  alphas[i, ] <- mcnm_model$alpha
  etas[i, ] <- mcnm_model$eta
  means[i, , ] <- mcnm_model$mu
  break
}




colours <- character(nrow(train))
colours[] <- "#00bfff"
colours[(mcnm_model$clusters == 1) & !missing] <- "blue"
colours[(mcnm_model$clusters == 2) & missing] <- "#ff5b5b"
colours[(mcnm_model$clusters == 2) & !missing] <- "red"

chars <- vector(mode = "numeric", length = nrow(train))
chars[] <- 16
chars[(mcnm_model$clusters == 1) & mcnm_model$outliers] <- 1
chars[(mcnm_model$clusters == 2) & !mcnm_model$outliers] <- 17
chars[(mcnm_model$clusters == 2) & mcnm_model$outliers] <- 2

plot(train[, 3], train[, 7], pch = chars, col = colours)


plot(c(0, 1, 5, 10, 30), ari_values[, 1], type = "b", pch = 16, col = "blue", xlab = "Noise", ylab = "ARI")
points(c(0, 1, 5, 10, 30), ari_values[, 2], type = "b", pch = 16, col = "red")
points(c(0, 1, 5, 10, 30), ari_values[, 3], type = "b", pch = 16, col = "green")
legend(
  x = "topright",
  legend = c("MtM", "MNM", "MCNM"),
  lty = c(1, 1, 1),
  col = c("blue", "red", "green")
)
