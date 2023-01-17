library(mclust)
library(ggplot2)
library(MixtureMissing)

source("distributions.R")

# Plot clusters, with missclassified points in red
plot_results <- function(df, classified, title = "Results") {
  # first_cluster <- df$V3[1]
  # if (first_cluster == 1) {
  #   classified[which(df$V3 != classified)] <- 3
  # } else {
  #   classified[which(df$V3 == classified)] <- 3
  # }
  ggplot(df, aes(x = `BALANCE`, y = `BALANCE_FREQUENCY`)) +
    geom_point(aes(colour = factor(classified))) +
    scale_color_manual(values = c("green", "blue")) +
    ggtitle(title)
}


# CASE LARGE condensate
i <- 1
for (x in distros_large) {
  m <- Mclust(x, G = 2)
  df <- as.data.frame(x)
  print(plot_results(df, m$classification, distros_large_names[i]))
  i <- i + 1
}

# CASE SMALL condensate
j <- 1
for (y in distros_small) {
  n <- Mclust(y, G = 2)
  d <- as.data.frame(y)
  print(plot_results(d, n$classification, distros_small_names[j]))
  j <- j + 1
}

i <- 1
ari_values_small <- matrix(nrow = length(distros_small), ncol = 3)
for (x in distros_small) {
  mtm_model <- MtM(x[, -3], 2)
  mnm_model <- MNM(x[, -3], 2)
  mcnm_model <- MCNM(x[, -3], 2)

  ari_values_small[i, 1] <- MixGHD::ARI(x[, 3], mtm_model$clusters)
  ari_values_small[i, 2] <- MixGHD::ARI(x[, 3], mnm_model$clusters)
  ari_values_small[i, 3] <- MixGHD::ARI(x[, 3], mcnm_model$clusters)
  i <- i + 1
}

i <- 1
ari_values_large <- matrix(nrow = length(distros_large), ncol = 3)
for (x in distros_large) {
  mtm_model <- MtM(x[, -3], 2)
  mnm_model <- MNM(x[, -3], 2)
  mcnm_model <- MCNM(x[, -3], 2)

  ari_values_large[i, 1] <- MixGHD::ARI(x[, 3], mtm_model$clusters)
  ari_values_large[i, 2] <- MixGHD::ARI(x[, 3], mnm_model$clusters)
  ari_values_large[i, 3] <- MixGHD::ARI(x[, 3], mcnm_model$clusters)
  i <- i + 1
}

dists <- rep(distros_small_names, 3)
models <- c(rep("MtM", 5), rep("MNM", 5), rep("MCNM", 5))
df <- as.data.frame(cbind(dists, models, matrix(ari_values_small, nrow = 15)))
colnames(df) <- c("dist", "model", "ARI")
ggplot(df, aes(x = dist, y = ARI, fill = model)) +
  geom_col(stat = "identity", position = position_dodge())

ggplot(data = df, aes(x = model, y = ARI, fill = dist)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal()
