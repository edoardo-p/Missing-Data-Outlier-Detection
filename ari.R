library(mclust)
library(ggplot2)
library(MixtureMissing)

source("distributions.R")

# Plot clusters, with missclassified points in red
plot_results <- function(df, classified, title = "Results") {
  first_cluster <- df$V3[1]
  if (first_cluster == 1) {
    classified[which(df$V3 != classified)] <- 3
  } else {
    classified[which(df$V3 == classified)] <- 3
  }
  ggplot(df, aes(x = `V1`, y = `V2`)) +
    geom_point(aes(colour = factor(classified))) +
    scale_color_manual(values = c("green", "blue", "red")) +
    ggtitle(title)
}

for (i in 1:length(distros_small_names)) {
  model <- Mclust(distros_small[[i]][, -3], G = 2)
  df <- as.data.frame(distrib)
  print(plot_results(df, model$classification, distros_small_names[i]))
}
