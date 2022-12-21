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

# SMALL CASE
# CASE 1
m1 = Mclust(tm_small,G = 2)
df1 = as.data.frame(tm_small)
print(plot_results(df1, m1$classification," tm_small"))

# CASE 2
m2 = Mclust(mcnm_small,G = 2)
df2 = as.data.frame(mcnm_small)
print(plot_results(df2, m2$classification, "mcnm_small"))

# CASE 3
m3 = Mclust(mnm_small_one,G=2)
df3 = as.data.frame(mnm_small_one)
print(plot_results(df3, m3$classification, "mnm_small_one"))

# CASE 4
m4 = Mclust(mnm_small_five,G=2)
df4 = as.data.frame(mnm_small_five)
print(plot_results(df4, m4$classification, "mnm_small_five"))

# CASE 5
m5 = Mclust(mnm_small_thirty,G=2)
df5 = as.data.frame(mnm_small_thirty)
print(plot_results(df5, m5$classification, "mnm_small_thirty"))


# LARGE CASE
# CASE 1
m1.1 = Mclust(tm_large,G = 2)
df1.1 = as.data.frame(tm_large)
print(plot_results(df1.1, m1.1$classification," tm_large"))

# CASE 2
m2.1 = Mclust(mcnm_small,G = 2)
df2.1 = as.data.frame(mcnm_small)
print(plot_results(df2.1, m2.1$classification, "mcnm_large"))

# CASE 3
m3.1 = Mclust(mnm_large_one,G=2)
df3.1 = as.data.frame(mnm_large_one)
print(plot_results(df3.1, m3.1$classification, "mnm_large_one"))

# CASE 4
m4.1 = Mclust(mnm_large_five,G=2)
df4.1 = as.data.frame(mnm_large_five)
print(plot_results(df4.1, m4.1$classification, "mnm_large_five"))

# CASE 5
m5.1 = Mclust(mnm_large_thirty,G=2)
df5.1 = as.data.frame(mnm_large_thirty)
print(plot_results(df5.1, m5.1$classification, "mnm_large_thirty"))



