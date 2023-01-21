library(mclust)
library(ggplot2)
library(MixtureMissing)

source("distributions.R")

# Plot clusters, with missclassified points in redù

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


# CASE LARGE condensate

i = 1
for (x in distros_large){
  m = Mclust(x , G = 2)
  df = as.data.frame(x)
  print(plot_results(df,m$classification,distros_large_names[i]))
  i = i + 1
}


# CASE SMALL condensate

j = 1
for (y in distros_small){
  n = Mclust(y , G = 2)
  d = as.data.frame(y)
  print(plot_results(d,n$classification,distros_small_names[j]))
  j = j + 1
}



