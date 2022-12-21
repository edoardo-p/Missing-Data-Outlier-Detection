library(clusterGeneration)
library(dplyr)

mn_mix <- genRandomClust(5, 0.8, numNonNoisy = 10, numReplicate = 1, clustszind = 3, clustSizes = c(100, 150, 200, 200, 350))
dataset <- as.data.frame(mn_mix$datList)
mn_mix_standard <- dataset %>% mutate_all(~ (scale(.) %>% as.vector()))
