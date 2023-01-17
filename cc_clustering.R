source("distributions.R")
library(MixtureMissing)
library(matrixcalc)
library(tidyverse)
cat("\014")
rm(list = ls())

# Data preprocessing
cc <- read.csv("music_genre.csv")
# We remove the useless variable (categorical principally)
index = c(1,2,3,10,13,16,18)
# we save the genre list in order then to verify if there will be any misclassification with the mixture model
label = cc[18]
cc = cc[-index]
# In the variable "tempo" there are some characters that don't have any sense in this context, so we substitute them with NA values
cc[which(cc$tempo == '?'),] = NA
# furthermore there are some rows in the dataframe which are all full NA values, so we remove them
cc = cc[-which(is.na(cc$popularity)),]

# Convert the valeus in "tempo" variable from char -> double
cc$tempo = as.double(cc$tempo)

# We compute the ampute function to insert NA values randomly
cc_missing <- ampute(cc, 0.01)

# sapply(cc_missing, function(x) sum(is.na(x)))

# Split the dataframe into train and test set (rule 80/20)

sample <- sample(c(TRUE, FALSE), nrow(cc_missing), replace=TRUE, prob=c(0.8,0.2))
train  <- cc_missing[sample, ]
test   <- cc_missing[!sample, ]


best_bic = 0
best_G = 0
cc_missing = cc_missing[sample(1:nrow(cc_missing),500),]
cc_missing$duration_ms[which(cc_missing$duration_ms == -1)] = NA
cc_missing = cc_missing[,-2]
best_bic_list = list()
for (G in 6:15){
  mnm_model = MNM(cc_missing, G, max_iter = 20,identity_cov = TRUE)
  best_bic = mnm_model$BIC
  best_bic_list = append(best_bic_list,best_bic)
  best_G = G
}
# plot of the Behaviour of the BIC
plot(as.numeric(best_bic_list),type = "b", pch = 20,col = "orange")
grid (nx = NULL, ny = NULL, lty = 2,col = "black", lwd = 2)

mnm_model = MNM(cc_missing, 3 , max_iter = 1)


# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)
kc = kmeans(cc_missing,5)


ggplot(cc_missing, aes(x = `BALANCE`, y = `PURCHASE`)) +
    geom_point(aes(colour = factor(mnm_model$clusters))) +
    scale_color_manual(values = c("green", "blue")) +
    ggtitle("results")
attach(cc_missing)
mnm_model$clusters
df_new = cbind(BALANCE,PURCHASES,CASH_ADVANCE,CREDIT_LIMIT,PAYMENTS,PRC_FULL_PAYMENT, mnm_model$clusters)
pairs(df_new [sample(1:nrow (df_new),nrow(df_new)*0.1),], col = c("red","blue"))

