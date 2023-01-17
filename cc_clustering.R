source("distributions.R")
library(MixtureMissing)
library(matrixcalc)
library(tidyverse)
cat("\014")
rm(list = ls())

# Data preprocessing
cc <- read.csv("music_genre.csv")

# Remove categorical variables and the label
index <- c(1, 2, 3, 10, 13, 16, 18)

# Save labeled genre for training the model
label <- cc[18]
cc <- cc[-index]

# Substitute '?' with NA values and convert column to double
cc[which(cc$tempo == "?"), ] <- NA
cc$tempo <- as.double(cc$tempo)

# Remove empty rows
cc <- cc[-which(is.na(cc$popularity)), ]

# Ampute dataset to randomly insert NAs
cc_missing <- ampute(cc, 0.01)

# Split the dataframe into train and test set (80/20)
sample <- sample(c(TRUE, FALSE), nrow(cc_missing), replace = TRUE, prob = c(0.8, 0.2))
train <- cc_missing[sample, ]
test <- cc_missing[!sample, ]


best_bic <- 0
best_G <- 0
cc_missing <- cc_missing[sample(1:nrow(cc_missing), 500), ]
cc_missing$duration_ms[which(cc_missing$duration_ms == -1)] <- NA
cc_missing <- cc_missing[, -2]
best_bic_list <- list()
for (G in 6:15) {
    mnm_model <- MNM(cc_missing, G, max_iter = 20, identity_cov = TRUE)
    best_bic <- mnm_model$BIC
    best_bic_list <- append(best_bic_list, best_bic)
    best_G <- G
}

# Plot BIC
plot(as.numeric(best_bic_list), type = "b", pch = 20, col = "orange")
grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 2)

mnm_model <- MNM(cc_missing, 3, max_iter = 1)

ggplot(cc_missing, aes(x = `BALANCE`, y = `PURCHASE`)) +
    geom_point(aes(colour = factor(mnm_model$clusters))) +
    scale_color_manual(values = c("green", "blue")) +
    ggtitle("results")
attach(cc_missing)
mnm_model$clusters
df_new <- cbind(BALANCE, PURCHASES, CASH_ADVANCE, CREDIT_LIMIT, PAYMENTS, PRC_FULL_PAYMENT, mnm_model$clusters)
pairs(df_new[sample(1:nrow(df_new), nrow(df_new) * 0.1), ], col = c("red", "blue"))
