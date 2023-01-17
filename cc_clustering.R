library(MixtureMissing)
library(matrixcalc)
library(tidyverse)
cat("\014")
rm(list = ls())


ampute <- function(data, prop) {
    missing <- sample(1:nrow(data), nrow(data) * prop)
    for (i in missing) {
        select <- sample(1:ncol(data), 1)
        data[i, select] <- NA
    }
    return(data)
}

# Data preprocessing
music_data <- read.csv("music_genre.csv")

# Remove categorical variables and the label
index <- c(1, 2, 3, 10, 13, 16, 18)

# Save labeled genre for training the model
label <- music_data[18]
music_data <- music_data[-index]

# Substitute '?' with NA values and convert column to double
music_data[which(music_data$tempo == "?"), ] <- NA
music_data$tempo <- as.double(music_data$tempo)

# Remove empty rows
music_data <- music_data[-which(is.na(music_data$popularity)), ]

# Ampute dataset to randomly insert NAs
music_missing <- ampute(music_data, 0.01)

# Split the dataframe into train and test set (80/20)
sample <- sample(1:nrow(music_missing), nrow(music_missing) * 0.8)
train <- music_missing[sample, ]
test <- music_missing[-sample, ]

best_bic <- 0
best_g <- 0
cc_missing <- music_missing[sample(1:nrow(music_missing), 500), ]
cc_missing$duration_ms[which(cc_missing$duration_ms == -1)] <- NA
cc_missing <- cc_missing[, -2]
best_bic_list <- list()
for (G in 6:15) {
    mnm_model <- MNM(cc_missing, G, max_iter = 10, identity_cov = TRUE)
    best_bic <- mnm_model$BIC
    best_bic_list <- append(best_bic_list, best_bic)
    best_g <- G
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
