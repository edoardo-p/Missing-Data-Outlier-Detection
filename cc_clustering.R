library(MixtureMissing)
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

# Remove all rows with missing values
music_data$tempo[which(music_data$tempo == "?")] <- NA
music_data$tempo <- as.double(music_data$tempo)
music_data$duration_ms[which(music_data$duration_ms == -1)] <- NA
music_data <- music_data[-which(is.na(music_data$duration_ms)), ]
music_data <- music_data[-which(is.na(music_data$tempo)), ]

# Save labeled genre for training the model
label <- music_data[18]

# Remove categorical variables and the label
music_data <- music_data[-c(1, 2, 3, 10, 13, 16, 18)]

# Ampute dataset to randomly insert NAs
music_missing <- ampute(music_data, 0.01)

# Split the dataframe into train and test set (80/20)
sample <- sample(1:nrow(music_missing), nrow(music_missing) * 0.8)
train <- music_missing[sample, ]
test <- music_missing[-sample, ]


sub <- music_data[which(label == "Rock" | label == "Blues" | label == "Rap"), ]

# sub <- sub[, -2]
best_bic_list <- list()
for (G in 2:5) {
    mnm_model <- MNM(sub, G, max_iter = 20, identity_cov = TRUE)
    best_bic_list <- append(best_bic_list, mnm_model$BIC)
}












# Plot BIC
plot(as.numeric(best_bic_list), type = "b", pch = 20, col = "orange")
grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 2)

mnm_model <- MNM(music_missing, 3, max_iter = 1)

ggplot(music_missing, aes(x = `BALANCE`, y = `PURCHASE`)) +
    geom_point(aes(colour = factor(mnm_model$clusters))) +
    scale_color_manual(values = c("green", "blue")) +
    ggtitle("results")
attach(music_missing)
mnm_model$clusters
