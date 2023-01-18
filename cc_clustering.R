library(MixtureMissing)
library(MixGHD)
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

# Remove categorical variables and the label
music_data <- music_data[-c(1, 2, 3, 10, 13, 16)]

# Ampute dataset to randomly insert NAs
genres <- c("Classical", "Anime") # , "Hip-Hop", "Jazz")
sub <- music_data[which(music_data$music_genre %in% genres), ]

# K <- 4
# folds <- vector(mode = "list", length = K)

# for (k in 1:K) {
#     fold_size <- nrow(sub) / K
#     folds[[k]] <- train[((k - 1) * fold_size + 1): (k * fold_size), ]
# }

samp <- sample(1:nrow(sub), 500)
train <- sub[samp, ]
noises <- c(1, 5, 10, 30)
ari_values <- matrix(nrow = length(noises), ncol = 3)
for (i in 1:length(noises)) {
    train <- as.data.frame(cbind(ampute(train[-12], noises[i] / 100), train[12]))

    mtm_model <- MtM(train[-12], 2, max_iter = 20, identity_cov = TRUE)
    mnm_model <- MNM(train[-12], 2, max_iter = 20, identity_cov = TRUE)
    mcnm_model <- MCNM(train[-12], 2, max_iter = 20, identity_cov = TRUE)

    ari_values[i, 1] <- ARI(mtm_model$clusters, train$music_genre)
    ari_values[i, 2] <- ARI(mnm_model$clusters, train$music_genre)
    ari_values[i, 3] <- ARI(mcnm_model$clusters, train$music_genre)

    # clusts <- character(nrow(train))
    # clusts[] <- "turquoise"
    # clusts[mcnm_model$clusters == 1] <- "purple"
    # clusts[mcnm_model$clusters == 2] <- "limegreen"
    # clusts[mcnm_model$clusters == 3] <- "blue"

    # pairs(train[-12], col = clusts)

    # TODO calc ARI
    # TODO show outliers
}

ari_values
clusts <- character(nrow(train))
clusts[] <- "turquoise"
clusts[mcnm_model$clusters == 1] <- "purple"
# clusts[mcnm_model$clusters == 2] <- "limegreen"
# clusts[mcnm_model$clusters == 3] <- "blue"

labels <- character(nrow(train))
labels[] <- "turquoise"
labels[train$music_genre == "Anime"] <- "purple"
# labels[train$music_genre == "Classical"] <- "limegreen"
# labels[train$music_genre == "Jazz"] <- "blue"

pairs(train[-12], col = clusts)

plot(train$energy, train$tempo, col = clusts)
plot(train$energy, train$tempo, col = labels)
