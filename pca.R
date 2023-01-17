# Principal Components Analisys
rm(list = ls())
cat("\014")
library(psych)
library(stats)
library(devtools)
library(ggbiplot)
library(GGally)
library(ggplot2)
library(ggfortify)
df = read.csv("music_genre.csv")

# Pre-Processing 
index = c(1,2,3,10,13,16)
df = df[-index]
df = df[-which(is.na(df$popularity)),]
labels = c("Hip-Hop","Jazz","Anime", "Classical")

df = df[which(df$music_genre %in% labels),]
View(df)
df = df[-which(df$tempo == '?'),]
df = df[-which(df$duration_ms == -1),]
df$tempo = as.double(df$tempo)
str(df)
label = df[,12]


df_scaled = as.data.frame(cbind(scale(df[,1:11]),df[,12]))
View(df_scaled)
summary(df_scaled)
# Split the dataset
sample <- sample(c(TRUE, FALSE), nrow(df_scaled), replace=TRUE, prob=c(0.8,0.2))
train  <- df_scaled[sample, ]
label_train = train[,12]
test   <- df_scaled[!sample, ]
label_test = test[,12]
# DataVisualization
labels_train = train[,12]
subset = df_scaled[sample(nrow(df_scaled),000),]
train = sapply(train[,-12], function(x) as.double(x))
train = as.data.frame(train)
train = cbind(train,label_train)
attach(subset)

pairs(as.data.frame(subset[,-12]))
# Implement the pca
pca.model = prcomp(train[,-12], center = TRUE, scale. = TRUE)
summary(pca.model)
biplot(pca.model, main = "Biplot", scale = 0)
autoplot(pca.model, data = train , colour = 'label_train')#, loadings = TRUE, loadings.colour = "blue")

data_set = pca.model$x[,1:2]

